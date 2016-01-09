import           Control.Concurrent
import qualified Data.Time          as T
import qualified Grammer
import           Network            (PortID (..), Socket, accept, listenOn,
                                     withSocketsDo)
import qualified Parser
import           System.IO
import           Control.Concurrent.STM
import System.Random
import Person
import Doodle
import Data.List (delete, elemIndex)

-- Main function
main :: IO ()
main = withSocketsDo $ do
    -- Create TVars
    personList <- atomically $ newTVar [Person "admin" "1234" Admin] -- automatically add admin
    doodleList <- atomically $ newTVar []
    sock <- listenOn $ PortNumber 8000 -- Start server on port 8000
    putStrLn "Server started on 8000"
    handleRecursive sock personList doodleList -- Start loop

-- Different rights
data Right = AdminRight | TeacherRight | StudentRight | PersonRight deriving (Show, Enum, Eq)

-- Returns a person if that person has the right and its username and password are correct
compr :: Right -> String -> String -> Person -> Maybe Person -> Maybe Person
compr _ _ _ _ (Just p) = Just p
compr AdminRight givenU givenP person@(Person u p Admin) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr TeacherRight givenU givenP person@(Person u p Teacher) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr TeacherRight givenU givenP person@(Person u p Admin) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr StudentRight givenU givenP person@(Person u p Student) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr PersonRight givenU givenP person Nothing = if name person == givenU && password person == givenP then Just person else Nothing
-- If not match than false
compr _ _ _ _ _ = Nothing

-- Used in combination with foldr to form extract
-- Propagates a found element
comprr :: (a -> Bool) -> a -> Maybe a -> Maybe a
comprr _ _ (Just a) = Just a
comprr f a Nothing = if f a then Just a else Nothing

-- Extracts a an element from a list if its in that list
-- Returns nothing otherwise
extract :: (a -> Bool) -> [a] -> Maybe a
extract f = foldr (comprr f) Nothing

-- Authenticates a person, takes a function that returns the response and feeds
-- it the person if that person is authenticated, otherwise returns wrong login
-- error response
authenticate :: Grammer.LoginExpression -> Right -> TVar [Person] -> (Person -> IO Grammer.ResponseExpression) -> IO Grammer.ResponseExpression
authenticate (Grammer.Login u p) r ps f =
    do persons <- atomically(readTVar ps)
       let maybep = foldr (compr r u p) Nothing persons
       maybe (return Grammer.WrongLogin) f maybep

-- Executes a function that modifies a tvar
readWrite:: TVar [a] -> ([a] -> [a]) -> IO (TVar [a])
readWrite tvar f = atomically(do l <- readTVar tvar
                                 writeTVar tvar $ f l
                                 return tvar)

-- Reads a Tvar
myRead :: TVar [a] -> IO [a]
myRead tvar = atomically (readTVar tvar)

-- Modifies a single element in a Tvar list
modify :: (a -> Bool) -> TVar [a] -> (a -> a) -> IO (TVar [a])
modify identifyF tvar f = readWrite tvar $ foldr (\el build -> if identifyF el then f el : build else el : build) []

-- Modify a single person
modifyPerson :: String -> TVar [Person] -> (Person -> Person) -> IO (TVar [Person])
modifyPerson identify = modify (\(Person n _ _) -> n == identify)

-- Modify a single doodle
modifyDoodle :: String -> TVar [Doodle] -> (Doodle -> Doodle) -> IO (TVar [Doodle])
modifyDoodle identify = modify (\(Doodle _ n _ _ _) -> n == identify)

-- Dispatch over all commands, authenticate them and curry the handling function
-- for a specific command. Each handling function has to take the auth person
-- as last parameter
dispatchCommand :: Grammer.RequestExpression -> TVar [Person] -> TVar [Doodle] -> IO Grammer.ResponseExpression
dispatchCommand (Grammer.AddTeacher (Grammer.AuthCommand login command)) personList _ =
    authenticate login AdminRight personList $ addTeacher command personList
dispatchCommand (Grammer.AddStudent (Grammer.AuthCommand login command)) personList _ =
    authenticate login TeacherRight personList $ addStudent command personList
dispatchCommand (Grammer.ChangePassword (Grammer.AuthCommand login command)) personList _ =
    authenticate login PersonRight personList $ changePassword command personList
dispatchCommand (Grammer.GetDoodle dn) _ doodleList =
    getDoodle dn doodleList
dispatchCommand (Grammer.SetDoodle (Grammer.AuthCommand login dn) de) personList doodleList =
    authenticate login TeacherRight personList $ setDoodle dn doodleList de
dispatchCommand (Grammer.Subscribe (Grammer.AuthCommand login dn)) personList doodleList =
    authenticate login StudentRight personList $ subscribe dn doodleList
dispatchCommand (Grammer.Prefer (Grammer.AuthCommand login dn) slt) personList doodleList =
    authenticate login StudentRight personList $ prefer dn doodleList slt
dispatchCommand (Grammer.ExamSchedule login) personList doodleList =
    authenticate login StudentRight personList $ examSchedule doodleList


-- Handle add person, abstract function, takes a person type to create the right
-- kind of user
addPerson :: String -> TVar [Person] -> PersonType -> IO Grammer.ResponseExpression
addPerson token personList tc =
    do gen <- newStdGen
       let pass = take 4 $ randomRs ('a','z') gen
       let pnElem s = foldr (\x -> (||) (s == name x)) False
       persons <- myRead personList -- Read to know if the person already exist
       if pnElem token persons then
           return Grammer.IdTaken
       else
           do _ <- readWrite personList (\ps -> Person token pass tc : ps) -- Prevent race condition
              return $ Grammer.Ok $ Grammer.OkToken pass

-- Handle add teacher
addTeacher :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
addTeacher token personList _ = addPerson token personList Teacher

-- Handle add student
addStudent :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
addStudent token personList _ = addPerson token personList Student

-- Handle change password
changePassword :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
changePassword token personList person =
    -- Modify a single person
    do _ <- modifyPerson (name person) personList (\(Person nm _ tc) -> Person nm token tc)
       return $ Grammer.Ok Grammer.OkJust

getDoodle :: String -> TVar [Doodle] -> IO Grammer.ResponseExpression
getDoodle dName doodles = do actualDoodles <- myRead doodles -- Not bad if we use an "old" list
                             let maybed = extract (dEq dName)
                                                  actualDoodles
                             return $ maybe Grammer.NoSuchId
                                           (Grammer.Ok . Grammer.OkDoodle . toExpression)
                                           maybed

-- Abstract function that feeds a doodle into a function if the doodle exists,
-- and returns an exception if it does not
withExistingDoodle :: TVar [Doodle] -> String -> (Doodle -> IO Grammer.ResponseExpression) -> IO Grammer.ResponseExpression -> IO Grammer.ResponseExpression
withExistingDoodle doodles dName doodleExistsF exception =
    do actualDoodles <- myRead doodles
       maybe exception -- Uses lazy evaluation, exception will only be executed when needed
             doodleExistsF
             (extract (dEq dName) actualDoodles)

-- Handle set doodle
setDoodle :: String -> TVar [Doodle] -> Grammer.DoodleExpression -> Person -> IO Grammer.ResponseExpression
setDoodle dName doodles de p =
    withExistingDoodle doodles
                       dName
                       (\_ -> return Grammer.IdTaken)
                       -- Following do will be lazely evaluated
                       (do _ <- readWrite doodles (\doodlesAgain -> fromExpression de dName p : doodlesAgain)
                           return $ Grammer.Ok Grammer.OkJust)

-- Handle subscribe
subscribe :: String -> TVar [Doodle] -> Person -> IO Grammer.ResponseExpression
subscribe dName doodles p =
    do _ <- modifyDoodle dName doodles (\(Doodle slts nm pl own pr) -> Doodle slts nm (name p : pl) own pr)
       return $ Grammer.Ok Grammer.OkJust

-- Abstract function that returns a fullfilled expression if user in doodle
-- and returns an exception expression if he's not in doodle
withSubscribedUserInDoodle :: TVar [Doodle] -> String -> String -> IO Grammer.ResponseExpression -> IO Grammer.ResponseExpression -> IO Grammer.ResponseExpression
withSubscribedUserInDoodle doodles dName pName fullfillExp exception =
    withExistingDoodle
        doodles
        dName
        (\d -> maybe exception
                     (const fullfillExp)
                     (extract (== pName) $ people d))
        $ return Grammer.NoSuchId

-- Handle prefer
prefer :: String -> TVar [Doodle] -> Slot -> Person -> IO Grammer.ResponseExpression
prefer dName doodles sl p =
    withSubscribedUserInDoodle -- User should be subscribed
        doodles
        dName
        (name p)
        (do _ <- modifyDoodle -- Modify a single doodle
                    dName
                    doodles
                    (\(Doodle slts nm pl own pr) -> -- Add the preference
                        let maybet = elemIndex sl slts
                            pName = name p
                            (_, newPr) = maybe (0, pr)
                                               (\idx -> foldr (\nLst (i, build) ->
                                                   let ipp = i + 1 in
                                                       -- Check if user not already in list
                                                       -- If so replace his preference
                                                       if pName `elem` nLst
                                                       then if i == idx
                                                            then (ipp, nLst : build)
                                                            else (ipp, delete pName nLst : build)
                                                       else if i == idx
                                                            then (ipp, (pName : nLst) : build)
                                                            else (ipp, nLst : build))
                                                        (0, [])
                                                        pr)
                                               maybet
                        in
                           Doodle slts nm pl own newPr) -- return new doodle
            return $ Grammer.Ok Grammer.OkJust) -- Return ok to client
        (return Grammer.NotSubscribed)

-- Handle exam schedule
examSchedule :: TVar [Doodle] -> Person -> IO Grammer.ResponseExpression
examSchedule doodles (Person pName _ _) =
    do actualDoodles <- myRead doodles
       let relevantDoodles = filter (\d -> pName `elem` people d) actualDoodles
           getSlot d = snd $ foldr (\nl (i, p) -> if pName `elem` nl -- Checks prevent name being in 2 prefered slots
                                                  then (i+1, [slots d !! i])
                                                  else (i+1, p))
                                   (0, [])
                                   $ preferences d
            -- Slots here are represented in tuples (doodleName, slotTime)
           (filledInSlots, notFilledInDoodles) =
               foldr (\d (sls, ds) -> let sl = getSlot d
                                      in if null sl
                                         then (sls, d:ds)
                                         else ((dname d, head sl):sls, ds))
                     ([], [])
                     relevantDoodles
            -- Slots where the user did not prefer in the exam
           availableSlots = map (\d-> map (\s -> (dname d, s)) $ slots d) notFilledInDoodles
           -- Create all possible permutations and filter out the ones that are not possible
           possible sl = foldr (\el b -> not (sOverlap (snd sl) (snd el)) && b) True
           possibleList slts = foldr (\el b -> b && possible el (delete el slts)) True slts
           permutations = foldr (\cl tl -> [y:x | x <- tl, y <- cl]) [filledInSlots] availableSlots
           possiblePermutations = filter possibleList permutations
       if null possiblePermutations
          then return Grammer.NoPossibleSchedule
          else do let exams = map (uncurry Grammer.Exam) $ head possiblePermutations -- take first possible
                  return $ Grammer.Ok $ Grammer.OkSchedule $ Grammer.Schedule exams


-- Continues handling requests forever
handleRecursive :: Socket -> TVar [Person] -> TVar [Doodle]-> IO ()
handleRecursive sock pl dl = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- ($) forkIO $ handleCommand handle pl dl
    handleRecursive sock pl dl

-- Handle a single command
handleCommand :: Handle -> TVar [Person] -> TVar [Doodle] -> IO ()
handleCommand handle personList doodleList = do
    putStrLn "Handling new command:"
    message <- hGetContents handle
    putStrLn $ "Contents -> " ++ message
    let parsed =  Parser.apply Grammer.expression message
    -- If nothing is parsed, we return couldnotparse response
    response <- if null parsed
                   then return Grammer.CouldNotParse
                   else dispatchCommand ((fst.head) parsed) personList doodleList
    putStrLn $ "Response:" ++ show response
    putStrLn ""
    hPrint handle response
    hClose handle
