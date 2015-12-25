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

main :: IO ()
main = withSocketsDo $ do
    personList <- atomically $ newTVar [Person "admin" "1234" Admin]
    doodleList <- atomically $ newTVar []
    sock <- listenOn $ PortNumber 8000
    putStrLn "Server started on 8000"
    handleRecursive sock personList doodleList

data Right = AdminRight | TeacherRight | StudentRight | PersonRight deriving (Show, Enum, Eq)

-- TODO verber
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

authenticate :: Grammer.LoginExpression -> Right -> TVar [Person] -> (Person -> IO Grammer.ResponseExpression) -> IO Grammer.ResponseExpression
authenticate (Grammer.Login u p) r ps f =
    do persons <- atomically(readTVar ps)
       let maybep = foldr (compr r u p) Nothing persons
       maybe (return Grammer.WrongLogin) f maybep

readWrite:: TVar [a] -> ([a] -> [a]) -> IO (TVar [a])
readWrite tvar f = atomically(do l <- readTVar tvar
                                 writeTVar tvar $ f l
                                 return tvar)

myRead :: TVar [a] -> IO [a]
myRead tvar = atomically (readTVar tvar)

modify :: (a -> Bool) -> TVar [a] -> (a -> a) -> IO (TVar [a])
modify identifyF tvar f = readWrite tvar $ foldr (\el build -> if identifyF el then f el : build else el : build) []

modifyPerson :: String -> TVar [Person] -> (Person -> Person) -> IO (TVar [Person])
modifyPerson identify = modify (\(Person n _ _) -> n == identify)

modifyDoodle :: String -> TVar [Doodle] -> (Doodle -> Doodle) -> IO (TVar [Doodle])
modifyDoodle identify = modify (\(Doodle _ n _ _ _) -> n == identify)

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
-- dispatchCommand (Grammer.ExamSchedule lg) personList = authenticate TeacherRight [] examSchedule

addPerson :: String -> TVar [Person] -> PersonType -> IO Grammer.ResponseExpression
addPerson token personList tc =
    do gen <- newStdGen
       let pass = take 4 $ randomRs ('a','z') gen
       let pnElem s = foldr (\x -> (||) (s == name x)) False
       persons <- myRead personList
       if pnElem token persons then
           return Grammer.IdTaken
       else
           do _ <- readWrite personList (\ps -> Person token pass tc : ps) -- Prevent race condition
              return $ Grammer.Ok $ Grammer.OkToken pass


addTeacher :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
addTeacher token personList _ = addPerson token personList Teacher

addStudent :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
addStudent token personList _ = addPerson token personList Student

changePassword :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
changePassword token personList person =
    do _ <- modifyPerson (name person) personList (\(Person nm _ tc) -> Person nm token tc)
       return $ Grammer.Ok Grammer.OkJust

getDoodle :: String -> TVar [Doodle] -> IO Grammer.ResponseExpression
getDoodle dName doodles = do actualDoodles <- myRead doodles -- Not bad if we use an "old" list
                             let maybed = extract (dEq dName)
                                                  actualDoodles
                             return $ maybe Grammer.NoSuchId
                                           (Grammer.Ok . Grammer.OkDoodle . toExpression)
                                           maybed


withExistingDoodle :: TVar [Doodle] -> String -> (Doodle -> IO Grammer.ResponseExpression) -> IO Grammer.ResponseExpression -> IO Grammer.ResponseExpression
withExistingDoodle doodles dName doodleExistsF exception =
    do actualDoodles <- myRead doodles
       maybe exception -- Uses lazy evaluation, exception will only be executed when needed
             doodleExistsF
             (extract (dEq dName) actualDoodles)

setDoodle :: String -> TVar [Doodle] -> Grammer.DoodleExpression -> Person -> IO Grammer.ResponseExpression
setDoodle dName doodles de p =
    withExistingDoodle doodles
                       dName
                       (\_ -> return Grammer.IdTaken)
                       (do _ <- readWrite doodles (\doodlesAgain -> fromExpression de dName p : doodlesAgain)
                           return $ Grammer.Ok Grammer.OkJust)

subscribe :: String -> TVar [Doodle] -> Person -> IO Grammer.ResponseExpression
subscribe dName doodles p =
    do _ <- modifyDoodle dName doodles (\(Doodle slts nm pl own pr) -> Doodle slts nm (name p : pl) own pr)
       return $ Grammer.Ok Grammer.OkJust

withSubscribedUserInDoodle :: TVar [Doodle] -> String -> String -> IO Grammer.ResponseExpression -> IO Grammer.ResponseExpression -> IO Grammer.ResponseExpression
withSubscribedUserInDoodle doodles dName pName fullfillExp exception =
    withExistingDoodle
        doodles
        dName
        (\d -> maybe exception
                     (const fullfillExp)
                     (extract (== pName) $ people d))
        $ return Grammer.NoSuchId


prefer :: String -> TVar [Doodle] -> Slot -> Person -> IO Grammer.ResponseExpression
prefer dName doodles sl p =
    withSubscribedUserInDoodle
        doodles
        dName
        (name p)
        (do _ <- modifyDoodle
                    dName
                    doodles
                    (\(Doodle slts nm pl own pr) ->
                        let maybet = elemIndex sl slts
                            pName = name p
                            (_, newPr) = maybe (0, pr)
                                               (\idx -> foldr (\nLst (i, build) ->
                                                   let ipp = i + 1 in
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
                           Doodle slts nm pl own newPr)
            return $ Grammer.Ok Grammer.OkJust)
        (return Grammer.NotSubscribed)

-- Continues handling requests forever
handleRecursive :: Socket -> TVar [Person] -> TVar [Doodle]-> IO ()
handleRecursive sock pl dl = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- ($) forkIO $ handleCommand handle pl dl
    handleRecursive sock pl dl

handleCommand :: Handle -> TVar [Person] -> TVar [Doodle] -> IO ()
handleCommand handle personList doodleList = do
    print "Handling new command:"
    message <- hGetContents handle
    print $ "Contents -> " ++ message
    let parsed =  Parser.apply Grammer.expression message
    -- If nothing is parsed, we return couldnotparse response
    response <- if null parsed
                   then return Grammer.CouldNotParse
                   else dispatchCommand ((fst.head) parsed) personList doodleList
    print $ "Response:" ++ show response
    hPrint handle response
    hClose handle
