import           Control.Concurrent
import qualified Data.Time          as T
import qualified Grammer
import           Network            (PortID (..), Socket, accept, listenOn,
                                     withSocketsDo)
import qualified Parser
import           System.IO
import           Control.Concurrent.STM
import           Data.Maybe
import System.Random

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 8000
    putStrLn "Server started on 8000"
    handleRecursive sock

data Person = Student { name     :: String
                      , password :: String}
            | Teacher { name     :: String
                      , password :: String}
            | Admin { name     :: String
                    , password :: String}
                      deriving (Show)

data Right = AdminRight | TeacherRight | StudentRight | PersonRight deriving (Show, Enum, Eq)

-- TODO verber
compr :: Right -> String -> String -> Person -> Maybe Person -> Maybe Person
compr _ _ _ _ (Just p) = Just p
compr AdminRight givenU givenP person@(Admin u p) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr TeacherRight givenU givenP person@(Teacher u p) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr StudentRight givenU givenP person@(Student u p) Nothing = if u == givenU && p == givenP then Just person else Nothing
compr PersonRight givenU givenP person Nothing = if name person == givenU && password person == givenP then Just person else Nothing
-- If not match than false
compr StudentRight _ _ _ _ = Nothing
compr TeacherRight _ _ _ _ = Nothing
compr AdminRight _ _ _ _ = Nothing
compr PersonRight _ _ _ _ = Nothing

authenticate :: Grammer.LoginExpression -> Right -> TVar [Person] -> (Person -> IO Grammer.ResponseExpression) -> IO Grammer.ResponseExpression
authenticate (Grammer.Login u p) r ps f =
    do persons <- atomically(readTVar ps)
       let maybep = foldr (compr r u p) Nothing persons
       maybe (return Grammer.WrongLogin) f maybep

readWritePersons :: TVar [Person] -> ([Person] -> [Person]) -> IO (TVar [Person])
readWritePersons tvar f = atomically(do l <- readTVar tvar
                                        writeTVar tvar $ f l
                                        return tvar)

readPersons :: TVar [Person] -> IO [Person]
readPersons tvar = atomically (readTVar tvar)


dispatchCommand :: Grammer.RequestExpression -> TVar [Person] -> IO Grammer.ResponseExpression
dispatchCommand (Grammer.AddTeacher (Grammer.AuthCommand login command)) personList
    = authenticate login AdminRight personList $ addTeacher command personList
dispatchCommand (Grammer.AddStudent (Grammer.AuthCommand login command)) personList
    = authenticate login TeacherRight personList $ addStudent command personList
-- dispatchCommand (Grammer.ChangePassword ac) personList = authenticate TeacherRight [] changePassword
-- dispatchCommand (Grammer.GetDoodle ac) personList = authenticate TeacherRight [] getDoodle
-- dispatchCommand (Grammer.SetDoodle ac d) personList = authenticate TeacherRight []  $ setDoodle d
-- dispatchCommand (Grammer.Subscribe ac) personList = authenticate TeacherRight [] subscribe
-- dispatchCommand (Grammer.Prefer ac sl) personList = authenticate TeacherRight [] $ prefer sl
-- dispatchCommand (Grammer.ExamSchedule lg) personList = authenticate TeacherRight [] examSchedule

addPerson :: String -> TVar [Person] -> (String -> String -> Person) -> IO Grammer.ResponseExpression
addPerson token personList tc =
    do gen <- newStdGen
       let pass = take 4 $ randomRs ('a','z') gen
       let pnElem s = foldr (\x -> (||) (s == name x)) False
       persons <- readPersons personList
       if pnElem token persons then
           return Grammer.IdTaken
       else
           do _ <- readWritePersons personList (\ps -> tc token pass : ps) -- Prevent race condition
              return $ Grammer.Ok $ Grammer.OkToken pass


addTeacher :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
addTeacher token personList _ = addPerson token personList Teacher


addStudent :: String -> TVar [Person] -> Person -> IO Grammer.ResponseExpression
addStudent token personList _ = addPerson token personList Student

-- Continues handling requests forever
handleRecursive :: Socket -> IO ()
handleRecursive sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- ($) forkIO $ handleCommand handle
    handleRecursive sock

handleCommand :: Handle -> IO ()
handleCommand handle = do
    print "Handling new command:"
    message <- hGetContents handle
    print $ "Contents -> " ++ message
    personList <- atomically $ newTVar [Admin "admin" "1234"]
    let parsed =  Parser.apply Grammer.expression message
    print $ (fst.head) parsed
    response <- dispatchCommand ((fst.head) parsed) personList
    hPrint handle response
    hClose handle
