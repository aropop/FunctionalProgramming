{-# LANGUAGE FlexibleInstances #-}
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO
import Control.Concurrent
import qualified Data.Time as T
import qualified Parser

type MyTime = T.UTCTime -- Abstract the time type

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 8000
    putStrLn "Server started on 8000"
    handleRecursive sock

type TokenExpression = String
data LoginExpression = Login TokenExpression TokenExpression deriving Show
data SlotExpression = Slot MyTime MyTime deriving Show
type DoodleExpression = [SlotExpression]
data AuthCommand = AuthCommand LoginExpression TokenExpression deriving Show
data RequestExpression = AddTeacher AuthCommand
                        | AddStudent AuthCommand
                        | ChangePassword AuthCommand
                        | GetDoodle AuthCommand
                        | SetDoodle AuthCommand DoodleExpression
                        | Subscribe AuthCommand
                        | Prefer AuthCommand SlotExpression
                        | ExameSchedule LoginExpression
                        deriving Show


expression :: Parser.Parser RequestExpression
expression = Parser.oneof [addstudent, addTeacher, subscribe, changePassword, getDoodle]

loginexpression :: Parser.Parser LoginExpression
loginexpression = do
    identifier <- Parser.token
    Parser.keyword "@"
    password <- Parser.token
    return $ Login identifier password

authCommand :: Parser.Parser AuthCommand
authCommand = do
    login <- loginexpression
    token <- Parser.token
    return $ AuthCommand login token

parseSimpleCommand :: String -> (AuthCommand -> RequestExpression) -> Parser.Parser RequestExpression
parseSimpleCommand st tp = do
    Parser.keyword st
    ac <- authCommand
    return $ tp ac

addstudent :: Parser.Parser RequestExpression
addstudent = parseSimpleCommand "add-student" AddStudent

addTeacher :: Parser.Parser RequestExpression
addTeacher = parseSimpleCommand "add-teacher" AddTeacher

changePassword :: Parser.Parser RequestExpression
changePassword = parseSimpleCommand "change-password" ChangePassword

getDoodle :: Parser.Parser RequestExpression
getDoodle = parseSimpleCommand "get-doodle" GetDoodle

subscribe :: Parser.Parser RequestExpression
subscribe = parseSimpleCommand "subscribe" Subscribe

time :: Parser.Parser MyTime
time = do
    year <- Parser.integer
    Parser.keyword "-"
    month <- Parser.integer
    Parser.keyword "-"
    day <- Parser.integer
    Parser.keyword "T"
    hour <- Parser.integer
    Parser.keyword ":"
    minute <- Parser.integer
    Parser.keyword "+"
    tzhour <- Parser.integer
    Parser.keyword ":"
    tzminute <- Parser.integer
    let (Just time) = T.parseTime ((show year) ++ "-" ++ (show month) ++ "-" ++ (show day))
        Nothing = T.parseTime ((show year) ++ "-" ++ (show month) ++ "-" ++ (show day))

slot :: Parser.Parser SlotExpression
slot = do
    start <- time
    Parser.keyword "/"
    end <- time
    return $ Slot start end

slots :: Parser.Parser [SlotExpression]
slots = do
    Parser.orelse (do Parser.keyword ","
                      ; slt <- slot
                      ; slts <- slots
                      ; return slt:slts)
                  (do return "")

doodle = do
    Parser.keyword "["
    sl <- slots
    Parser.keyword "]"
    return sl

setDoodle = do
    Parser.keyword "set-doodle"
    ac <- authCommand
    d <- doodle
    return $ SetDoodle ac d

-- Continues handling requests forever
handleRecursive :: Socket -> IO ()
handleRecursive sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    _ <- ($) forkIO $ handleCommand handle
    handleRecursive sock

handleCommand :: Handle -> IO ()
handleCommand handle = do
    message <- hGetContents handle
    let lst = Parser.apply expression message
    hPrint handle $ (fst . head) $ Parser.apply expression message
    hClose handle
