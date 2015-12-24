module Grammer  where

import qualified Parser
import qualified Data.Time as T

type TokenExpression = String

data LoginExpression = Login TokenExpression TokenExpression deriving Show

data SlotExpression = Slot {start :: T.UTCTime, end :: T.UTCTime} deriving Show

type DoodleExpression = [SlotExpression]

data AuthCommand = AuthCommand LoginExpression TokenExpression deriving Show

data RequestExpression = AddTeacher AuthCommand
                        | AddStudent AuthCommand
                        | ChangePassword AuthCommand
                        | GetDoodle TokenExpression -- AuthCommand
                        | SetDoodle AuthCommand DoodleExpression
                        | Subscribe AuthCommand
                        | Prefer AuthCommand SlotExpression
                        | ExamSchedule LoginExpression
                        deriving Show

data ExamExpression = Exam TokenExpression SlotExpression
instance Show ExamExpression where
    show (Exam t s) = show t ++ ":" ++ show s

type Exams = [ExamExpression]

data ScheduleExpression = Schedule Exams
instance Show ScheduleExpression where
    show (Schedule lst) = "{" ++ tail (concatMap (("," ++ ) . show) lst) ++ "}"

data OkExpression = OkToken TokenExpression
                    | OkDoodle DoodleExpression
                    | OkJust
                    | OkSchedule ScheduleExpression
instance Show OkExpression where
    show OkJust = "ok"
    show (OkToken t) = "ok " ++ show t
    show (OkDoodle t) = "ok " ++ show t
    show (OkSchedule t) = "ok " ++ show t

data ResponseExpression = WrongLogin
                          | IdTaken
                          | NoSuchId
                          | NoSuchSlot
                          | NotSubscribed
                          | NoPossibleSchedule
                          | CouldNotParse
                          | Ok OkExpression
instance Show ResponseExpression where
    show WrongLogin = "wrong-login"
    show CouldNotParse = "could-not-parse-request"
    show IdTaken = "id-taken"
    show NoSuchId = "no-such-id"
    show NoSuchSlot = "no-such-slot"
    show NotSubscribed = "not-subscribed"
    show NoPossibleSchedule = "no-possible-exam-schedule"
    show (Ok e) = show e


expression :: Parser.Parser RequestExpression
expression = Parser.oneof [addstudent, addTeacher, subscribe, changePassword, getDoodle, setDoodle]

loginexpression :: Parser.Parser LoginExpression
loginexpression = do
    identifier <- Parser.token
    Parser.keyword "@"
    password <- Parser.text
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

--getDoodle :: Parser.Parser RequestExpression
--getDoodle = parseSimpleCommand "get-doodle" GetDoodle

getDoodle :: Parser.Parser RequestExpression
getDoodle = do
    Parser.keyword "get-doodle"
    identifier <- Parser.token
    return $ GetDoodle identifier

subscribe :: Parser.Parser RequestExpression
subscribe = parseSimpleCommand "subscribe" Subscribe

time :: Parser.Parser T.UTCTime
time = do
    year <- Parser.exactInteger
    Parser.keyword "-"
    month <- Parser.exactInteger
    Parser.keyword "-"
    day <- Parser.exactInteger
    Parser.keyword "T"
    hour <- Parser.exactInteger
    Parser.keyword ":"
    minute <- Parser.exactInteger
    Parser.keyword "+"
    tzhour <- Parser.exactInteger
    Parser.keyword ":"
    tzminute <- Parser.exactInteger
    -- See https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Format.html#t:ParseTime
    return $ read $ year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":00 " ++ "+" ++ tzhour ++ tzminute


slot :: Parser.Parser SlotExpression
slot = do
    st <- time
    Parser.keyword "/"
    en <- time
    return $ Slot st en

slots :: Parser.Parser [SlotExpression]
slots =  do slt <- slot
            Parser.orelse (do Parser.keyword ","
                              slts <- slots
                              return $ slt:slts)
                           (return [slt])

doodle :: Parser.Parser DoodleExpression
doodle = do
    Parser.keyword "["
    sl <- slots
    Parser.keyword "]"
    return sl

setDoodle :: Parser.Parser RequestExpression
setDoodle = do
    Parser.keyword "set-doodle"
    ac <- authCommand
    d <- doodle
    return $ SetDoodle ac d
