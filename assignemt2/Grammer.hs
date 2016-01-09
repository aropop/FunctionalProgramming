module Grammer  where

import qualified Parser
import qualified Data.Time as T

-- The grammer as defined in the assignment
-- Alias token as string
type TokenExpression = String

-- Login expression (name and password)
data LoginExpression = Login TokenExpression TokenExpression deriving Show

-- Slot expression, start time and end time
data SlotExpression = Slot {start :: T.UTCTime, end :: T.UTCTime}
instance Show SlotExpression where -- show such that the output can be copy pasted as input
    show (Slot st en) = let format = T.formatTime T.defaultTimeLocale "%FT%R%z" in
                            format st ++ "/" ++ format en
instance Eq SlotExpression where
    x == y = start x == start y && end x == end y

-- Doodle is a list of slots (for the grammer)
type DoodleExpression = [SlotExpression]

-- Authcommand is a auxiliary grammer element
-- It consists of a login expression and a toke expression
-- Abstracts several request such as addteacher, and changepassword
data AuthCommand = AuthCommand LoginExpression TokenExpression deriving Show

-- All possible requests
data RequestExpression = AddTeacher AuthCommand
                        | AddStudent AuthCommand
                        | ChangePassword AuthCommand
                        | GetDoodle TokenExpression -- AuthCommand is not necesary here
                        | SetDoodle AuthCommand DoodleExpression
                        | Subscribe AuthCommand
                        | Prefer AuthCommand SlotExpression
                        | ExamSchedule LoginExpression
                        deriving Show

-- Exam as a name and a slot
data ExamExpression = Exam TokenExpression SlotExpression
instance Show ExamExpression where
    show (Exam t s) = show t ++ ":" ++ show s

-- List of exams
type Exams = [ExamExpression]

-- Schedule is a list of slots
data ScheduleExpression = Schedule Exams
instance Show ScheduleExpression where
    show (Schedule lst) = "{" ++ tail (concatMap (("," ++ ) . show) lst) ++ "}"

-- Response data types

-- Ok responses
data OkExpression = OkToken TokenExpression -- ok and a toke
                    | OkDoodle DoodleExpression -- ok and a doodle
                    | OkJust -- Just ok
                    | OkSchedule ScheduleExpression -- Ok and a schedule
instance Show OkExpression where
    show OkJust = "ok"
    show (OkToken t) = "ok " ++ show t
    show (OkDoodle t) = "ok " ++ show t
    show (OkSchedule t) = "ok " ++ show t

-- All different responses
data ResponseExpression = WrongLogin
                          | IdTaken
                          | NoSuchId
                          | NoSuchSlot
                          | NotSubscribed
                          | NoPossibleSchedule
                          | CouldNotParse
                          | Ok OkExpression -- Wraps the ok expressions
instance Show ResponseExpression where
    show WrongLogin = "wrong-login"
    show CouldNotParse = "could-not-parse-request"
    show IdTaken = "id-taken"
    show NoSuchId = "no-such-id"
    show NoSuchSlot = "no-such-slot"
    show NotSubscribed = "not-subscribed"
    show NoPossibleSchedule = "no-possible-exam-schedule"
    show (Ok e) = show e


-- Parse all gramar elements

-- Parse input expression is one of the commands
expression :: Parser.Parser RequestExpression
expression = Parser.oneof [addstudent, addTeacher, subscribe, changePassword, getDoodle, setDoodle, prefer, examSchedule]

-- Parse a login expression
loginexpression :: Parser.Parser LoginExpression
loginexpression = do
    identifier <- Parser.token
    Parser.keyword "@"
    password <- Parser.text
    return $ Login identifier password

-- Parse a authcommand
authCommand :: Parser.Parser AuthCommand
authCommand = do
    login <- loginexpression
    token <- Parser.token
    return $ AuthCommand login token

-- Simple command is a abstraction, takes the authcommand to request expression
-- data constructor as a parameter
parseSimpleCommand :: String -> (AuthCommand -> RequestExpression) -> Parser.Parser RequestExpression
parseSimpleCommand st tp = do
    Parser.keyword st
    ac <- authCommand
    return $ tp ac

-- Parses something after the auth command, similar to parseSimpleCommand
parseComplexCommand :: String -> Parser.Parser a -> (AuthCommand -> a -> RequestExpression) -> Parser.Parser RequestExpression
parseComplexCommand st pr tp = do
    Parser.keyword st
    ac <- authCommand
    oth <- pr
    return $ tp ac oth

-- Parse simple commands
addstudent :: Parser.Parser RequestExpression
addstudent = parseSimpleCommand "add-student" AddStudent

addTeacher :: Parser.Parser RequestExpression
addTeacher = parseSimpleCommand "add-teacher" AddTeacher

changePassword :: Parser.Parser RequestExpression
changePassword = parseSimpleCommand "change-password" ChangePassword

subscribe :: Parser.Parser RequestExpression
subscribe = parseSimpleCommand "subscribe" Subscribe

-- Different, because does not require a login
getDoodle :: Parser.Parser RequestExpression
getDoodle = do
    Parser.keyword "get-doodle"
    identifier <- Parser.token
    return $ GetDoodle identifier

-- Parse a time
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
    tzhour <- Parser.exactIntegerLength 2 -- Allow for both +01:00 and +0100 notations (as used in show)
    Parser.orelse (Parser.keyword ":") (return ())
    tzminute <- Parser.exactInteger
    -- See https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Format.html#t:ParseTime
    return $ read $ year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":00 " ++ "+" ++ tzhour ++ tzminute

-- Parse asingle slot
slot :: Parser.Parser SlotExpression
slot = do
    st <- time
    Parser.keyword "/"
    en <- time
    return $ Slot st en

-- Parse a list of slots, at least one
slots :: Parser.Parser [SlotExpression]
slots =  do slt <- slot
            Parser.orelse (do Parser.keyword ","
                              slts <- slots
                              return $ slt:slts)
                           (return [slt])

-- Parse a doodle
doodle :: Parser.Parser DoodleExpression
doodle = do
    Parser.keyword "["
    sl <- slots
    Parser.keyword "]"
    return sl

-- Set doodle command
setDoodle :: Parser.Parser RequestExpression
setDoodle = parseComplexCommand "set-doodle" doodle SetDoodle

prefer :: Parser.Parser RequestExpression
prefer = parseComplexCommand "prefer" slot Prefer

-- Parse a exam schedule request
examSchedule :: Parser.Parser RequestExpression
examSchedule = do
    Parser.keyword "exam-schedule"
    l <- loginexpression
    return $ ExamSchedule l
