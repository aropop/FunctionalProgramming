module Person where

-- Person data types, represent differnt types of users
data PersonType = Admin | Teacher | Student deriving (Show)
-- Person data type
data Person = Person { name :: String
                     , password :: String
                     , personType :: PersonType} deriving (Show)
