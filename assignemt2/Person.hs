module Person where
data PersonType = Admin | Teacher | Student deriving (Show)
data Person = Person { name :: String
                     , password :: String
                     , personType :: PersonType} deriving (Show)
