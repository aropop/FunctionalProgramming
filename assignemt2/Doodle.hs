module Doodle where
import qualified Grammer
import Person
type Slot = Grammer.SlotExpression
data Doodle = Doodle { slots :: [Slot]
                     , dname :: String
                     , people :: [String]
                     , owner :: String
                     , preferences :: [[String]]} deriving (Show)

instance Eq Doodle where
    x == y = dname x == dname y

toExpression :: Doodle -> Grammer.DoodleExpression
toExpression = slots

fromExpression :: Grammer.DoodleExpression -> String -> Person -> Doodle
fromExpression expr dn p = foldr addSlot (create dn p) expr

create :: String -> Person -> Doodle
create n p = Doodle [] n [] (name p) []

addSlot :: Slot -> Doodle -> Doodle
addSlot sl (Doodle sls dn pls own prefs) = Doodle (sl:sls) dn pls own $ [] : prefs

dEq :: String -> Doodle -> Bool
dEq s d = s == dname d 
