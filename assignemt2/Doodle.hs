module Doodle where
import qualified Grammer
import Person
type Slot = Grammer.SlotExpression
data Doodle = Doodle { slots :: [Slot]
                     , dname :: String
                     , people :: [String]
                     , owner :: String
                     , preferences :: [[String]]}
instance Show Doodle where
    show (Doodle slts _ _ _ _) = show slts

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

sOverlap :: Slot -> Slot -> Bool
sOverlap s1 s2 = (st1 <= st2) && (en1 > st2) || (st1 < en2) && (en1 >= en2)
                 where st1 = Grammer.start s1
                       st2 = Grammer.start s2
                       en1 = Grammer.end s1
                       en2 = Grammer.end s2
