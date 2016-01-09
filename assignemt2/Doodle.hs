module Doodle where
import qualified Grammer
import Person

-- Slot is a slot expression from grammer
type Slot = Grammer.SlotExpression

-- Doodle data type
data Doodle = Doodle { slots :: [Slot]
                     , dname :: String
                     , people :: [String]
                     , owner :: String
                     , preferences :: [[String]]}
-- Implement show to output doodle to client
instance Show Doodle where
    show (Doodle slts _ _ _ _) = show slts

-- Implement Eq to identify a doodle
instance Eq Doodle where
    x == y = dname x == dname y

-- transform to expression (= returning all the slots)
toExpression :: Doodle -> Grammer.DoodleExpression
toExpression = slots

-- transofrm grammer expression to data type
fromExpression :: Grammer.DoodleExpression -> String -> Person -> Doodle
fromExpression expr dn p = foldr addSlot (create dn p) expr

-- Create a new doodle
create :: String -> Person -> Doodle
create n p = Doodle [] n [] (name p) []

-- Add a slot to a doodle (add the slot and an empty list of preferences)
addSlot :: Slot -> Doodle -> Doodle
addSlot sl (Doodle sls dn pls own prefs) = Doodle (sl:sls) dn pls own $ [] : prefs

-- Checks if a ddoodle name is equal to the doodle
dEq :: String -> Doodle -> Bool
dEq s d = s == dname d

-- Checks if 2 slots overlap
sOverlap :: Slot -> Slot -> Bool
sOverlap s1 s2 = (st1 <= st2) && (en1 > st2) || (st1 < en2) && (en1 >= en2)
                 where st1 = Grammer.start s1
                       st2 = Grammer.start s2
                       en1 = Grammer.end s1
                       en2 = Grammer.end s2
