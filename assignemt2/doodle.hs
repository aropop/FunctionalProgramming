{-# LANGUAGE FlexibleInstances #-}
module Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run, DoodleInstance, emptyPool) where
import qualified Data.Time as T
import qualified System.IO


type MyTime = T.UTCTime -- Abstract the time type

prompt :: Read a => String -> IO b -> (a -> IO b) -> IO b
prompt s m f = do putStrLn s
                  xs <- fmap reads System.IO.getLine
                  if null xs
                     then System.IO.putStrLn "What???" >> m
                     else f $ fst $ head xs

-- Function to replace an element in a list
replaceNum :: Int -> a -> [a] -> [a]
replaceNum _ _ [] = []
replaceNum 0 a (_:xs) = a : xs
replaceNum n a (x:xs) = x : replaceNum (n-1) a xs

class Doodle d where
  initialize :: String -> d t
  add :: Ord t => (t,t) -> d t -> d t
  remove :: Int -> d t -> d t
  toogle :: String -> Int -> d t -> d t

-- Data type that will instanciate the doodle class
data DoodleInstance tf = Di { name         :: String
                            , startTimes   :: [tf]
                            , endTimes     :: [tf]
                            -- Each entry in the surrounding list corresponds to a time slot
                            -- At each timeslot there is a list of participants
                            , participants :: [[String]]
                            }

instance Doodle DoodleInstance where
    -- Create empty instance
    initialize n = Di n [] [] []
    -- Add tuple to instance
    add (t1, t2) orig@(Di n ss es ns) = let test [] = True -- test whether this tuple is valid and does not overlap
                                            test ((st, en):ts) = not ((st <= t1) && (en > t1) || (st < t2) && (en >= t2)) && test ts
                                            -- Put the tuple in the right chronologic place
                                            putInPlace [] _ = ([t1], [t2]) -- Last place
                                            putInPlace (st:sts) (en:ens)
                                                | st < t1 = (st : fst rec, en : snd rec) -- Go deeper
                                                | otherwise = (t1:st:sts, t2:en:ens) -- stop
                                                where rec = putInPlace sts ens -- Recursive value
                                        in
                                            if (t1 < t2) && test (zip ss es)
                                                then let placed = putInPlace ss es in uncurry (Di n) placed ([] : ns)
                                                else orig
    -- Remove a slot = removing elements from the list
    remove num (Di n ss es ns) =  Di n (deleteFromList num ss) (deleteFromList num es) (deleteFromList num ns)
        where deleteFromList lnum ls
                  | num < length ls = take (lnum - 1) ls ++ tail (snd $ splitAt (lnum - 1) ls)
                  | otherwise = ls
    -- Add person to the list at the right index in the surrounding list
    toogle s i orig@(Di n ss es ns)
           | i > (length ns -1) || i < 0 = orig
           | otherwise = let newlist = if s `elem` sublist
                                           then sublist
                                           else s : sublist
                                       where sublist = (ns !!i)
                            in Di n ss es $ replaceNum i newlist ns

instance Show (DoodleInstance MyTime) where
    -- Show the table
    show di = let lineExtraLength = foldr (\lst cnt -> foldr (\str cnt2 -> length str + cnt2 + 2) 0 lst `max` cnt) 0 $ participants di
                  line = "+---------------------------------------------------"
                     ++ replicate (if lineExtraLength /= 0 then lineExtraLength + 3 else 0) '-' -- + 4 to count in table delimeter and spacing
                     ++ "+\n"
                  fill x l = x ++ replicate (l - length x - 1)  ' '
                  timesLn s e p =
                      "| " ++ s ++ " | "
                      ++ e ++
                      (if not $ null p then " | " ++ concat p else "")
                      ++  " |\n" ++ line
                  zipped = zip3 (startTimes di) (endTimes di) $ participants di
                in
                    line
                        ++ "| " ++ fill (name di) (length line - 4) ++ " |\n"
                        ++ line
                        ++ foldr (\(st, en, lst) s -> timesLn (show st) (show en) lst ++ s) "" zipped

class Pool p where
  freshKey :: (Ord k, Enum k) => p k (d t) -> k
  get :: Ord k => k -> p k (d t) -> Maybe (d t)
  set :: Ord k => k -> d t -> p k (d t) -> p k (d t)

-- Data type that will instanciate Pool
data PoolInstance k d = Pi { keys :: [k]
                           , vals :: [d]}  deriving (Show)

instance Pool PoolInstance where
    freshKey (Pi [] _) =  toEnum 0 -- Take null as start and take succesor each time
    freshKey (Pi (k:_) _) = succ k
    get _ (Pi [] _) = Nothing
    get key (Pi (k:ks) (v:vs)) -- Get element from list
        | k == key = Just v
        | otherwise = get key $ Pi ks vs
    set key val (Pi ks vs) = Pi (key:ks) (val:vs) -- Just add elements to the lists

run :: (Read t, Doodle d, Show k, Ord k, Enum k, Read k, Pool p, Show (d t), Ord t) => p k (d t) -> IO ()
run p = prompt "Create a new doodle or participate to an existing one?" (return p) (turn p) >>= run

turn :: (Read t, Doodle d, Ord k, Show k, Enum k, Read k, Pool p, Show (d t), Ord t) => p k (d t) -> Either String k -> IO (p k (d t))
turn p (Left s)  = do d <- populate $ initialize s
                      let k = freshKey p
                      putStrLn $ "Doodle ID: " ++ show k
                      return $ set k d p
turn p (Right k) = maybe (System.IO.putStrLn "Unknown doodle" >> return p)
                         (\d1 -> prompt "What is your name?"
                                        (turn p (Right k))
                                        (\s -> fmap (\d -> set k d p) (participate s d1)))
                         (get k p)

populate :: (Read t, Doodle d, Show (d t), Ord t) => d t -> IO (d t)
populate d = print d >> prompt "Add/Remove a slot?" (populate d) f
  where f Nothing                = return d
        f (Just (Left i))        = populate $ remove i d
        f (Just (Right (t1,t2))) = populate $ add (t1,t2) d

participate :: (Doodle d, Show (d t)) => String -> d t -> IO (d t)
participate n d = print d >> prompt "Toogle a slot?" (participate n d) f
  where f Nothing  = putStrLn "Thanks for participating!" >> return d
        f (Just i) = participate n (toogle n i d)

emptyPool :: PoolInstance Int (DoodleInstance MyTime)
emptyPool = Pi [] []

-- Run main to run the project
main = run emptyPool

-- Use one of the below commands to
-- Just(Right(2015-01-01 04:00:00, 2015-01-01 06:00:00))
-- Just(Right(2015-01-01 05:00:00, 2015-01-01 07:00:00))
-- Just(Right(2015-01-01 06:00:01, 2015-01-01 07:00:00))
-- Just(Right(2015-01-02 04:00:00, 2015-01-02 06:00:00))
-- Just(Right(2015-01-02 06:00:01, 2015-01-02 07:00:00))
