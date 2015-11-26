{-# LANGUAGE FlexibleInstances #-}
module Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run, DoodleInstance, emptyPool) where
import qualified System.IO
import qualified Data.Time as T

type MyTime = T.UTCTime

prompt :: Read a => String -> IO b -> (a -> IO b) -> IO b
prompt s m f = do putStrLn s
                  xs <- fmap reads System.IO.getLine
                  if null xs
                     then System.IO.putStrLn "What???" >> m
                     else f $ fst $ head xs

replaceNum :: Int -> a -> [a] -> [a]
replaceNum _ _ [] = []
replaceNum 1 a (_:xs) = a : xs
replaceNum n a (x:xs) = x : replaceNum (n-1) a xs

class Doodle d where
  initialize :: String -> d t
  add :: Ord t => (t,t) -> d t -> d t
  remove :: Int -> d t -> d t
  toogle :: String -> Int -> d t -> d t

data DoodleInstance tf = Di { name :: String
                            , startTimes :: [tf]
                            , endTimes :: [tf]
                            , participants :: [[String]]
                            }
instance Doodle DoodleInstance where
    initialize n = Di n [] [] []
    add (t1, t2) (Di n ss es ns) = Di n (t1 : ss) (t2 : es) ([] : ns)
    remove num (Di n ss es ns) =  Di n (deleteFromList num ss) (deleteFromList num es) (deleteFromList num ns) where
        deleteFromList lnum ls
          | num < length ls = take (lnum - 1) ls ++ tail (snd $ splitAt (lnum - 1) ls)
          | otherwise = ls
    toogle s i (Di n ss es ns) = Di n ss es $ replaceNum i (s : (ns !! i)) ns

instance Show (DoodleInstance MyTime) where
    show di = let line = "+-------------------------------------------------+\n"
                  timesLn s e u = "| " ++ s ++ " | " ++ e ++ " | "++ u ++ " |\n"
                in
                    line
                    ++ "| " ++ name di ++ " |\n" -- TODO spacing
                    ++ line
                    ++ foldr (\(st, en, ul) s -> timesLn (show st) (show en) (show ul) ++ s) "" (zip3 (startTimes di) (endTimes di) (concat (participants di)))

class Pool p where
  freshKey :: (Ord k, Enum k) => p k (d t) -> k
  get :: Ord k => k -> p k (d t) -> Maybe (d t)
  set :: Ord k => k -> d t -> p k (d t) -> p k (d t)

data PoolInstance k d = Pi { keys :: [k]
                           , vals :: [d]}  deriving (Show)
instance Pool PoolInstance where
    freshKey (Pi [] _) =  toEnum 0
    freshKey (Pi (k:_) _) = succ k
    get _ (Pi [] _) = Nothing
    get key (Pi (k:ks) (v:vs))
        | k == key = Just v
        | otherwise = get key $ Pi ks vs
    set key val (Pi ks vs) = Pi (key:ks) (val:vs)

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

main = run emptyPool
