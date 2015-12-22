module Parser (Parser, apply, sat, char, string, some, many, orelse, oneof, blank, keyword, token, integer, float, allof) where

import Control.Monad
import Data.Char

newtype Parser a = P (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (P f) s = f s

instance Functor Parser where

instance Applicative Parser where

instance Monad Parser where
  -- Let the input intact
  return x = P (\s -> [(x,s)])
  -- Combine two parsers
  p >>= f = P (\s1 -> concat (map (\(x,s2) -> apply (f x) s2) (apply p s1)))

------------------------
-- Parser combinators --
------------------------

-- Combine all the possible reading
plus :: Parser a -> Parser a -> Parser a
plus p1 p2 = P (\s -> (apply p1 s) ++ (apply p2 s))

-- Indem plus but on a list of parser
allof :: [Parser a] -> Parser a
allof ps = P (\s -> concat $ map (\p -> apply p s) ps)

-- Try the first parser, in case of failure use the second parser
orelse :: Parser a -> Parser a -> Parser a
orelse p1 p2 = P (\s -> if null (apply p1 s) then apply p2 s else apply p1 s)

-- Idem orelse but on a list of parser
oneof :: [Parser a] -> Parser a
oneof [] = zero
oneof (p:ps) = p `orelse` (oneof ps)

-- Use the same parser again and again until failure
many :: Parser a -> Parser [a]
many p = orelse
           (do x <- p
               xs <- many p
               return $ x:xs)
           (return [])

-- Idem many, but require at least one parsing
some :: Parser a -> Parser [a]
some p = do x <- p
            xs <- many p
            return $ x:xs

--------------------
-- Simple Parsers --
--------------------

-- The failure parser
zero :: Parser a
zero = P (\s -> [])

-- Eat a character
item :: Parser Char
item = P f where f [] = []
                 f (c:cs) = [(c,cs)]

-- Eat a character only if it satisfies
sat :: (Char -> Bool) -> Parser Char
sat f = item >>= (\c -> if f c then return c else zero)

-- Try to eat the given character
char :: Char -> Parser ()
char c = sat (==c) >> return ()

-- Try to eat the given string
string :: String -> Parser ()
string (c:cs) = char c >> string cs
string [] = return ()

-------------
-- Helpers --
-------------

-- Eat blanks characters
blank :: Parser ()
blank = many (sat (\c -> c `elem` [' ', '\t', '\n'])) >> return ()

-- Eat blank then call string parser
keyword :: String -> Parser ()
keyword s = blank >> string s

-- Eat blank then parse alphabetical characters
token :: Parser String
token = blank >> (some $ sat isAlpha)

-- Parse an integer
integer :: Parser Int
integer =
  do blank
     sign <- (string "-" >> return "-") `orelse` return ""
     blank
     digits <- some (sat isDigit)
     return $ read (sign ++ digits)

-- Parse a float
float :: Parser Float
float =
  do blank
     sign <- (string "-" >> return "-") `orelse` return ""
     blank
     ints <- some (sat isDigit)
     keyword "."
     decs <- some (sat isDigit)
     return $ read $ sign ++ ints ++ "." ++ decs

