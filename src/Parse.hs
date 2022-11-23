{-# LANGUAGE LambdaCase #-}

-- Two types of Methods: interface method and class method.
-- look for first curly brace or semicolon

module Parse(Parse, doParse, get, eof, filter, 
                          parse, parseFromFile, ParseError,
                          satisfy, alpha, digit, upper, lower, space,
                          char, string, int,
                          chainl1, chainl, choice,
                          between, sepBy1, sepBy) where

import Prelude hiding (filter)

import Control.Applicative (Alternative(..))
import Data.Char
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Control.Monad (guard)
import Data.Foldable (asum)


-- definition of the parser type
newtype Parse a = P { doParse :: String -> Maybe (a, String) }

instance Functor Parse where
  fmap :: (a -> b) -> Parse a -> Parse b
  fmap f p = P $ \s -> do (c, cs) <- doParse p s
                          return (f c, cs)
                          
instance Applicative Parse where
  pure :: a -> Parse a 
  pure x    = P $ \s -> Just (x,s)

  (<*>) :: Parse (a -> b) -> Parse a -> Parse b
  p1 <*> p2 = P $ \ s -> do (f, s') <- doParse p1 s
                            (x,s'') <- doParse p2 s'
                            return (f x, s'')
                            
instance Alternative Parse where
  
  empty :: Parse a
  empty = P $ const Nothing
  
  (<|>) :: Parse a -> Parse a -> Parse a
  p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s


-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing  y = y


-- | Return the next character from the input
get :: Parse Char
get = P $  \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- | This parser *only* succeeds at the end of the input.
eof :: Parse ()
eof = P $ \case
  [] -> Just ((), [])
  _ : _ -> Nothing

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parse a -> Parse a
filter f p = P $ \s ->  do 
                         (c , cs) <- doParse p s
                         guard (f c)
                         return (c , cs)



---------------------------------------------------------------
---------------------------------------------------------------
---------------------------------------------------------------

type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors, but we
-- give it a type similar to other Parsing libraries.
parse :: Parse a -> String -> Either ParseError a
parse parser str = case doParse parser str of
    Nothing    -> Left  "No parses"
    Just (a,_) -> Right a


-- | parseFromFile p filePath runs a string parser p on the input
-- read from filePath using readFile. Returns either a
-- ParseError (Left) or a value of type a (Right).
parseFromFile :: Parse a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do
  IO.catchIOError
    (do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parse parser str)
    (\e ->
        pure $ Left $ "Error:" ++ show e)
    

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parse Char
satisfy p = filter p get

-- | Parses for specific sorts of characters
alpha, digit, upper, lower, space :: Parse Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parse Char
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parse String
string = foldr (\c p -> (:) <$> char c <*> p) (pure "")

-- | succeed only if the input is a (positive or negative) integer
int :: Parse Int
int = read <$> ((++) <$> string "-" <*> some digit <|> some digit)


-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parses` lecture for explanation of this operator.
chainl1 :: Parse a -> Parse (a -> a -> a) -> Parse a
p `chainl1` pop = foldl comb <$> p <*> rest where
   comb x (op,y) = x `op` y
   rest = many ((,) <$> pop <*> p)

-- | @chainl p pop x@ parses zero or more occurrences of @p@, separated by @pop@.
-- If there are no occurrences of @p@, then @x@ is returned.
chainl :: Parse b -> Parse (b -> b -> b) -> b -> Parse b
chainl p pop x = chainl1 p pop <|> pure x

  
-- | Combine all parsers in the list (sequentially)
choice :: [Parse a] -> Parse a
choice = asum -- equivalent to: foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parse open -> Parse a -> Parse close -> Parse a
between open p close = open *> p <* close


-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parse a -> Parse sep -> Parse [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parse a -> Parse sep -> Parse [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

---------------------------------------------

