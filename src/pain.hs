
module Lessons.Pain () where

import qualified Data.Char as C
import qualified Data.List as L
import Data.Functor.Classes (eq1)
import Data.List (delete)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)

-- type Parser a = String -> Either String (a, String)
type Parser2 a = ExceptT String (Control.Monad.Trans.State.Strict.State String) a
parse :: Parser2 a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

type Parser a = String -> Either String (a, String)

many :: Parser2 a -> Parser2 [a]
many p = many' p []
    where
        many' p' acc = do
            input <- lift get
            case parse p' input of
                (Left _, rest') -> lift $ put rest' >> return acc
                (Right a, rest) -> do
                    lift $ put rest
                    many' p' (a:acc)

and2 :: Parser2 a -> Parser2 b -> Parser2 (a, b)
and2 p1 p2 = do
    a <- p1
    b <- p2
    return (a, b)

and2' :: (a -> b -> c) -> Parser2 a -> Parser2 b -> Parser2 c
and2' f p1 p2 = do
    a <- p1
    b <- p2
    return $ f a b

and3' :: (a -> b -> c -> d) -> Parser2 a -> Parser2 b -> Parser2 c -> Parser2 d
and3' f p1 p2 p3 = do
    a <- p1
    b <- p2
    c <- p3
    return $ f a b c

and5' :: (a -> b -> c -> d -> e -> f) -> Parser2 a -> Parser2 b -> Parser2 c -> Parser2 d -> Parser2 e -> Parser2 f
and5' f p1 p2 p3 p4 p5 = do
    a <- p1
    b <- p2
    c <- p3
    d <- p4
    e <- p5
    return $ f a b c d e

or2 :: Parser2 a -> Parser2 a -> Parser2 a
or2 p1 p2 = do
    input <- lift get
    case parse p1 input of
        (Left _, _) -> p2
        (Right _, _) -> p1

-- >>> parse (parseExact "a") "a"
-- (Right "a","")
parseExact :: String -> Parser2 String
parseExact expected = do
  input <- lift get
  let len = length expected
  if take (length expected) input == expected
       then lift $ put (drop (len) input) >> return expected --Right (expected, drop len input)
       else throwE $ "Expected " ++ expected

-- HELPER FUNCTIONS
parseChar :: Char -> Parser2 Char
parseChar c = do
  input <- lift get
  if null input
    then throwE ("Cannot find " ++ [c] ++ " in an empty input")
    else if c == head input
      then lift $ put (tail input) >> return c
      else throwE (c : " is not found in " ++ input)
-- <character> ::= [1-9] | [A-Z] | [a-z] | "_"
parseLetter :: Parser2 Char
parseLetter = do
  input <- lift get
  if null input
    then throwE "Cannot find any letter in an empty input"
    else if C.isLetter (head input) || head input == '_'
      then lift $ put (tail input) >> return (head input)
      else throwE (input ++ " does not start with a letter")
-- HELPER FUNCTIONS
parseDigit :: Parser2 Char
parseDigit = do
  input <- lift get
  if null input
    then throwE "Cannot find any digits in an empty input"
    else if C.isDigit (head input)
      then lift $ put (tail input) >> return (head input)
      else throwE (input ++ " does not start with a digit")
-- <string> ::= <character> <string> | <character>
parseString :: Parser2 String
parseString = many (or2 parseLetter parseDigit)
-- ", "
parseSeperator :: Parser2 String
parseSeperator = parseExact ", "
-- "."
parseDotSeperator :: Parser2 String
parseDotSeperator = parseExact "."
-- <integer>


-- >>> parse parsePositiveFloat "1.25 abc"
-- (Right 1.25," abc")

parsePositiveFloat :: Parser2 Float
parsePositiveFloat = do
    integerPart <- parseInteger
    input <- lift get
    case parse parseDotSeperator input of
        (Left _, _) -> return (read (show integerPart))
        (Right _, rest) -> do
            case parse parseInteger rest of
                (Left _, _) -> throwE "not an integer"
                (Right decimalPart, rest') -> lift $ put rest' >> return (read (show integerPart ++ "." ++ show decimalPart))

        

        

parseInteger :: Parser2 Integer
parseInteger = do
  input <- lift get
  if null input
    then throwE "empty input, cannot parse an integer"
    else
      let
        digits = takeWhile C.isDigit input
        rest = drop (length digits) input
      in
        if null digits
          then throwE "not an integer"
          else lift $ put rest >> return (read digits)

-- <float>
-- >>> parse parseFloat "-1 abc"
-- (Right (-1.0)," abc")
parseFloat :: Parser2 Float
parseFloat = do
    input <- lift get
    case parse (parseExact "-") input of
        (Left _, _) -> parsePositiveFloat
        (Right _, rest) -> do
            case parse parsePositiveFloat rest of
                (Left _, _) -> throwE "not a float"
                (Right numner, rest') -> lift $ put rest' >> return (-numner)

-- >>> parse (parseManySeperated (parseExact "a a") parseSeperator) "a a, a a, a a"
-- (Right ["a a","a a","a a"],"")
parseManySeperated :: Parser2 a -> Parser2 b -> Parser2 [a]
parseManySeperated p sep = do
    a <- p
    input <- lift get
    case parse sep input of
        (Left _, rest) -> lift $ put rest >> return [a]
        (Right _, rest) -> do
            lift $ put rest
            as <- parseManySeperated p sep
            return (a:as)
