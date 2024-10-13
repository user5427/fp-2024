{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query
  = CreateStop Int String Float Float Int Int
  | CreateRoute Int String [Int]
  | CreatePath Int String Float Int Int
  | CreateTrip Int String [Int]
  | JoinTwoTrips Int Int Int String
  | JoinTwoRouter Int Int Int String
  | JoinTwoRouterAtStop Int Int Int Int String
  | CleanupTrip Int Int
  | ValidateTrip Int
  | FindNextStop Int Int
  | FindPreviousStop Int Int
  | TripDistance Int
-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"


















type Parser a = String -> Either String (a, String)

many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right ((v1, v2), r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (c v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

-- >>> parseExact "abc" "abcdef" 
-- Right ("abc","def")
-- >>> parseExact "." ".def"
-- Right (".","def")
-- >>> parseExact "." "de.f"
-- Left "Expected ."
-- >>> parseExact "" "def"
-- Right ("","def")
parseExact :: String -> Parser String
parseExact expected input =
  let len = length expected
  in if take len input == expected
       then Right (expected, drop len input)
       else Left $ "Expected " ++ expected

-- >>> parseExactInString "." "abc.def"
-- Right (".","def")
-- >>> parseExactInString "." "de.f.a"
-- Right (".","f.a")
-- >>> parseExactInString "." "123.123"
-- Right (".","123")
parseExactInString :: String -> Parser String
parseExactInString str [] = Left "Empty input, cannot parse a string"
parseExactInString str input =  
  let 
    letters = L.takeWhile (\c -> C.isLetter c || C.isNumber c) input
    rest = drop (length letters) input
  in 
    case parseExact str rest of
      Right (v1, r1) -> Right (v1, r1)
      Left e1 -> Left e1

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- <character> ::= [1-9] | [A-Z] | [a-z] | "_"
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h || h == '_' then Right (h, t) else Left (s ++ " does not start with a letter")


-- <string> ::= <character> <string> | <character>
-- >>> parseString "abc"
-- Right ("abc","")
-- >>> parseString "abc123"
-- Right ("abc","123")
-- >>> parseString "afeafe45"
parseString :: Parser [Char]
parseString = many parseLetter 


-- ", "
parseSeperator :: Parser String
parseSeperator input =
      case parseExactInString ", " input of
        Right(v2, r2) -> Right(v2, r2)
        Left e1 -> Left(e1)


parseDotSeperator :: Parser String
parseDotSeperator input =
  case parseExactInString "." input of
    Left e1 -> Left e1
    Right (v1, r1) -> 
      case parseExactInString "." r1 of
        Right _ -> Left "too many dots"
        Left _ -> Right (v1, r1)


-- <integer>
parseInteger :: Parser Integer
parseInteger [] = Left "empty input, cannot parse an integer"
parseInteger input =
    let
        digits = L.takeWhile C.isDigit input
        rest = drop (length digits) input
    in
        case digits of
            [] -> Left "not an integer"
            _ -> Right (read digits, rest)

-- <pos>
parsePositiveFloat :: Parser Float
parsePositiveFloat [] = Left "empty input, cannot parse a float"
parsePositiveFloat input =
    let
      isSeperatorOne = parseDotSeperator input
    in
      case isSeperatorOne of
        Right (_, _) -> 
          let
            digits = L.takeWhile (\c -> C.isDigit c || c == '.') input
            rest = drop (length digits) input
          in
              case digits of
                [] -> Left "not a float"
                _ -> Right (read digits, rest)
        Left e1 -> Left e1
       

-- <float>
parseFloat :: Parser Float
parseFloat [] = Left "empty input, cannot parse a float"
parseFloat input =
  let
    sign = parseChar '-' input
  in
    case sign of
      Right (_, r1) -> 
        case parsePositiveFloat r1 of
          Right (v2, r2) -> Right (-v2, r2)
          Left e2 -> Left e2
      Left _ -> 
        case parsePositiveFloat input of
          Right (v2, r2) -> Right (v2, r2)
          Left e2 -> Left e2




-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"
