
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


parseExact :: String -> Parser String
parseExact expected input =
  let len = length expected
  in if take len input == expected
       then Right (expected, drop len input)
       else Left $ "Expected " ++ expected


checkIfStringIsInString :: String -> Parser (String, String)
checkIfStringIsInString str [] = Left "Empty input, cannot parse a string"
checkIfStringIsInString str input = parseExactInString' ([], str) input
  where
    parseExactInString' (_, _) []  = Left ("Cannot find " ++ str ++ " in the input")
    parseExactInString' (acc, str') s@(h:t)  =
        case parseExact str' s of
          Right (v1, r1) -> Right ((acc, v1), r1)
          Left _ -> parseExactInString' (acc ++ [h], str') t


parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)


-- <character> ::= [1-9] | [A-Z] | [a-z] | "_"
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h || h == '_' then Right (h, t) else Left (s ++ " does not start with a letter")


-- <string> ::= <character> <string> | <character>
parseString :: Parser String
parseString = many parseLetter


-- ", "
parseSeperator :: Parser String
parseSeperator input =
      case parseExact ", " input of
        Left e1 -> Left e1
        Right(v2, r2) -> Right (v2, r2)

-- "."
parseDotSeperator :: Parser String
parseDotSeperator input =
  case parseExact "." input of
    Left e1 -> Left e1
    Right (v1, r1) -> Right (v1, r1)


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
      integerPart = parseInteger input
      rest = case integerPart of
        Left e1 -> Left e1
        Right (v1, r) -> 
          case parseDotSeperator r of
            Left e2 -> Left e2
            Right (_, r2) -> 
              case parseInteger r2 of
                Left e3 -> Left e3
                Right (v3, r3) -> Right ((v1, v3), r3)
    in case rest of
      Left _ -> 
        case integerPart of
          Left e1 -> Left e1
          Right (v1, r1) -> Right (fromIntegral v1, r1)
      Right (v1, r1) -> Right (read (show (fst v1) ++ "." ++ show (snd v1)), r1)


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
