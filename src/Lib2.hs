
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


-- >>> parseManyFunctions [parseName, (parseChar ' '), parseName] "JonasJonas jonelis"
-- Couldn't match type `Char' with `Name'
-- Expected: Parser Name
--   Actual: Parser Char
-- In the expression: parseChar ' '
-- In the first argument of `parseManyFunctions', namely
--   `[parseName, (parseChar ' '), parseName]'
-- In the expression:
--   parseManyFunctions
--     [parseName, (parseChar ' '), parseName] "JonasJonas jonelis"
-- Define a sum type to hold both Names and Chars
-- data NameOrChar = NameResult Name | CharResult Char deriving (Show)

-- Adapt your parsers to return this type

parseManyFunctions :: [Parser a] -> Parser [a]
parseManyFunctions [] _ = Left "empty input, cannot parse a number" -- Accepts two arguments
parseManyFunctions funct input = many' funct input [] -- Consistent two-argument pattern
  where
    many' [] s acc = Right (acc, s) -- Base case: if no more parsers, return accumulated results
    many' (f:fs) s acc =
      case f s of
        Right (v1, r1) -> many' fs r1 (acc ++ [v1]) -- Apply parser, continue with the rest
        Left _ -> Right (acc, s) -- If a parser fails, return accumulated results so far



  


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

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
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

parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h:t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")

-- <string> ::= <character> <string> | <character>
parseString :: Parser String
parseString = many (parseLetter `or2` parseDigit)


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

data TripId = TripId Char Int deriving Show

-- <trip_id> ::= "T" <integer>
-- >>> parseTripId "T123"
-- Right (TripId 'T' 123,"")
parseTripId :: Parser TripId
parseTripId [] = Left "empty input, cannot parse a trip id"
parseTripId input = 
  let
    typeChar = parseChar 'T' input
    in case typeChar of
      Left e1 -> Left e1
      Right (v0, r1) ->
        let
          tripId = parseInteger r1
          in case tripId of
            Left e1 -> Left e1
            Right (v1, r2) -> Right (TripId v0 (fromIntegral v1), r2)

data RouteId = RouteId Char Int deriving Show

-- <route_id> ::= <integer>
parseRouteId :: Parser RouteId
parseRouteId [] = Left "empty input, cannot parse a route id"
parseRouteId input = 
  let
    typeChar = parseChar 'R' input
    in case typeChar of
      Left e1 -> Left e1
      Right (v0, r1) ->
        let
          tripId = parseInteger r1
          in case tripId of
            Left e1 -> Left e1
            Right (v1, r2) -> Right (RouteId v0 (fromIntegral v1), r2)

data StopId = StopId Char Int deriving Show

-- <stop_id> ::= <integer>
parseStopId :: Parser StopId
parseStopId [] = Left "empty input, cannot parse a stop id"
parseStopId input = 
  let
    typeChar = parseChar 'S' input
    in case typeChar of
      Left e1 -> Left e1
      Right (v0, r1) ->
        let
          tripId = parseInteger r1
          in case tripId of
            Left e1 -> Left e1
            Right (v1, r2) -> Right (StopId v0 (fromIntegral v1), r2)

data PathId = PathId Char Int deriving Show

-- <path_id> ::= <integer>
parsePathId :: Parser PathId
parsePathId [] = Left "empty input, cannot parse a path id"
parsePathId input = 
  let
    typeChar = parseChar 'P' input
    in case typeChar of
      Left e1 -> Left e1
      Right (v0, r1) ->
        let
          tripId = parseInteger r1
          in case tripId of
            Left e1 -> Left e1
            Right (v1, r2) -> Right (PathId v0 (fromIntegral v1), r2)

data PathLenght = PathLenght Float deriving Show

-- <path_length> ::= <float>
parsePathLenght :: Parser PathLenght
parsePathLenght [] = Left "empty input, cannot parse a path length"
parsePathLenght input = 
  let
    pathLenght = parseFloat input
    in case pathLenght of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (PathLenght v1, r1)

data CoordX = CoordX Float deriving Show

-- <coord_x> ::= <float>
parseCoordX :: Parser CoordX
parseCoordX [] = Left "empty input, cannot parse a coord x"
parseCoordX input = 
  let
    coordX = parseFloat input
    in case coordX of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (CoordX v1, r1)

data CoordY = CoordY Float deriving Show

-- <coord_y> ::= <float>
parseCoordY :: Parser CoordY
parseCoordY [] = Left "empty input, cannot parse a coord y"
parseCoordY input = 
  let
    coordY = parseFloat input
    in case coordY of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (CoordY v1, r1)

data Point = Point CoordX CoordY deriving Show

-- <point> ::= <coord_x> ", " <coord_y>
parsePoint :: Parser Point
parsePoint input = (and3' (\a _ b -> Point a b) parseCoordX (parseSeperator) parseCoordY) input

data Name = Name String deriving Show

-- <name> ::= <string>
parseName :: Parser Name
parseName [] = Left "empty input, cannot parse a name"
parseName input = 
  let
    name = parseString input
    in case name of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (Name v1, r1)

data PreviousStopId = PreviousStopId StopId deriving Show

-- <previous_stop_id> ::= <stop_id>
parsePreviousStopId :: Parser PreviousStopId
parsePreviousStopId [] = Left "empty input, cannot parse a previous stop id"
parsePreviousStopId input = 
  let
    previousStopId = parseStopId input
    in case previousStopId of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (PreviousStopId v1, r1)

data NextStopId = NextStopId StopId deriving Show

-- <next_stop_id> ::= <stop_id>
parseNextStopId :: Parser NextStopId
parseNextStopId [] = Left "empty input, cannot parse a next stop id"
parseNextStopId input = 
  let
    nextStopId = parseStopId input
    in case nextStopId of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (NextStopId v1, r1)

data StopOrPath = StopID StopId | PathID PathId deriving Show

-- <stop_or_path> ::= <stop_id> | <path_id>
parseStopOrPath :: Parser StopOrPath
parseStopOrPath input = 
  let
    stopId = parseStopId input
    in case stopId of
      Right (v1, r1) -> Right (StopID v1, r1)
      Left _ -> 
        let
          pathId = parsePathId input
          in case pathId of
            Right (v1, r1) -> Right (PathID v1, r1)
            Left e1 -> Left e1


-- >>> parseManyFunctions [parseString, parseSeperator, parseString] "Jonas, Jonas"
-- Right (["Jonas",", ","Jonas"],"")

-- >>> skipEmptySpace [" ", "Jonas", "Jonas", "jonelis"]

-- >>> skipElements ["Jonas", "Jonas", "jonelis", "galva skauda"] "Jonas"
-- Right (["jonelis","galva skauda"],"")
skipElements :: [String] -> Parser [String]
skipElements input skip = 
      let
        skipy = many' skip [] input
        in case skipy of
          Left e1 -> Left e1
          Right (v2, r2) -> Right (v2, r2)
      where 
        many' [] acc _ = Left "empty remove element"
        many' rem acc [] = Right (acc, [])
        many' rem acc s@(h:t) = 
          if rem == h
            then many' rem acc t
            else many' rem (acc ++ [h]) t



-- parseFunctionArguments :: String -> Parser [String]
-- parseFunctionArguments com = 
--   let 
--     parseCom = parseExact com
--     in case parseCom of
--       Left e1 -> Left e1
--       Right (_, v1) - 
--         let
--           openBracket = parseExact "(" v1
--           in case openBracket of
--             Left e1 -> Left e1
--             Right (_, v2) -> 
--               let
--                 arguments = parseManyFunctions [parseString, parseSeperator] v2
--                 in case arguments of
--                   Left e1 -> Left e1
--                   Right (v1, v2) -> 
--                     let
--                       closeBracket = parseExact ")" v2
--                       in case closeBracket of
--                         Left e1 -> Left e1
--                         Right (_, v3) -> Right (v1, v3)


data Stop = Stop StopId Name Point deriving Show


-- <create_stop> ::= "create_stop(" <stop_id> ", " <name> ", " <point> ")"
-- >>> parseCreateStop "create_stop(S123, Jonas, 1.0, 2.0)"
-- Left "not an integer"


parseCreateStop :: Parser Stop
parseCreateStop [] = Left "empty input, cannot parse a create stop"
parseCreateStop input = 
  let 
    createStop = parseExact "create_stop(" input
    in case createStop of
      Left e1 -> Left e1
      Right (_, r1) -> 
        let
          stopId = parseStopId r1
          in case stopId of
            Left e1 -> Left e1
            Right (v1, r2) -> 
              let
                name = parseName r2
                in case name of
                  Left e1 -> Left e1
                  Right (v2, r3) -> 
                    let
                      point = parsePoint r3
                      in case point of
                        Left e1 -> Left e1
                        Right (v3, r4) -> 
                          let
                            closeBracket = parseExact ")" r4
                            in case closeBracket of
                              Left e1 -> Left e1
                              Right (_, r5) -> Right (Stop v1 v2 v3, r5)



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
