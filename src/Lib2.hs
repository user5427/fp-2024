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
import Data.Functor.Classes (eq1)
import Data.List (delete)
import Lib2.AndOrMany (many, and2, and2', and3', and5', or2)
import Lib2.ArrayFunc (getByExtractorFromArray, getByIndexFromArray, addToArray, updateArray, deleteFromArray, elementInArray)
import Lessons.Lesson04(Parser)

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query
  = CreateStop StopId Name Point -- +
  | CreateRoute RouteId Name [StopId] -- +
  | CreatePath PathId Name PathLenght StopId StopId -- +
  | CreateTrip TripId Name [QueryStopOrPathOrCreate] -- +~ QueryStopOrPathOrCreate
  | JoinTwoTrips QueryTrip QueryTrip TripId Name -- +
  | JoinTwoRoutes QueryRoute QueryRoute RouteId Name -- + QueryRoute
  | JoinTwoRoutesAtStop QueryRoute QueryRoute QueryStopOrCreatOrNextPrev RouteId Name --
  | CleanupTrip QueryTrip -- + QueryTrip
  | ValidateTrip QueryTrip -- +
  | FindNextStop StopId RouteId -- +
  | FindPreviousStop StopId RouteId -- +
  | TripDistance QueryTrip -- +
  | SetNextStop StopId RouteId StopId -- + it can automatically set the previous to?
  | SetPreviousStop StopId RouteId StopId -- +
  | ConnectRouteStopsByMinDistance RouteId -- +
  | CheckIfRouteStopsConnected RouteId -- +
  | DistanceBetweenStops StopId StopId -- +
  | AssignStopToRoute StopId RouteId -- +




-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- HELPER FUNCTIONS
parseExact :: String -> Parser String
parseExact expected input =
  let len = length expected
  in if take len input == expected
       then Right (expected, drop len input)
       else Left $ "Expected " ++ expected
-- HELPER FUNCTIONS
checkIfStringIsInString :: String -> Parser (String, String)
checkIfStringIsInString str [] = Left "Empty input, cannot parse a string"
checkIfStringIsInString str input = parseExactInString' ([], str) input
  where
    parseExactInString' (_, _) []  = Left ("Cannot find " ++ str ++ " in the input")
    parseExactInString' (acc, str') s@(h:t)  =
        case parseExact str' s of
          Right (v1, r1) -> Right ((acc, v1), r1)
          Left _ -> parseExactInString' (acc ++ [h], str') t
-- HELPER FUNCTIONS
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)
-- <character> ::= [1-9] | [A-Z] | [a-z] | "_"
parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h || h == '_' then Right (h, t) else Left (s ++ " does not start with a letter")
-- HELPER FUNCTIONS
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
-- HELPER FUNCTIONS
getStringFromParser :: Parser String -> Parser String
getStringFromParser parser input =
  let
    result = parser input
    in case result of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (v1, r1)
-- <trip_id> ::= "T" <integer>
data TripId = TripId Char Int deriving (Show, Eq)
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
-- <route_id> ::= <integer>
data RouteId = RouteId Char Int deriving (Show, Eq)
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
-- <stop_id> ::= <integer>
data StopId = StopId Char Int deriving (Show, Eq)
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
-- <list_of_stop_ids> ::= <stop_id> "," <list_of_stop_ids> | <stop_id>
parseStopIdList :: Parser [StopId]
parseStopIdList [] = Left "empty input, cannot parse a stop id list"
parseStopIdList input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input acc =
      case parseStopId input of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ ->
          case parseSeperator input of
            Right (_, r1) -> many' r1 acc
            Left e1 -> Right (acc, input)
-- <path_id> ::= <integer>
data PathId = PathId Char Int deriving (Show, Eq)
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
-- <path_length> ::= <float>
data PathLenght = PathLenght Float deriving (Show, Eq)
parsePathLenght :: Parser PathLenght
parsePathLenght [] = Left "empty input, cannot parse a path length"
parsePathLenght input =
  let
    pathLenght = parseFloat input
    in case pathLenght of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (PathLenght v1, r1)
-- <coord_x> ::= <float>
data CoordX = CoordX Float deriving (Show, Eq)
parseCoordX :: Parser CoordX
parseCoordX [] = Left "empty input, cannot parse a coord x"
parseCoordX input =
  let
    coordX = parseFloat input
    in case coordX of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (CoordX v1, r1)
-- <coord_y> ::= <float>
data CoordY = CoordY Float deriving (Show, Eq)
parseCoordY :: Parser CoordY
parseCoordY [] = Left "empty input, cannot parse a coord y"
parseCoordY input =
  let
    coordY = parseFloat input
    in case coordY of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (CoordY v1, r1)
-- <point> ::= <coord_x> ", " <coord_y>
data Point = Point CoordX CoordY deriving (Show, Eq)
parsePoint :: Parser Point
parsePoint = (and3' (\a _ b -> Point a b) parseCoordX (parseSeperator) parseCoordY)
-- <name> ::= <string>
data Name = Name String deriving (Show, Eq)
parseName :: Parser Name
parseName [] = Left "empty input, cannot parse a name"
parseName input =
  let
    name = parseString input
    in case name of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (Name v1, r1)
-- <stop_or_path> ::= <stop_id> | <path_id>
data QueryStopOrPath = StopId' StopId | PathId' PathId deriving (Show, Eq)
parseQueryStopOrPath :: Parser QueryStopOrPath
parseQueryStopOrPath input =
  let
    res = or2 (and2' (\a _ -> StopId' a) parseStopId parseSeperator)
          (and2' (\a _ -> PathId' a) parsePathId parseSeperator) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- + <list_of_stops_and_paths> ::= <stop_or_path> "," <list_of_stops_and_paths> | <stop_or_path> 
parseQueryStopOrPathList :: Parser [QueryStopOrPath]
parseQueryStopOrPathList input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input acc =
      case parseQueryStopOrPath input of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ ->
          case parseSeperator input of
            Right (_, r1) -> many' r1 acc
            Left e1 -> Right (acc, input)
data QueryStopOrPathOrCreate = QueryStopOrPath' QueryStopOrPath | CreateStop' Query | FindNextStop' Query | FindPreviousStop' Query deriving (Show, Eq)
-- <stop_or_path_or_creat> ::= <create_stop> | <stop_or_path> | <find_next_stop> | <find_previous_stop>
parseQueryStopOrPathOrCreat :: Parser QueryStopOrPathOrCreate
parseQueryStopOrPathOrCreat input = 
  let 
    res = parseQueryCreateStop input
    in case res of
      Right (r1, v1) -> Right (CreateStop' r1, v1)
      Left _ -> 
        let 
          res' = parseQueryStopOrPath input
          in case res' of
            Right (r1, v1) -> Right (QueryStopOrPath' r1, v1)
            Left e1 -> 
              let res'' = parseQueryFindNextStop input
              in case res'' of
                Right (r1, v1) -> Right (FindNextStop' r1, v1)
                Left e2 -> 
                  let res''' = parseQueryFindPreviousStop input
                  in case res''' of
                    Right (r1, v1) -> Right (FindPreviousStop' r1, v1)
                    Left e3 -> Left e3
-- <list_of_stops_paths_creat> ::= <stop_or_path_or_creat> "," <list_of_stops_paths_creat> | <stop_or_path_or_creat>
parseQueryStopOrPathOrCreatList :: Parser [QueryStopOrPathOrCreate]
parseQueryStopOrPathOrCreatList input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input acc =
      case parseQueryStopOrPathOrCreat input of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ ->
          case parseSeperator input of
            Right (_, r1) -> many' r1 acc
            Left e1 -> Right (acc, input)
-- <stop_or_creat_or_nextprev> ::= <create_stop> | <stop_id> | <find_next_stop> | <find_previous_stop>
data QueryStopOrCreatOrNextPrev = QueryStopOrCreatOrNextPrevStop StopId | QueryStopOrCreatOrNextPrevCreateStop Query | QueryStopOrCreatOrNextPrevFindNextStop Query | QueryStopOrCreatOrNextPrevFindPreviousStop Query deriving (Show, Eq)
parseQueryStopOrCreatOrNextPrev :: Parser QueryStopOrCreatOrNextPrev
parseQueryStopOrCreatOrNextPrev input = 
  let 
    res = parseQueryCreateStop input
    in case res of
      Right (r1, v1) -> Right (QueryStopOrCreatOrNextPrevCreateStop r1, v1)
      Left _ -> 
        let 
          res' = parseStopId input
          in case res' of
            Right (r1, v1) -> Right (QueryStopOrCreatOrNextPrevStop r1, v1)
            Left e1 -> 
              let res'' = parseQueryFindNextStop input
              in case res'' of
                Right (r1, v1) -> Right (QueryStopOrCreatOrNextPrevFindNextStop r1, v1)
                Left e2 -> 
                  let res''' = parseQueryFindPreviousStop input
                  in case res''' of
                    Right (r1, v1) -> Right (QueryStopOrCreatOrNextPrevFindPreviousStop r1, v1)
                    Left e3 -> Left e3
-- <trip> ::= <trip_id> | <create_trip>
data QueryTrip = Trip' TripId | CreateTrip' Query deriving (Show, Eq)
parseQueryTrip :: Parser QueryTrip
parseQueryTrip input =
  let
    res = parseTripId input
    in case res of
      Right (r1, v1) -> Right (Trip' r1, v1)
      Left e1 -> 
        let res' = parseQueryCreateTrip input
        in case res' of
          Right (r1, v1) -> Right (CreateTrip' r1, v1)
          Left e2 -> Left e2
-- <route> ::= <route_id> | <create_route> | <join_two_routes> | <join_two_routes_at_stop> -- Route
data QueryRoute = Route' RouteId | CreateRoute' Query | JoinTwoRoutes' Query | JoinTwoRoutesAtStop' Query deriving (Show, Eq)
parseQueryRoute :: Parser QueryRoute
parseQueryRoute input =
  let
    res = parseRouteId input
    in case res of
      Right (r1, v1) -> Right (Route' r1, v1)
      Left e1 -> 
        let res' = parseQueryCreateRoute input
        in case res' of
          Right (r1, v1) -> Right (CreateRoute' r1, v1)
          Left e2 -> 
            let res'' = parseQueryJoinTwoRouter input
            in case res'' of
              Right (r1, v1) -> Right (JoinTwoRoutes' r1, v1)
              Left e3 -> 
                let res''' = parseQueryJoinTwoRouterAtStop input
                in case res''' of
                  Right (r1, v1) -> Right (JoinTwoRoutesAtStop' r1, v1)
                  Left e4 -> Left e4
--
--  | Functions
-- \ /
-- <create_stop> ::= "create_stop(" <stop_id> ", " <name> ", " <point> ")"
parseQueryCreateStop :: Parser Query
parseQueryCreateStop input =
  let
    res = and3' (\a b c -> CreateStop a b c)
          (and3' (\_ b _ -> b) (parseExact "create_stop(") parseStopId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator parsePoint (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <create_route> ::= "create_route(" <route_id> ", " <name> ", " <list_of_stop_ids> ")"
parseQueryCreateRoute :: Parser Query
parseQueryCreateRoute input =
  let
    res = and3' (\a b c -> CreateRoute a b c)
          (and3' (\_ b _ -> b) (parseExact "create_route(") parseRouteId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator (parseStopIdList) (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <create_path> ::= "create_path(" <path_id> ", " <name> ", " <path_length> ", " <stop_id> ", " <stop_id> ")"
parseQueryCreatePath :: Parser Query
parseQueryCreatePath input =
  let
    res = and5' (\a b c d e -> CreatePath a b c d e)
          (and3' (\_ b _ -> b) (parseExact "create_path(") parsePathId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator parsePathLenght (parseSeperator))
          (and3' (\_ d _ -> d) parseSeperator parseStopId parseSeperator)
          (and3' (\_ e _ -> e) parseSeperator parseStopId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <create_trip> ::= "create_trip(" <trip_id> ", " <name> ", " <list_of_stops_paths_creat> ")"
parseQueryCreateTrip :: Parser Query
parseQueryCreateTrip input =
  let
    res = and3' (\a b c -> CreateTrip a b c)
          (and3' (\_ b _ -> b) (parseExact "create_trip(") parseTripId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator (parseQueryStopOrPathOrCreatList) (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <find_next_stop> ::= "find_next_stop(" <stop_id> ", " <route_id> ")"
parseQueryFindNextStop :: Parser Query
parseQueryFindNextStop input =
  let
    res = and3' (\_ a b -> FindNextStop a b)
          (parseExact "find_next_stop(")
          parseStopId
          (and3' (\_ c _ -> c) parseSeperator parseRouteId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <find_previous_stop> ::= "find_previous_stop(" <stop_id> ", " <route_id> ")"
parseQueryFindPreviousStop :: Parser Query
parseQueryFindPreviousStop input =
  let
    res = and3' (\_ a b -> FindPreviousStop a b)
          (parseExact "find_previous_stop(")
          parseStopId
          (and3' (\_ c _ -> c) parseSeperator parseRouteId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <set_next_stop> ::= "set_next_stop(" <stop_id> ", " <route_id> ", " <next_stop_id> ")"
parseQuerySetNextStop :: Parser Query
parseQuerySetNextStop input =
  let
    res = and3' (\c a b -> SetNextStop c a b)
          (and3' (\_ a _ -> a) (parseExact "set_next_stop(") parseStopId parseSeperator)
          (and2' (\c _ -> c) parseRouteId parseSeperator)
          (and2' (\d _ -> d) parseStopId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <set_previous_stop> ::= "set_previous_stop(" <stop_id> ", " <route_id> ", " <previous_stop_id> ")"
parseQuerySetPreviousStop :: Parser Query
parseQuerySetPreviousStop input =
  let
    res = and3' (\c a b -> SetPreviousStop c a b)
          (and3' (\_ a _ -> a) (parseExact "set_previous_stop(") parseStopId parseSeperator)
          (and2' (\c _ -> c) parseRouteId parseSeperator)
          (and2' (\d _ -> d) parseStopId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <join_two_trips> ::= "join_two_trips(" <trip> ", " <trip> ", " <new_trip_id> ", " <new_name> ")"
parseQueryJoinTwoTrips :: Parser Query
parseQueryJoinTwoTrips input =
  let
    res = and5' (\_ a b c d -> JoinTwoTrips a b c d)
          (parseExact "join_two_trips(")
          parseQueryTrip
          (and3' (\_ b _ -> b) parseSeperator parseQueryTrip parseSeperator)
          parseTripId
          (and3' (\_ c _ -> c) parseSeperator parseName (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <assign_stop_to_route> ::= "assign_stop_to_route(" <stopId> ", " <route_id ")"
parseQueryAssignStopToRoute :: Parser Query
parseQueryAssignStopToRoute input =
  let
    res = and3' (\a b _ -> AssignStopToRoute a b)
          (and3' (\_ b _ -> b) (parseExact "assign_stop_to_route(") parseStopId parseSeperator)
          parseRouteId
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <distance_between_stops> ::= "distance_between_stops(" <stop_id> ", " <stop_id> ")"
parseQueryDistanceBetweenStops :: Parser Query
parseQueryDistanceBetweenStops input =
  let
    res = and3' (\_ a b -> DistanceBetweenStops a b)
          (parseExact "distance_between_stops(")
          parseStopId
          (and3' (\_ c _ -> c) parseSeperator parseStopId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <check_if_route_stops_connected> ::= "check_if_route_stops_connected(" <route_id> ")"
parseQueryCheckIfRouteStopsConnected :: Parser Query
parseQueryCheckIfRouteStopsConnected input =
  let
    res = and3' (\_ a _ -> CheckIfRouteStopsConnected a)
          (parseExact "check_if_route_stops_connected(")
          parseRouteId
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <connect_route_stops_by_min_dist> ::= "connect_route_stops_by_min_dist(" <route_id> ")"
parseQueryConnectRouteStopsByMinDistance :: Parser Query
parseQueryConnectRouteStopsByMinDistance input =
  let
    res = and3' (\_ a _ -> ConnectRouteStopsByMinDistance a)
          (parseExact "connect_route_stops_by_min_dist(")
          parseRouteId
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <trip_distance> ::= "trip_distance(" <trip> ")"
parseQueryTripDistance :: Parser Query
parseQueryTripDistance input =
  let
    res = and3' (\_ a _ -> TripDistance a)
          (parseExact "trip_distance(")
          parseQueryTrip
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <validate_trip> ::= "validate_trip(" <trip> ")"
parseQueryValidateTrip :: Parser Query
parseQueryValidateTrip input =
  let
    res = and3' (\_ a _ -> ValidateTrip a)
          (parseExact "validate_trip(")
          parseQueryTrip
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <cleanup_trip> ::= "cleanup_trip(" <trip> ")"
parseQueryCleanupTrip :: Parser Query
parseQueryCleanupTrip input =
  let
    res = and3' (\_ a _ -> CleanupTrip a)
          (parseExact "cleanup_trip(")
          parseQueryTrip
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <join_two_routes> ::= "join_two_routes(" <route> ", " <route> ", " <new_route_id> ", " <new_name> ")"
parseQueryJoinTwoRouter :: Parser Query
parseQueryJoinTwoRouter input =
  let
    res = and5' (\_ a b c d -> JoinTwoRoutes a b c d)
          (parseExact "join_two_routes(")
          parseQueryRoute
          (and3' (\_ b _ -> b) parseSeperator parseQueryRoute parseSeperator)
          parseRouteId
          (and3' (\_ c _ -> c) parseSeperator parseName (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)
-- <join_two_routes_at_stop> ::= "join_two_routes_at_stop(" <route> ", " <route> ", " <stop_or_creat_or_nextprev> ", " <new_route_id> ", " <new_name> ")"
parseQueryJoinTwoRouterAtStop :: Parser Query
parseQueryJoinTwoRouterAtStop input =
  let
    res = and5' (\a b c d e -> JoinTwoRoutesAtStop a b c d e)
          (and3' (\_ a _ -> a) (parseExact "join_two_routes_at_stop(") parseQueryRoute parseSeperator)
          (and2' (\_ b -> b) parseSeperator parseQueryRoute)
          (and3' (\_ c _ -> c) parseSeperator parseQueryStopOrCreatOrNextPrev parseSeperator)
          parseRouteId
          (and3' (\_ c _ -> c) parseSeperator parseName (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right (r1, v1)


-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input =
  let
    res = (parseQueryCreateStop `or2` parseQueryCreateRoute 
      `or2` parseQueryCreatePath `or2` parseQueryCreateTrip 
        `or2` parseQueryJoinTwoTrips `or2` parseQueryJoinTwoRouter 
          `or2` parseQueryJoinTwoRouterAtStop `or2` parseQueryCleanupTrip 
            `or2` parseQueryValidateTrip `or2` parseQueryFindNextStop 
              `or2` parseQueryFindPreviousStop `or2` parseQueryTripDistance 
                `or2` parseQuerySetNextStop `or2` parseQuerySetPreviousStop 
                  `or2` parseQueryConnectRouteStopsByMinDistance `or2` parseQueryCheckIfRouteStopsConnected 
                    `or2` parseQueryDistanceBetweenStops `or2` parseQueryAssignStopToRoute) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> Right r1

addStopIdToRoute :: StopId -> Route -> Either String Route
addStopIdToRoute stopId route@(Route rid name stops) =
  let
    newRoute = Route rid name (stops ++ [stopId])
    in Right newRoute
-- HELPER FUNCTIONS
assignStopsToRoute :: [Stop] -> RouteId -> Either String [Stop]
assignStopsToRoute stops' routeId = assignStopsToRoute' stops' routeId []
  where
    assignStopsToRoute' [] _ acc = Right acc
    assignStopsToRoute' (h:t) routeId acc =
      let
        stop@(Stop sid name point nextStops prevStops routes') = h
        newStop = Stop sid name point nextStops prevStops (routeId : routes')
        in assignStopsToRoute' t routeId (acc ++ [newStop])
-- HELPER FUNCTIONS
distanceBetweenPoints :: Point -> Point -> Float
distanceBetweenPoints (Point (CoordX x1) (CoordY y1)) (Point (CoordX x2) (CoordY y2)) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
-- HELPER FUNCTIONS
getStopsFromStopIdList :: [Stop] -> [StopId] -> Either String [Stop]
getStopsFromStopIdList _ [] = Right []
getStopsFromStopIdList stops' (h:t) =
  let
    stop = getByExtractorFromArray h (\(Stop sid _ _ _ _ _) -> sid) stops'
    in case stop of
      Left e1 -> Left e1
      Right foundStop ->
        let
          rest = getStopsFromStopIdList stops' t
          in case rest of
            Left e2 -> Left e2
            Right foundRest -> Right (foundStop : foundRest)
-- HELPER FUNCTIONS
findClosestStop :: Stop -> [Stop] -> Either String Stop
findClosestStop _ [] = Left "Closest stop not found"
findClosestStop stop [] = Left "Closest stop not found"
findClosestStop stop input =
  let
    closStop = findClosestStop' stop input (-1) stop
    in case closStop of
      Left e1 -> Left e1
      Right v1 -> Right v1

    where
      findClosestStop' a [] _ b = if a == b then Left "Closest stop not found" else Right b
      findClosestStop' a (h:t) distance closestStop =
        let
          newDistance = distanceBetweenStopsData a h
          in if newDistance < distance || distance == (-1)
            then findClosestStop' a t newDistance h
            else findClosestStop' a t distance closestStop
-- HELPER FUNCTIONS
distanceBetweenStopsData :: Stop -> Stop-> Float
distanceBetweenStopsData a b =
  let
    aPoint = (case a of (Stop _ _ a' _ _ _ ) -> a')
    bPoint = (case b of (Stop _ _ b' _ _ _ ) -> b')
    in distanceBetweenPoints aPoint bPoint
-- HELPER FUNCTIONS
getRouteStops :: RouteId -> [Stop] -> Either String [Stop]
getRouteStops _ [] = Left "Route stops not found"
getRouteStops routeId stops' = getRouteStops' routeId stops' []
  where
    getRouteStops' _ [] [] = Left "Route stops not found"
    getRouteStops' _ [] acc = Right acc
    getRouteStops' routeId' (h:t) acc =
      let
        routesFromStop = case h of (Stop _ _ _ _ _ routes) -> routes
        in case L.find (== routeId') routesFromStop of
          Nothing -> getRouteStops' routeId' t acc
          Just _ -> getRouteStops' routeId' t (acc ++ [h])
-- HELPER FUNCTIONS for validateTrip
stopConnectedWithPath :: StopOrPath -> StopOrPath -> Bool
stopConnectedWithPath sop1 sop2 =
  case (sop1, sop2) of
    (Stop' (Stop sid _ _ _ _ _), Path' (Path _ _ _ stopId1 stopId2)) ->
      sid == stopId1 || sid == stopId2
    (Path' (Path _ _ _ stopId1' stopId2'), Stop' (Stop sid _ _ _ _ _)) ->
      sid == stopId1' || sid == stopId2'
    _ -> False
-- HELPER FUNCTIONS for validateTrip
pathWithPathConnected :: StopOrPath -> StopOrPath -> Bool
pathWithPathConnected sop1 sop2 =
  case (sop1, sop2) of
    (Path' p1, Path' p2) -> pathWithPathConnected' p1 p2
    _ -> False

    where 
      pathWithPathConnected' p1 p2 =
        let
          path@(Path _ _ _ stopId1 stopId2) = p1
          path2@(Path _ _ _ stopId1' stopId2') = p2
          in stopId1 == stopId1' || stopId1 == stopId2' || stopId2 == stopId1' || stopId2 == stopId2'
-- HELPER FUNCTIONS for validateTrip
-- check if the stopsAndPaths are connected with each other
connectedForwards :: StopOrPath -> StopOrPath -> Bool
connectedForwards sop1 sop2 =
  let
    stopConWithPath = stopConnectedWithPath sop1 sop2
  in stopConWithPath || 
     case (sop1, sop2) of
       (Stop' (Stop _ _ _ nextStops _ _), Stop' (Stop sid2 _ _ _ _ _)) ->
         let
           findNext = getByExtractorFromArray sid2 (\(NextStop sid _) -> sid) nextStops
         in case findNext of
              Right _ -> True
              Left _  -> pathWithPathConnected sop1 sop2
       _ -> pathWithPathConnected sop1 sop2
-- HELPER FUNCTIONS for validateTrip
connectBackwards :: StopOrPath -> StopOrPath -> Bool
connectBackwards sop1 sop2 =
  let
    stopConWithPath = stopConnectedWithPath sop1 sop2
  in stopConWithPath || 
     case (sop1, sop2) of
       (Stop' (Stop _ _ _ _ prevStops _), Stop' (Stop sid2 _ _ _ _ _)) ->
         let
           findPrev = getByExtractorFromArray sid2 (\(PreviousStop sid _) -> sid) prevStops
         in case findPrev of
              Right _ -> True
              Left _  -> pathWithPathConnected sop1 sop2
       _ -> pathWithPathConnected sop1 sop2

-- <stop_or_path> ::= <stop_id> | <path_id>
data StopOrPath = Stop' Stop
                  | Path' Path deriving (Show, Eq)

data NextStop = NextStop StopId RouteId deriving (Show, Eq)
data PreviousStop = PreviousStop StopId RouteId deriving (Show, Eq)
data Stop = Stop StopId Name Point [NextStop] [PreviousStop] [RouteId] deriving (Show, Eq)
-- <create_stop> ::= "create_stop(" <stop_id> ", " <name> ", " <point> ")"
createStop :: State -> Query -> Either String (Stop, State)
createStop state' query' =
  case query' of 
    (CreateStop stopId name point) -> 
      let
        stop = (Stop stopId name point [] [] [])
        updateState = addStop stop state'
        in case updateState of
          Left e1 -> Left e1
          Right newState -> Right (stop, newState)
    _ -> Left "Not a create stop query"
-- <create_route> ::= "create_route(" <route_id> ", " <name> ", " <list_of_stop_ids> ")"
data Route = Route RouteId Name [StopId] deriving (Show, Eq)
createRoute :: State -> Query -> Either String (Route, State)
createRoute state query' =
  case query' of
    (CreateRoute routeId name stopIds) ->
      let
        route = Route routeId name stopIds
        routeStops = getStopsFromStopIdList (stops state) stopIds
        in case routeStops of
          Left e1 -> Left e1
          Right foundRouteStops ->
            let
              assigned = assignStopsToRoute foundRouteStops routeId
              in case assigned of
                Left e2 -> Left e2
                Right foundStops -> 
                  let
                    updateStops = updateOrAddStops foundStops state
                    in case updateStops of
                      Left e3 -> Left e3
                      Right newState -> 
                        let
                          addRoute' = addRoute route newState
                          in case addRoute' of
                            Left e4 -> Left e4
                            Right finalState -> Right (route, finalState)
    _ -> Left "Not a create route query"
-- <create_trip> ::= "create_trip(" <trip_id> ", " <name> ", " <list_of_stops_paths_creat> ")"
data Trip = Trip TripId Name [StopOrPath] deriving (Show, Eq)
createTrip :: State -> Query -> Either String (Trip, State)
createTrip state query' =
  case query' of
    (CreateTrip tripId name stopOrPathList) ->
      let
        parseStopOrPathOrCreat = parseQueryStopOrPathOrCreatListData stopOrPathList state
        in case parseStopOrPathOrCreat of 
          Left e994 -> Left e994
          Right (stopsAndPaths, state'''') ->
            let
              trip = Trip tripId name stopsAndPaths -- FIXME CHANGE IT TO STOPORPATH
              addTrip' = addTrip trip state''''
              in case addTrip' of
                Left e1 -> Left e1
                Right newState -> Right (trip, newState)
    _ -> Left "Not a create trip query"
-- <create_path> ::= "create_path(" <path_id> ", " <name> ", " <path_length> ", " <stop_id> ", " <stop_id> ")"
data Path = Path PathId Name PathLenght StopId StopId deriving (Show, Eq)
createPath :: State -> Query -> Either String (Path, State)
createPath state query' =
  case query' of
    (CreatePath pathId name pathLenght stopId1 stopId2) ->
      let
        path = Path pathId name pathLenght stopId1 stopId2
        addPath' = addPath path state
        in case addPath' of
          Left e1 -> Left e1
          Right newState -> Right (path, newState)
    _ -> Left "Not a create path query"
-- <assign_stop_to_route> ::= "assign_stop_to_route(" <stopId> ", " <route_id ")"
assignStopToRoute :: State -> Query -> Either String (Stop, State)
assignStopToRoute state query' =
  case query' of
    (AssignStopToRoute stopId routeId) ->
      let
        stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
        in case stop of
          Left e1 -> Left e1
          Right foundStop ->
            let
              updatedStop = assignStopToRouteData foundStop routeId
              in case updatedStop of
                Left e2 -> Left e2
                Right stop' ->
                  let
                    updateStop = updateOrAddStops [stop'] state
                    in case updateStop of
                      Left e2 -> Left e2
                      Right newState -> 
                        let
                          route = getByExtractorFromArray routeId (\(Route rid _ _) -> rid) (routes state)
                          in case route of
                            Left e3 -> Left e3
                            Right route@(Route rid name stops') ->
                              let
                                updatedRoute = Route rid name (stops' ++ [stopId])
                                in case updateRoute updatedRoute newState of
                                  Left e4 -> Left e4
                                  Right finalState -> Right (stop', finalState)
    _ -> Left "Not an assign stop to route query"
assignStopToRouteData :: Stop -> RouteId -> Either String Stop
assignStopToRouteData stop routeId =
  let
    (Stop sid name point nextStops prevStops routes') = stop
    in case L.find (== routeId) routes' of
      Nothing -> Right( Stop sid name point nextStops prevStops (routeId : routes'))
      Just _ -> Left "Stop already belongs to the route"
-- <distance_between_stops> ::= "distance_between_stops(" <stop_id> ", " <stop_id> ")"
distanceBetweenStops :: State -> Query -> Either String (Float, State)
distanceBetweenStops state query' =
  case query' of
    (DistanceBetweenStops stopId1 stopId2) ->
      let
        stop1 = getByExtractorFromArray stopId1 (\(Stop sid _ _ _ _ _) -> sid) (stops state)
        in case stop1 of
          Left e1 -> Left e1
          Right foundStop1@(Stop _ _ c' _ _ _) ->
            let
              stop2 = getByExtractorFromArray stopId2 (\(Stop sid _ _ _ _ _) -> sid) (stops state)
              in case stop2 of
                Left e2 -> Left e2
                Right foundStop2@(Stop _ _ d' _ _ _) ->
                  let
                    distance = distanceBetweenPoints c' d'
                    in Right (distance, state)
    _ -> Left "Not a distance between stops query"
-- <set_next_stop> ::= "set_next_stop(" <stop_id> ", " <route_id> ", " <next_stop_id> ")"
-- check if both stops belong to the same route
-- check if the stops are not the same
-- check if the stops are not already connected
-- check if the distance is not 0
setNextStop :: State -> Query -> Either String State
setNextStop state query' =
  case query' of
    (SetNextStop stopId routeId nextStopId) ->
      let
        stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
        in case stop of
          Left e1 -> Left e1
          Right stop' ->
            let
              nextStop = getByExtractorFromArray nextStopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
              in case nextStop of
                Left e2 -> Left e2
                Right nextStop' ->
                  let
                    updatedStop = parseSetNextStopData stop' routeId nextStop'
                    in case updatedStop of
                      Left e3 -> Left e3
                      Right stop'' ->
                        let
                          updateStop' = updateStop stop'' state
                          in case updateStop' of
                            Left e4 -> Left e4
                            Right newState -> Right newState
    _ -> Left "Not a set next stop query"
parseSetNextStopData :: Stop -> RouteId -> Stop -> Either String Stop
parseSetNextStopData stop routeId nextStop =
  let
    currentStop@(Stop sid name point nextStops prevStops routes') = stop
    in case L.find (== routeId) routes' of
      Nothing -> Left "Stop does not belong to the route"
      Just _ ->
        let
          nextStop@(Stop nextStopId _ _ _ _ routes'') = nextStop
          in case L.find (== routeId) routes'' of
            Nothing -> Left "Next stop does not belong to the route"
            Just _ ->
              let
                sameNextStop = getByExtractorFromArray routeId (\(NextStop _ rid) -> rid) nextStops
                in case sameNextStop of
                  Right e5 -> Left "Next stop already exists for that route"
                  Left _ -> let
                    in if stop == nextStop then Left "Stop cannot be connected to itself" else
                      let
                        distance = distanceBetweenStopsData stop nextStop
                        in if distance == 0 then Left "Distance between stops is 0" else
                          Right (Stop sid name point (NextStop nextStopId routeId : nextStops) prevStops routes')
-- <set_previous_stop> ::= "set_previous_stop(" <stop_id> ", " <route_id> ", " <previous_stop_id> ")"
-- same for previous stop
setPreviousStop :: State -> Query -> Either String State
setPreviousStop state query' =
  case query' of
    (SetPreviousStop stopId routeId previousStopId) ->
      let
        stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
        in case stop of
          Left e1 -> Left e1
          Right stop' ->
            let
              previousStop = getByExtractorFromArray previousStopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
              in case previousStop of
                Left e2 -> Left e2
                Right previousStop' ->
                  let
                    updatedStop = parseSetPreviousStopData stop' routeId previousStop'
                    in case updatedStop of
                      Left e3 -> Left e3
                      Right stop'' ->
                        let
                          updateStop' = updateStop stop'' state
                          in case updateStop' of
                            Left e4 -> Left e4
                            Right newState -> Right newState
    _ -> Left "Not a set previous stop query"
parseSetPreviousStopData :: Stop -> RouteId -> Stop -> Either String Stop
parseSetPreviousStopData stop routeId previousStop =
  let
    currentStop@(Stop sid name point nextStops prevStops routes') = stop
    in case L.find (== routeId) routes' of
      Nothing -> Left "Stop does not belong to the route"
      Just _ ->
        let
          previousStop@(Stop previousStopId _ _ _ _ routes'') = previousStop
          in case L.find (== routeId) routes'' of
            Nothing -> Left "Previous stop does not belong to the route"
            Just _ ->
              let
                samePreviousStop = getByExtractorFromArray routeId (\(PreviousStop _ rid) -> rid) prevStops
                in case samePreviousStop of
                  Right e5 -> Left "Previous stop already exists for that route"
                  Left _ -> let
                    in if stop == previousStop then Left "Stop cannot be connected to itself" else
                      let
                        distance = distanceBetweenStopsData stop previousStop
                        in if distance == 0 then Left "Distance between stops is 0" else
                          Right (Stop sid name point nextStops (PreviousStop previousStopId routeId : prevStops) routes')
-- <find_next_stop> ::= "find_next_stop(" <stop_id> ", " <route> ")"
findNextStop :: State -> Query -> Either String (StopId, State)
findNextStop state query' =
  case query' of
    (FindNextStop stopId routeId) ->
      let
        nextStop = findNextStopData stopId routeId (stops state)
        in case nextStop of
          Left e1 -> Left e1
          Right foundNextStopId -> Right (foundNextStopId, state)
    _ -> Left "Not a find next stop query"
findNextStopData :: StopId -> RouteId -> [Stop] -> Either String StopId
findNextStopData stopId routeId stops' =
  let
    stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) stops'
    in case stop of
      Left e1 -> Left e1
      Right foundStop@(Stop _ _ _ nextStops _ _) ->
        let
          nextStopId = findNextStop' routeId nextStops
          in case nextStopId of
            Left e2 -> Left e2
            Right foundNextStopId -> Right foundNextStopId

        where
          findNextStop' _ [] = Left "Next stop not found"
          findNextStop' route nextStops' =
            case L.find (\(NextStop _ routeId) -> routeId == route) nextStops' of
              Nothing -> Left "Next stop not found"
              Just (NextStop nextStopId _) -> Right nextStopId
-- <find_previous_stop> ::= "find_previous_stop(" <stop_id> ", " <route> ")"
findPreviousStop :: State -> Query -> Either String (StopId, State)
findPreviousStop state query' =
  case query' of
    (FindPreviousStop stopId routeId) ->
      let
        previousStop = findPreviousStopData stopId routeId (stops state)
        in case previousStop of
          Left e1 -> Left e1
          Right foundPreviousStopId -> Right (foundPreviousStopId, state)
    _ -> Left "Not a find previous stop query"
findPreviousStopData :: StopId -> RouteId -> [Stop] -> Either String StopId
findPreviousStopData stopId routeId stops' =
  let
    stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) stops'
    in case stop of
      Left e1 -> Left e1
      Right foundStop@(Stop _ _ _ _ prevStops _) ->
        let
          previousStopId = findPreviousStop' routeId prevStops
          in case previousStopId of
            Left e2 -> Left e2
            Right foundPreviousStopId -> Right foundPreviousStopId

        where
          findPreviousStop' _ [] = Left "Previous stop not found"
          findPreviousStop' route prevStops' =
            case L.find (\(PreviousStop _ routeId) -> routeId == route) prevStops' of
              Nothing -> Left "Previous stop not found"
              Just (PreviousStop previousStopId _) -> Right previousStopId
-- <connect_route_stops_by_min_dist> ::= "connect_route_stops_by_min_dist(" <route_id> ")"
connectRouteStopsByMinDist :: State -> Query -> Either String State
connectRouteStopsByMinDist state query' =
  case query' of
    (ConnectRouteStopsByMinDistance routeId) ->
      let
        routeStops = getRouteStops routeId (stops state)
        in case routeStops of
          Left e1 -> Left e1
          Right foundRouteStops ->
            let
              connectedStops = connectStopsByMinDistData foundRouteStops routeId
              in case connectedStops of
                Left e2 -> Left e2
                Right connectedStops' ->
                  let
                    updateStops = updateOrAddStops connectedStops' state
                    in case updateStops of
                      Left e3 -> Left e3
                      Right newState -> Right newState
    _ -> Left "Not a connect route stops by min dist query"
connectStopsByMinDistData :: [Stop] -> RouteId -> Either String [Stop]
connectStopsByMinDistData [] _ = Right []
connectStopsByMinDistData [stop] _ = Right [stop]
connectStopsByMinDistData (stop:rest) route' =
  let
    connectedStops = connectStopsByMinDist' stop stop rest []
    in case connectedStops of
      Left e1 -> Left e1
      Right connectedStops' -> Right connectedStops'


  where
    connectStopsByMinDist' prev' stop' [] acc =
      let
        newPrevStop = parseSetPreviousStopData stop' route' prev'
        in case newPrevStop of
          Left e1 -> Left e1
          Right newPrevStop' -> Right (acc ++ [newPrevStop'])

    connectStopsByMinDist' prev' stop' (h:t) acc =
      let
        closestStop = findClosestStop stop' (h:t)
        in case closestStop of
          Left e1 -> Left e1
          Right closestStop' -> let
            newStop = parseSetNextStopData stop' route' closestStop'
            in case newStop of
              Left e2 -> Left e2
              Right newStop' ->
                let
                  in if stop' == h then connectStopsByMinDist' newStop' closestStop' t (acc ++ [newStop'])
                  else
                    let
                      newPrevStop = parseSetPreviousStopData newStop' route' prev'
                      in case newPrevStop of
                        Left e3 -> Left e3
                        Right newPrevStop' -> connectStopsByMinDist' newPrevStop' closestStop' t (acc ++ [newStop'])
-- <check_if_route_stops_connected> ::= "check_if_route_stops_connected(" <route_id> ")" -- atleast n-1 previous connections and n-1 next connections
checkIfRouteStopsConnected :: State -> Query -> Either String (Bool, State)
checkIfRouteStopsConnected state query' =
  case query' of
    (CheckIfRouteStopsConnected routeId) ->
      let
        routeStops = getRouteStops routeId (stops state)
        in case routeStops of
          Left e1 -> Left e1
          Right foundRouteStops ->
            let
              connected = checkIfRouteStopsConnectedData foundRouteStops 0 0 routeId
              in case connected of
                Left e2 -> Left e2
                Right (prevCount, nextCount) ->
                  let
                    in if prevCount >= (length foundRouteStops - 1) && nextCount >= (length foundRouteStops - 1)
                      then Right (True, state)
                      else Right (False, state)
    _ -> Left "Not a check if route stops connected query"
checkIfRouteStopsConnectedData :: [Stop] -> Int -> Int -> RouteId -> Either String (Int, Int)
checkIfRouteStopsConnectedData [] prevCount nextCount rid = Right (prevCount, nextCount)
checkIfRouteStopsConnectedData [_] _ _ _ = Right (1, 1)
checkIfRouteStopsConnectedData (h:t) prevCount nextCount rid =
  let
    Stop _ _ _ nextStop prevStops _ = h

    routeId' = getByExtractorFromArray rid (\(NextStop _ rid') -> rid') nextStop
    routeId'' = getByExtractorFromArray rid (\(PreviousStop _ rid') -> rid') prevStops
    in case routeId' of
      Left e1 -> Left e1
      Right routeId1 ->
        case routeId'' of
          Left e2 -> Left e2
          Right routeId2 ->
            let
              routeId1' = case routeId1 of (NextStop _ rid'') -> rid''
              routeId2' = case routeId2 of (PreviousStop _ rid'') -> rid''
              in if routeId1' == rid && routeId2' == rid
                then checkIfRouteStopsConnectedData t (prevCount + 1) (nextCount + 1) rid
                else
                  if routeId1' == rid
                    then checkIfRouteStopsConnectedData t prevCount (nextCount + 1) rid
                    else
                      if routeId2' == rid
                        then checkIfRouteStopsConnectedData t (prevCount + 1) nextCount rid
                        else checkIfRouteStopsConnectedData t prevCount nextCount rid
-- <validate_trip> ::= "validate_trip(" <trip> ")" -- all stops and paths are connected
validateTrip :: State -> Query -> Either String (Bool, State)
validateTrip state query' =
  case query' of
    (ValidateTrip tripId) ->
      let
        tripFromQuery = parseQueryTripData tripId state
        in case tripFromQuery of
          Left e1 -> Left e1
          Right (new@(Trip tid' _ _), state'') ->
            let
              trip = getByExtractorFromArray tid' (\(Trip tid _ _) -> tid) (trips state'')
              in case trip of
                Left e1 -> Left e1
                Right foundTrip@(Trip _ _ stopOrPathList) ->
                  let
                    connected = validateTripData stopOrPathList
                    in case connected of
                      Left e2 -> Left e2
                      Right connected' ->
                        let
                          in if connected' then Right (True, state'')
                          else Right (False, state'')
    _ -> Left "Not a validate trip query"
validateTripData :: [StopOrPath] -> Either String Bool
validateTripData [] = Right True
validateTripData input = validateTripData' input
  where
    validateTripData' [] = Right True
    validateTripData' [stopOrPath] = Right True
    validateTripData' s@(h:t) =
      let
        validateForwards = validateForwardsData h t
        in case validateForwards of
          Left e1 -> Left e1
          Right v1 ->
            let
              backwardsList = reverse s
              validateBackwards = initBackwardsValidation backwardsList
              in case validateBackwards of
                Left e2 -> Left e2
                Right v2 -> Right v2

      where
        initBackwardsValidation [] = Right True
        initBackwardsValidation [stopOrPath] = Right True
        initBackwardsValidation s@(h:t) =
          let
            validateBackwards = validateBackwardsData h t
            in case validateBackwards of
              Left e1 -> Left e1
              Right v1 -> Right v1

        validateBackwardsData _ [] = Right True
        validateBackwardsData stopOrPath [] = Right True
        validateBackwardsData stopOrPath s@(h:t) =
          let
            connected = connectBackwards stopOrPath h
            in if connected
              then validateBackwardsData h t
              else Left "Trip stops or paths are not connected"


        validateForwardsData _ [] = Right True
        validateForwardsData stopOrPath [] = Right True
        validateForwardsData stopOrPath (h:t) =
          let
            connected = connectedForwards stopOrPath h
            in if connected
              then validateForwardsData h t
              else Left "Trip stops or paths are not connected"
-- <cleanup_trip> ::= "cleanup_trip(" <trip> ")"
cleanupTrip :: State -> Query -> Either String State
cleanupTrip state query' =
  case query' of
    (CleanupTrip tripId) -> -- FIXME
      let
        tripFromQuery = parseQueryTripData tripId state
        in case tripFromQuery of
          Left e1 -> Left e1
          Right (new@(Trip tid' _ _), state'''') ->
            let
              trip = getByExtractorFromArray tid' (\(Trip tid _ _) -> tid) (trips state'''')
              in case trip of
                Left e1 -> Left e1
                Right foundTrip@(Trip _ _ stopOrPathList) -> -- stopOrPathList FIXME not same type
                  let
                    cleaned = cleanupTripData stopOrPathList
                    in case cleaned of
                      Left e2 -> Left e2
                      Right cleaned' ->
                        let
                          updatedTrip = Trip tid' (case foundTrip of Trip _ name _ -> name) cleaned'
                          in case updateTrip updatedTrip state'''' of
                            Left e3 -> Left e3
                            Right newState -> Right newState
    _ -> Left "Not a cleanup trip query"
-- basically add one element and check if it is valid, if yes continue adding if no, then just return the valid part
cleanupTripData :: [StopOrPath] -> Either String [StopOrPath]
cleanupTripData [] = Right []
cleanupTripData [stopOrPath] = Right [stopOrPath]
cleanupTripData input = cleanUpTripData input []
  where
    cleanUpTripData [] acc = Right acc
    cleanUpTripData [stopOrPath] acc = Right (acc ++ [stopOrPath])
    cleanUpTripData s@(h:t) acc =
      let
        connected = validateTripData s
        in case connected of
          Left e1 -> Right acc
          Right _ -> cleanUpTripData t (acc ++ [h])
-- <trip_distance> ::= "trip_distance(" <trip> ")" -- sum of path lengths and distances between stop poitntssnsns
tripDistance :: State -> Query -> Either String (Float, State)
tripDistance state query' =
  case query' of
    (TripDistance tripId) ->
      let
        tripFromQueryTrip = parseQueryTripData tripId state
        in case tripFromQueryTrip of
          Left e1 -> Left e1
          Right (trip@(Trip tid' _ _), state') -> 
            let
              trip = getByExtractorFromArray tid' (\(Trip tid _ _) -> tid) (trips state')
              in case trip of
                Left e1 -> Left e1
                Right foundTrip@(Trip _ _ stopOrPathList) ->
                  let
                    distance = tripDistanceData stopOrPathList
                    in case distance of
                      Left e2 -> Left e2
                      Right distance' -> Right (distance', state')
    _ -> Left "Not a trip distance query"
stopOrPathDistance :: StopOrPath -> StopOrPath -> Either String Float
stopOrPathDistance sop1 sop2 =
  case (sop1, sop2) of
    (Stop' (Stop _ _ point1 _ _ _), Stop' (Stop _ _ point2 _ _ _)) ->
      let distance = distanceBetweenPoints point1 point2
      in Right distance
    (Stop' (Stop _ _ point1 _ _ _), Path' (Path _ _ length' _ _)) ->
      let distance = case length' of PathLenght l -> l
      in Right distance
    (Path' (Path _ _ length' _ _), Stop' (Stop _ _ point1 _ _ _)) ->
      Right 0
    (Path' (Path _ _ length1 _ _), Path' (Path _ _ length2 _ _)) ->
      let distance = case length2 of PathLenght l -> l
      in Right distance
    _ -> Left "Invalid stop or path"
tripDistanceData :: [StopOrPath] -> Either String Float
tripDistanceData [] = Right 0
tripDistanceData [stopOrPath] = Right 0
tripDistanceData input = 
  let
    valid = validateTripData input
    in case valid of
      Left e1 -> Left e1
      Right _ -> tripDistanceData' input 0
      
  where
    tripDistanceData' [] acc = Right acc
    tripDistanceData' [stopOrPath] acc = Right acc
    tripDistanceData' s@(h:t) acc =
      let
        distance = stopOrPathDistance h (head t)
        in case distance of
          Left e1 -> Left e1
          Right distance' -> tripDistanceData' t (acc + distance')

-- <join_two_routes> ::= "join_two_routes(" <route> ", " <route> ", " <new_route_id> ", " <new_name> ")"
joinTwoRoutes :: State -> Query -> Either String (Route, State)
joinTwoRoutes state query' =
  case query' of
    (JoinTwoRoutes route1 route2 newRouteId newName) ->
      let
        routeFromQuery = parseQueryRouteData route1 state
        in case routeFromQuery of
          Left e1 -> Left e1
          Right (r1@(Route rid1 _ _), state') ->
            let
              routeFromQuery2 = parseQueryRouteData route2 state'
              in case routeFromQuery2 of
                Left e1 -> Left e1
                Right (r2@(Route rid2 _ _), state'') ->
                  let
                    newRoute = joinTwoRoutesData (routes state'') (stops state'') rid1 rid2 newRouteId newName
                    in case newRoute of
                      Left e1 -> Left e1
                      Right (route, stops') ->
                        let
                          updateStops = updateOrAddStops stops' state''
                          in case updateStops of
                            Left e2 -> Left e2
                            Right newState ->
                              let
                                addRoute' = addRoute route newState
                                in case addRoute' of
                                  Left e3 -> Left e3
                                  Right finalState -> Right (route, finalState)
    _ -> Left "Not a join two routes query"
joinTwoRoutesData :: [Route] ->  [Stop] -> RouteId -> RouteId -> RouteId -> Name -> Either String (Route, [Stop])
joinTwoRoutesData routes' stops' v1 v2 v3 v4 =
  let
    route1' = getByExtractorFromArray v1 (\(Route rid _ _) -> rid) routes'
    in case route1' of
      Left e5 -> Left e5
      Right foundRoute1@(Route _ _ stops1) ->
        let
          route2' = getByExtractorFromArray v2 (\(Route rid _ _) -> rid) routes'
          in case route2' of
            Left e6 -> Left e6
            Right foundRoute2@(Route _ _ stops2) ->
              let
                newRouteStops = stops1 ++ stops2
                newRoute = Route v3 v4 newRouteStops
                firstRouteStops = getRouteStops v1 stops'
                in case firstRouteStops of
                  Left e7 -> Left e7
                  Right firstRouteStops' ->
                    let
                      secondRouteStops = getRouteStops v2 stops'
                      in case secondRouteStops of
                        Left e8 -> Left e8
                        Right secondRouteStops' ->
                          let
                            allStopsCombined = firstRouteStops' ++ secondRouteStops'
                            assigned = assignStopsToRoute allStopsCombined v3
                            in case assigned of
                              Left e9 -> Left e9
                              Right foundStops -> Right (newRoute, foundStops)
-- <join_two_trips> ::= "join_two_trips(" <trip> ", " <trip> ", " <new_trip_id> ", " <new_name> ")"
joinTwoTrips :: State -> Query -> Either String (Trip, State)
joinTwoTrips state query' =
  case query' of
    (JoinTwoTrips trip1 trip2 newTripId newName) ->
      let
        tripFromQuery1 = parseQueryTripData trip1 state
        in case tripFromQuery1 of
          Left e1 -> Left e1
          Right (t1@(Trip tid1 _ stopOrPathList1), state') ->
            let
              tripFromQuery2 = parseQueryTripData trip2 state'
              in case tripFromQuery2 of
                Left e1 -> Left e1
                Right (t2@(Trip tid2 _ stopOrPathList2), state'') ->
                  let
                    newTrip = Trip newTripId newName (stopOrPathList1 ++ stopOrPathList2)
                    addTrip' = addTrip newTrip state''
                    in case addTrip' of
                      Left e4 -> Left e4
                      Right newState -> Right (newTrip, newState)
    _ -> Left "Not a join two trips query"

-- <join_two_routes_at_stop> ::= "join_two_routes_at_stop(" <route> ", " <route> ", " <stop_or_creat_or_nextprev> ", " <new_route_id> ", " <new_name> ")" -- min distance order
joinTwoRoutesAtStop :: State -> Query -> Either String (Route, State)
joinTwoRoutesAtStop state query' =
  case query' of
    (JoinTwoRoutesAtStop route1' route2' stopOrCreateNew newRouteId newName) ->
      let
        routeFromQuery1 = parseQueryRouteData route1' state
        in case routeFromQuery1 of
          Left e1 -> Left e1
          Right (r@(Route rid _ _), state') ->
            let
              routeFromQuery2 = parseQueryRouteData route2' state'
              in case routeFromQuery2 of
                Left e1 -> Left e1
                Right (r2@(Route rid2 _ _), state'') ->
                  let
                    newRoute = (joinTwoRoutesData (routes state'') (stops state'') rid rid2 newRouteId newName)
                    in case newRoute of
                      Left e1 -> Left e1
                      Right (route, stops') ->
                        let
                          parseStopOrCreateNew = parseQueryStopOrCreatOrNextPrevData stopOrCreateNew state''
                          in case parseStopOrCreateNew of
                            Left e99 -> Left e99
                            Right (get@(Stop sid name point nexts prevs rout), state'''') ->
                              let
                                assignStop = assignStopsToRoute [get] newRouteId
                                in case assignStop of
                                  Left e2 -> Left e2
                                  Right [] -> Left "Stop not assigned to route"
                                  Right (foundStops:t) -> 
                                    let
                                      addStopToRoute = addStopIdToRoute sid route -- get the stop from the state  
                                      in case addStopToRoute of
                                        Left e3 -> Left e3
                                        Right updatedRoute ->  
                                          let
                                            addRoute' = addRoute updatedRoute state''''
                                            in case addRoute' of
                                              Left e4 -> Left e4
                                              Right almostFinalState -> 
                                                let
                                                  updateStop' = updateStop foundStops almostFinalState
                                                  in case updateStop' of
                                                    Left e5 -> Left e5
                                                    Right finalState -> Right (updatedRoute, finalState)
                                      
                                        
    _ -> Left "Not a join two routes at stop query"





-- QUERY HELPERS
parseQueryRouteData :: QueryRoute -> State -> Either String (Route, State) -- create new route if needed and return the route id
parseQueryRouteData query' state =
  case query' of
    (Route' routeId) ->
      let
        route = getByExtractorFromArray routeId (\(Route rid _ _) -> rid) (routes state)
        in case route of
          Left e1 -> Left e1
          Right foundRoute -> Right (foundRoute, state)
    (CreateRoute' query'') ->
      let
        createRoute' = createRoute state query''
        in case createRoute' of
          Left e1 -> Left e1
          Right (route, newState) -> Right (route, newState)
    (JoinTwoRoutes' query'') ->
      let
        joinTwoRoutes' = joinTwoRoutes state query''
        in case joinTwoRoutes' of
          Left e1 -> Left e1
          Right (route, newState) -> Right (route, newState)
    (JoinTwoRoutesAtStop' query'') ->
      let
        joinTwoRoutesAtStop' = joinTwoRoutesAtStop state query''
        in case joinTwoRoutesAtStop' of
          Left e1 -> Left e1
          Right newState -> Right newState
-- QUERY HELPERS
parseQueryStopOrPathOrCreateData :: QueryStopOrPathOrCreate -> State -> Either String (StopOrPath, State)
parseQueryStopOrPathOrCreateData query' state =
  case query' of
    (QueryStopOrPath' query'') ->
      let
        queryStopOrPath' = parseQueryStopOrPathData query'' state
        in case queryStopOrPath' of
          Left e1 -> Left e1
          Right (stopOrPath, newState) -> Right (stopOrPath, newState)
    (CreateStop' query'') ->
      let
        createStop' = createStop state query''
        in case createStop' of
          Left e1 -> Left e1
          Right (stop, newState) -> Right (Stop' stop, newState)
    (FindNextStop' query'') ->
      let
        findNextStop' = findNextStop state query''
        in case findNextStop' of
          Left e1 -> Left e1
          Right (stopId, newState) ->
            let
              stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops newState)
              in case stop of
                Left e2 -> Left e2
                Right foundStop -> Right (Stop' foundStop, newState)
    (FindPreviousStop' query'') ->
      let
        findPreviousStop' = findPreviousStop state query''
        in case findPreviousStop' of
          Left e1 -> Left e1
          Right (stopId, newState) ->
            let
              stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops newState)
              in case stop of
                Left e2 -> Left e2
                Right foundStop -> Right (Stop' foundStop, newState)
-- QUERY HELPERS
parseQueryStopOrPathData :: QueryStopOrPath -> State -> Either String (StopOrPath, State)
parseQueryStopOrPathData query' state =
  case query' of
    (StopId' stopId) ->
      let
        stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
        in case stop of
          Left e1 -> Left e1
          Right foundStop -> Right (Stop' foundStop, state)
    (PathId' pathId) ->
      let
        path = getByExtractorFromArray pathId (\(Path pid _ _ _ _) -> pid) (paths state)
        in case path of
          Left e1 -> Left e1
          Right foundPath -> Right (Path' foundPath, state)
    _ -> Left "Not a stop or path query"
-- QUERY HELPERS
parseQueryStopOrCreatOrNextPrevData :: QueryStopOrCreatOrNextPrev -> State -> Either String (Stop, State)
parseQueryStopOrCreatOrNextPrevData query' state =
  case query' of
    (QueryStopOrCreatOrNextPrevStop stopId) ->
      let
        stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops state)
        in case stop of
          Left e1 -> Left e1
          Right foundStop -> Right (foundStop, state)
    (QueryStopOrCreatOrNextPrevCreateStop query'') ->
      let
        createStop' = createStop state query''
        in case createStop' of
          Left e1 -> Left e1
          Right (stop, newState) -> Right (stop, newState)
    (QueryStopOrCreatOrNextPrevFindNextStop query'') ->
      let
        findNextStop' = findNextStop state query''
        in case findNextStop' of
          Left e1 -> Left e1
          Right (stopId, newState) ->
            let
              stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops newState)
              in case stop of
                Left e2 -> Left e2
                Right foundStop -> Right (foundStop, newState)
    (QueryStopOrCreatOrNextPrevFindPreviousStop query'') ->
      let
        findPreviousStop' = findPreviousStop state query''
        in case findPreviousStop' of
          Left e1 -> Left e1
          Right (stopId, newState) ->
            let
              stop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops newState)
              in case stop of
                Left e2 -> Left e2
                Right foundStop -> Right (foundStop, newState)
-- QUERY HELPERS
parseQueryTripData :: QueryTrip -> State -> Either String  (Trip, State)
parseQueryTripData query' state =
  case query' of
    (Trip' tripId) ->
      let
        trip = getByExtractorFromArray tripId (\(Trip tid _ _) -> tid) (trips state)
        in case trip of
          Left e1 -> Left e1
          Right foundTrip -> Right (foundTrip, state)
    (CreateTrip' query'') ->
      let
        createTrip' = createTrip state query''
        in case createTrip' of
          Left e1 -> Left e1
          Right (trip, newState) -> Right (trip, newState)
-- QUERY HELPERS
parseQueryStopOrPathOrCreatListData :: [QueryStopOrPathOrCreate] -> State -> Either String ([StopOrPath], State)
parseQueryStopOrPathOrCreatListData [] state = Right ([], state)
parseQueryStopOrPathOrCreatListData (h:t) state =
  let
    parse = parseQueryStopOrPathOrCreateData h state
    in case parse of
      Left e1 -> Left e1
      Right (stopOrPath, newState) ->
        let
          parse' = parseQueryStopOrPathOrCreatListData t newState
          in case parse' of
            Left e2 -> Left e2
            Right (stopOrPathList, newState') -> Right (stopOrPath : stopOrPathList, newState')





        


-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State {
  stops :: [Stop],
  routes :: [Route],
  paths :: [Path],
  trips :: [Trip]
}

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State [] [] [] []



-- STATE HELPER FUNCTIONS
addTrip :: Trip -> State -> Either String State
addTrip trip state =
  let
    trips' = addToArray trip (trips state)
    in case trips' of
      Left e1 -> Left e1
      Right v1 -> Right state {trips = v1}
getTrip :: TripId -> State -> Either String Trip
getTrip targetId state = getTrip' targetId (trips state)
  where
    getTrip' _ [] = Left "Trip not found"
    getTrip' targetId' (Trip tid name stopOrPath : rest)
      | targetId' == tid = Right (Trip tid name stopOrPath)
      | otherwise       = getTrip' targetId' rest
updateTrip :: Trip -> State -> Either String State
updateTrip trip state =
  let
    target = getTrip (case trip of Trip tid _ _ -> tid) state
    in case target of
      Left _ -> Left "Trip not found"
      Right e1 ->
        let
          trips' = updateArray e1 trip (trips state)
          in case trips' of
            Left e1' -> Left e1'
            Right v1 -> Right state {trips = v1}
updateOrAddTrips :: [Trip] -> State -> Either String State
updateOrAddTrips [] state = Right state
updateOrAddTrips (h:t) state =
  let
    update = updateTrip h state
    in case update of
      Left _ ->
        let
          add = addTrip h state
          in case add of
            Left e1 -> Left e1
            Right v1 -> updateOrAddTrips t v1
      Right v1 -> updateOrAddTrips t v1
deleteTrip :: TripId -> State -> Either String State
deleteTrip targetId state =
  let
    target = getTrip targetId state
    in case target of
      Left _ -> Left "Trip not found"
      Right e1 ->
        let
          trips' = deleteFromArray e1 (trips state)
          in case trips' of
            Left e1' -> Left e1'
            Right v1 -> Right state {trips = v1}
tripInState :: Trip -> State -> Bool
tripInState trip state = elementInArray trip (trips state)
-- STATE HELPER FUNCTIONS
addStop :: Stop -> State -> Either String State
addStop stop state =
  let
    stops' = addToArray stop (stops state)
    in case stops' of
      Left e1 -> Left e1
      Right v1 -> Right state {stops = v1}
getStop :: StopId -> State -> Either String Stop
getStop targetId state = getStop' targetId (stops state)
  where
    getStop' _ [] = Left "Stop not found"
    getStop' targetId' (Stop sid name point prevStops nextStops routes : rest)
      | targetId' == sid = Right (Stop sid name point prevStops nextStops routes)
      | otherwise       = getStop' targetId' rest
updateStop :: Stop -> State -> Either String State
updateStop stop state =
  let
    target = getStop (case stop of Stop sid _ _ _ _ _ -> sid) state
    in case target of
      Left _ -> Left "Stop not found"
      Right e1 ->
        let
          stops' = updateArray e1 stop (stops state)
          in case stops' of
            Left e1' -> Left e1'
            Right v1 -> Right state {stops = v1}
updateOrAddStops :: [Stop] -> State -> Either String State
updateOrAddStops [] state = Right state
updateOrAddStops (h:t) state =
  let
    update = updateStop h state
    in case update of
      Left _ ->
        let
          add = addStop h state
          in case add of
            Left e1 -> Left e1
            Right v1 -> updateOrAddStops t v1
      Right v1 -> updateOrAddStops t v1
deleteStop :: StopId -> State -> Either String State
deleteStop targetId state =
  let
    stop = getStop targetId state
    in case stop of
      Left _ -> Left "Stop not found"
      Right e1 ->
        let
          stops' = deleteFromArray e1 (stops state)
          in case stops' of
            Left e1' -> Left e1'
            Right v1 -> Right state {stops = v1}
stopInState :: Stop -> State -> Bool
stopInState stop state = elementInArray stop (stops state)
-- STATE HELPER FUNCTIONS
addRoute :: Route -> State -> Either String State
addRoute route state =
  let
    routes' = addToArray route (routes state)
    in case routes' of
      Left e1 -> Left e1
      Right v1 -> Right state {routes = v1}
getRoute :: RouteId -> State -> Either String Route
getRoute targetId state = getRoute' targetId (routes state)
  where
    getRoute' _ [] = Left "Route not found"
    getRoute' targetId' (Route rid name stopIds : rest)
      | targetId' == rid = Right (Route rid name stopIds)
      | otherwise       = getRoute' targetId' rest
updateRoute :: Route -> State -> Either String State
updateRoute route state =
  let
    target = getRoute (case route of Route rid _ _ -> rid) state
    in case target of
      Left _ -> Left "Route not found"
      Right e1 ->
        let
          routes' = updateArray e1 route (routes state)
          in case routes' of
            Left e1' -> Left e1'
            Right v1 -> Right state {routes = v1}
updateOrAddRoutes :: [Route] -> State -> Either String State
updateOrAddRoutes [] state = Right state
updateOrAddRoutes (h:t) state =
  let
    update = updateRoute h state
    in case update of
      Left _ ->
        let
          add = addRoute h state
          in case add of
            Left e1 -> Left e1
            Right v1 -> updateOrAddRoutes t v1
      Right v1 -> updateOrAddRoutes t v1
deleteRoute :: RouteId -> State -> Either String State
deleteRoute targetId state =
  let
    route = getRoute targetId state
    in case route of
      Left _ -> Left "Route not found"
      Right e1 ->
        let
          routes' = deleteFromArray e1 (routes state)
          in case routes' of
            Left e1' -> Left e1'
            Right v1 -> Right state {routes = v1}
routeInState :: Route -> State -> Bool
routeInState route state = elementInArray route (routes state)
-- STATE HELPER FUNCTIONS
addPath :: Path -> State -> Either String State
addPath path state =
  let
    paths' = addToArray path (paths state)
    in case paths' of
      Left e1 -> Left e1
      Right v1 -> Right state {paths = v1}
getPath :: PathId -> State -> Either String Path
getPath targetId state = getPath' targetId (paths state)
  where
    getPath' _ [] = Left "Path not found"
    getPath' targetId' (Path pid name pathLenght startId endId : rest)
      | targetId' == pid = Right (Path pid name pathLenght startId endId)
      | otherwise       = getPath' targetId' rest
updatePath :: Path -> State -> Either String State
updatePath path state =
  let
    target = getPath (case path of Path pid _ _ _ _ -> pid) state
    in case target of
      Left _ -> Left "Path not found"
      Right e1 ->
        let
          paths' = updateArray e1 path (paths state)
          in case paths' of
            Left e1' -> Left e1'
            Right v1 -> Right state {paths = v1}
updateOrAddPaths :: [Path] -> State -> Either String State
updateOrAddPaths [] state = Right state
updateOrAddPaths (h:t) state =
  let
    update = updatePath h state
    in case update of
      Left _ ->
        let
          add = addPath h state
          in case add of
            Left e1 -> Left e1
            Right v1 -> updateOrAddPaths t v1
      Right v1 -> updateOrAddPaths t v1
deletePath :: PathId -> State -> Either String State
deletePath targetId state =
  let
    path = getPath targetId state
    in case path of
      Left _ -> Left "Path not found"
      Right e1 ->
        let
          paths' = deleteFromArray e1 (paths state)
          in case paths' of
            Left e1' -> Left e1'
            Right v1 -> Right state {paths = v1}
pathInState :: Path -> State -> Bool
pathInState path state = elementInArray path (paths state)



-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query =
  case createStop state query of
    Right (stop, state') -> Right (Just ("Stop " ++ (case stop of Stop _ name _ _ _ _ -> show name) ++ " created"), state')
    Left e1 -> case (createRoute state query) of
      Right (route, state') -> Right (Just ("Route " ++ (case route of Route _ name _ -> show name) ++ " created"), state')
      Left e1 -> case (createTrip state query) of
        Right (trip, state') -> Right (Just ("Trip " ++ (case trip of Trip _ name _ -> show name) ++ " created"), state')
        Left e1 -> case (createPath state query) of
          Right (path, state') -> Right (Just ("Path " ++ (case path of Path _ name _ _ _ -> show name) ++ " created"), state')
          Left e1 ->  case (assignStopToRoute state query) of
              Right (stop, state') -> Right (Just ("Stop " ++ (case stop of Stop _ name _ _ _ _ -> show name) ++ " assigned to route."), state')
              Left e1 -> case (distanceBetweenStops state query) of
                Right (distance, state') -> Right (Just ("Distance between stops is " ++ show distance), state')
                Left e1 -> case (setNextStop state query) of
                  Right (state') -> Right (Just ("Next stop set"), state')
                  Left e1 -> case (setPreviousStop state query) of
                    Right (state') -> Right (Just ("Previous stop set"), state')
                    Left e1 -> case (findNextStop state query) of
                      Right (stopId, state') -> Right (Just ("Next stop found: " ++ show stopId), state')
                      Left e1 -> case (findPreviousStop state query) of
                        Right (stopId, state') -> Right (Just ("Previous stop found: " ++ show stopId), state')
                        Left e1 -> case (connectRouteStopsByMinDist state query) of
                          Right (state') -> Right (Just ("Route stops connected by min distance"), state')
                          Left e1 -> case (checkIfRouteStopsConnected state query) of
                            Right (connected, state') -> Right (Just ("Route stops connected: " ++ show connected), state')
                            Left e1 -> case (validateTrip state query) of
                              Right (connected, state') -> Right (Just ("Trip stops connected: " ++ show connected), state')
                              Left e1 -> case (cleanupTrip state query) of
                                Right state' -> Right (Just ("Trip cleaned up"), state')
                                Left e1 -> case (tripDistance state query) of
                                  Right (distance, state') -> Right (Just ("Trip distance: " ++ show distance), state')
                                  Left e1 -> case (joinTwoRoutes state query) of
                                    Right (route, state') -> Right (Just ("Routes joined"), state')
                                    Left e1 -> case (joinTwoTrips state query) of
                                      Right (trip, state') -> Right (Just ("Trips joined"), state')
                                      Left e1 -> case (joinTwoRoutesAtStop state query) of
                                        Right (route, state') -> Right (Just ("Routes joined at stop"), state')
                                        Left e1 -> Left "Invalid query"
                              
                            