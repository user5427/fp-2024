{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lib2
    ( Query(..),
    StopId(..), RouteId(..), PathId(..), Name(..), Point(..), QueryStopOrPath(..), QueryStopOrPathOrCreate(..), QueryStopOrCreatOrNextPrev(..),
    CoordX(..), CoordY(..), PathLenght(..), TripId(..), QueryTrip(..), QueryRoute(..),
    Stop(..), Route(..), Path(..), Trip(..), NextStop(..), PreviousStop(..), 
    parseQuery,
    MyState(..),
    emptyState,
    stateTransition,
    parseExact,
    parse,
    parseFloat,
    parseInteger
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import Data.Functor.Classes (eq1)
import Data.List (delete)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Lessons.Lesson08 (Parser(Parser))

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
                (Left _, rest') -> lift $ put rest' >> return (reverse acc)
                (Right a, rest) -> do
                    lift $ put rest
                    many' p' (a:acc)

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

getByExtractorFromArray :: Eq a => a -> (b -> a) -> [b] -> Either String b
getByExtractorFromArray = getByIndexFromArray'
  where
    getByIndexFromArray' _ _ [] = Left "Element not found"
    getByIndexFromArray' value' extractor' (h:t) =
      if value' == extractor' h
      then Right h
      else getByIndexFromArray' value' extractor' t
getByIndexFromArray :: Eq a => Int -> [a] -> Either String a
getByIndexFromArray index arr = getByIndexFromArray' index arr 0
  where
    getByIndexFromArray' _ [] _ = Left "Element not found"
    getByIndexFromArray' index' (h:t) i = if index' == i then Right h else getByIndexFromArray' index' t (i + 1)
getIndexFromArray :: Eq a => a -> [a] -> Either String Int
getIndexFromArray target arr = getIndexFromArray' target arr 0
  where
    getIndexFromArray' _ [] _ = Left "Element not found"
    getIndexFromArray' target' (h:t) index = if target' == h then Right index else getIndexFromArray' target' t (index + 1)
addToArray :: Eq a => a -> [a] -> Either String [a]
addToArray elem' array = addToArray' [] elem' array
  where
    addToArray' acc target [] = Right (acc ++ [target])
    addToArray' acc target (h:t) = if target == h then Left "Element already exists" else addToArray' (acc ++ [h]) target t
updateArray :: Eq a => a -> a -> [a] ->  Either String [a]
updateArray = updateArray' []
  where
    updateArray' _ _ _ [] = Left "Element not found"
    updateArray' acc target new (h:t) = if target == h then Right (acc ++ [new] ++ t) else updateArray' (acc ++ [h]) target new t
deleteFromArray :: Eq a => a -> [a] -> Either String [a]
deleteFromArray = deleteFromArray' []
  where
    deleteFromArray' _ _ [] = Left "Element not found"
    deleteFromArray' acc target (h:t) = if target == h then Right (acc ++ t) else deleteFromArray' (acc ++ [h]) target t
elementInArray :: Eq a => a -> [a] -> Bool
elementInArray _ [] = False
elementInArray target (h:t) = if target == h then True else elementInArray target t
-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query
  = CreateStop StopId Name Point -- +
  | CreateRoute RouteId Name [QueryStopOrCreatOrNextPrev] -- +
  | CreatePath PathId Name PathLenght StopId StopId -- +
  | CreateTrip TripId Name [QueryStopOrPathOrCreate] -- +
  | JoinTwoTrips QueryTrip QueryTrip TripId Name -- +
  | JoinTwoRoutes QueryRoute QueryRoute RouteId Name -- +
  | JoinTwoRoutesAtStop QueryRoute QueryRoute QueryStopOrCreatOrNextPrev RouteId Name --
  | CleanupTrip QueryTrip -- +
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
  | View




-- | The instances are needed basically for tests
instance Eq Query where
  (==) aQ bQ = 
    case (aQ, bQ) of
      (CreateStop a1 a2 a3, CreateStop b1 b2 b3) -> a1 == b1 && a2 == b2 && a3 == b3
      (CreateRoute a1 a2 a3, CreateRoute b1 b2 b3) -> a1 == b1 && a2 == b2 && a3 == b3
      (CreatePath a1 a2 a3 a4 a5, CreatePath b1 b2 b3 b4 b5) -> a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5
      (CreateTrip a1 a2 a3, CreateTrip b1 b2 b3) -> a1 == b1 && a2 == b2 && a3 == b3
      (JoinTwoTrips a1 a2 a3 a4, JoinTwoTrips b1 b2 b3 b4) -> a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4
      (JoinTwoRoutes a1 a2 a3 a4, JoinTwoRoutes b1 b2 b3 b4) -> a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4
      (JoinTwoRoutesAtStop a1 a2 a3 a4 a5, JoinTwoRoutesAtStop b1 b2 b3 b4 b5) -> a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5
      (CleanupTrip a1, CleanupTrip b1) -> a1 == b1
      (ValidateTrip a1, ValidateTrip b1) -> a1 == b1
      (FindNextStop a1 a2, FindNextStop b1 b2) -> a1 == b1 && a2 == b2
      (FindPreviousStop a1 a2, FindPreviousStop b1 b2) -> a1 == b1 && a2 == b2
      (TripDistance a1, TripDistance b1) -> a1 == b1
      (SetNextStop a1 a2 a3, SetNextStop b1 b2 b3) -> a1 == b1 && a2 == b2 && a3 == b3
      (SetPreviousStop a1 a2 a3, SetPreviousStop b1 b2 b3) -> a1 == b1 && a2 == b2 && a3 == b3
      (ConnectRouteStopsByMinDistance a1, ConnectRouteStopsByMinDistance b1) -> a1 == b1
      (CheckIfRouteStopsConnected a1, CheckIfRouteStopsConnected b1) -> a1 == b1
      (DistanceBetweenStops a1 a2, DistanceBetweenStops b1 b2) -> a1 == b1 && a2 == b2
      (AssignStopToRoute a1 a2, AssignStopToRoute b1 b2) -> a1 == b1 && a2 == b2
      _ -> False

instance Show Query where
  show query = 
    case query of
      CreateStop stopId name point -> "CreateStop " ++ show stopId ++ " " ++ show name ++ " " ++ show point
      CreateRoute routeId name stopIds -> "CreateRoute " ++ show routeId ++ " " ++ show name ++ " " ++ show stopIds
      CreatePath pathId name pathLenght stopId1 stopId2 -> "CreatePath " ++ show pathId ++ " " ++ show name ++ " " ++ show pathLenght ++ " " ++ show stopId1 ++ " " ++ show stopId2
      CreateTrip tripId name stops' -> "CreateTrip " ++ show tripId ++ " " ++ show name ++ " " ++ show stops'
      JoinTwoTrips trip1 trip2 newTripId newName -> "JoinTwoTrips " ++ show trip1 ++ " " ++ show trip2 ++ " " ++ show newTripId ++ " " ++ show newName
      JoinTwoRoutes route1 route2 newRouteId newName -> "JoinTwoRoutes " ++ show route1 ++ " " ++ show route2 ++ " " ++ show newRouteId ++ " " ++ show newName
      JoinTwoRoutesAtStop route1 route2 stopOrCreatOrNextPrev newRouteId newName -> "JoinTwoRoutesAtStop " ++ show route1 ++ " " ++ show route2 ++ " " ++ show stopOrCreatOrNextPrev ++ " " ++ show newRouteId ++ " " ++ show newName
      CleanupTrip trip -> "CleanupTrip " ++ show trip
      ValidateTrip trip -> "ValidateTrip " ++ show trip
      FindNextStop stopId routeId -> "FindNextStop " ++ show stopId ++ " " ++ show routeId
      FindPreviousStop stopId routeId -> "FindPreviousStop " ++ show stopId ++ " " ++ show routeId
      TripDistance trip -> "TripDistance " ++ show trip
      SetNextStop stopId routeId nextStopId -> "SetNextStop " ++ show stopId ++ " " ++ show routeId ++ " " ++ show nextStopId
      SetPreviousStop stopId routeId previousStopId -> "SetPreviousStop " ++ show stopId ++ " " ++ show routeId ++ " " ++ show previousStopId
      ConnectRouteStopsByMinDistance routeId -> "ConnectRouteStopsByMinDistance " ++ show routeId
      CheckIfRouteStopsConnected routeId -> "CheckIfRouteStopsConnected " ++ show routeId
      DistanceBetweenStops stopId1 stopId2 -> "DistanceBetweenStops " ++ show stopId1 ++ " " ++ show stopId2
      AssignStopToRoute stopId routeId -> "AssignStopToRoute " ++ show stopId ++ " " ++ show routeId
      View -> "View"

-- HELPER FUNCTIONS
-- >>> parseExact "a" "a"
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
-- <pos>
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
-- <float>
parseFloat :: Parser2 Float
parseFloat = do
    input <- lift get
    case parse (parseExact "-") input of
        (Left _, _) -> parsePositiveFloat
        (Right _, rest) -> do
            case parse parsePositiveFloat rest of
                (Left _, _) -> throwE "not a float"
                (Right numner, rest') -> lift $ put rest' >> return (-numner)
                
-- <trip_id> ::= "T" <integer>
data TripId = TripId Char Int deriving (Show, Eq)
parseTripId :: Parser2 TripId
parseTripId = do
  _ <- parseChar 'T'
  tripId <- parseInteger
  return (TripId ('T') (fromIntegral tripId))

-- <route_id> ::= <integer>
data RouteId = RouteId Char Int deriving (Show, Eq)
parseRouteId :: Parser2 RouteId
parseRouteId = do
  _ <- parseChar 'R'
  routeId <- parseInteger
  return (RouteId ('R') (fromIntegral routeId))

-- <stop_id> ::= <integer>
data StopId = StopId Char Int deriving (Show, Eq)
parseStopId :: Parser2 StopId
parseStopId = do
  _ <- parseChar 'S'
  stopId <- parseInteger
  return (StopId ('S') (fromIntegral stopId))

-- <list_of_stop_ids> ::= <stop_id> "," <list_of_stop_ids> | <stop_id>
parseStopIdList :: Parser2 [StopId]
parseStopIdList = parseManySeperated parseStopId parseSeperator

-- <path_id> ::= <integer>
data PathId = PathId Char Int deriving (Show, Eq)
parsePathId :: Parser2 PathId
parsePathId = do
  _ <- parseChar 'P'
  pathId <- parseInteger
  return (PathId ('P') (fromIntegral pathId))
-- <path_length> ::= <float>
data PathLenght = PathLenght Float deriving (Show, Eq)
parsePathLenght :: Parser2 PathLenght
parsePathLenght = do
  pathLenght <- parseFloat
  return (PathLenght pathLenght)
-- <coord_x> ::= <float>
data CoordX = CoordX Float deriving (Show, Eq)
parseCoordX :: Parser2 CoordX
parseCoordX = do
  coordX <- parseFloat
  return (CoordX coordX)
-- <coord_y> ::= <float>
data CoordY = CoordY Float deriving (Show, Eq)
parseCoordY :: Parser2 CoordY
parseCoordY = do
  coordY <- parseFloat
  return (CoordY coordY)
-- <point> ::= <coord_x> ", " <coord_y>
data Point = Point CoordX CoordY deriving (Show, Eq)
parsePoint :: Parser2 Point
parsePoint = do
  coordX <- parseCoordX
  _ <- parseSeperator
  coordY <- parseCoordY
  return (Point coordX coordY)
-- <name> ::= <string>
data Name = Name String deriving (Show, Eq)
parseName :: Parser2 Name
parseName = do
  name <- parseString
  return (Name name)
-- <stop_or_path> ::= <stop_id> | <path_id>
data QueryStopOrPath = StopId' StopId | PathId' PathId deriving (Show, Eq)
parseQueryStopOrPath :: Parser2 QueryStopOrPath
parseQueryStopOrPath = do
  input <- lift get
  case parse parseStopId input of
    (Right stopId, rest) -> do
      lift $ put rest >> return (StopId' stopId)
    (Left _, _) -> do
      pathId <- parsePathId
      return (PathId' pathId)

-- + <list_of_stops_and_paths> ::= <stop_or_path> "," <list_of_stops_and_paths> | <stop_or_path> 
parseQueryStopOrPathList :: Parser2 [QueryStopOrPath]
parseQueryStopOrPathList = parseManySeperated parseQueryStopOrPath parseSeperator
data QueryStopOrPathOrCreate = QueryStopOrPath' QueryStopOrPath | CreateStop' Query | FindNextStop' Query | FindPreviousStop' Query deriving (Show, Eq)
-- <stop_or_path_or_creat> ::= <create_stop> | <stop_or_path> | <find_next_stop> | <find_previous_stop>
parseQueryStopOrPathOrCreat :: Parser2 QueryStopOrPathOrCreate
parseQueryStopOrPathOrCreat = do
  input <- lift get
  case parse parseQueryCreateStop input of
    (Right createStop', rest) -> do
      lift $ put rest
      return (CreateStop' createStop')
    (Left _, _) -> do
      case parse parseQueryStopOrPath input of
        (Right stopOrPath', rest) -> do
          lift $ put rest
          return (QueryStopOrPath' stopOrPath')
        (Left _, _) -> do
          case parse parseQueryFindNextStop input of
            (Right findNextStop', rest) -> do
              lift $ put rest
              return (FindNextStop' findNextStop')
            (Left _, _) -> do
              case parse parseQueryFindPreviousStop input of
                (Right findPreviousStop', rest) -> do
                  lift $ put rest
                  return (FindPreviousStop' findPreviousStop')
                (Left _, _) -> throwE "Cannot parse QueryStopOrPathOrCreate"
-- <list_of_stops_paths_creat> ::= <stop_or_path_or_creat> "," <list_of_stops_paths_creat> | <stop_or_path_or_creat>
parseQueryStopOrPathOrCreatList :: Parser2 [QueryStopOrPathOrCreate]
parseQueryStopOrPathOrCreatList = parseManySeperated parseQueryStopOrPathOrCreat parseSeperator
-- <stop_or_creat_or_nextprev> ::= <create_stop> | <stop_id> | <find_next_stop> | <find_previous_stop>
data QueryStopOrCreatOrNextPrev = QueryStopOrCreatOrNextPrevStop StopId | QueryStopOrCreatOrNextPrevCreateStop Query | QueryStopOrCreatOrNextPrevFindNextStop Query | QueryStopOrCreatOrNextPrevFindPreviousStop Query deriving (Show, Eq)
parseQueryStopOrCreatOrNextPrev :: Parser2 QueryStopOrCreatOrNextPrev
parseQueryStopOrCreatOrNextPrev = do
  input <- lift get
  case parse parseQueryCreateStop input of
    (Right createStop', rest) -> do
      lift $ put rest
      return (QueryStopOrCreatOrNextPrevCreateStop createStop')
    (Left _, _) -> do
      case parse parseStopId input of
        (Right stopId, rest) -> do
          lift $ put rest
          return (QueryStopOrCreatOrNextPrevStop stopId)
        (Left _, _) -> do
          case parse parseQueryFindNextStop input of
            (Right findNextStop', rest) -> do
              lift $ put rest
              return (QueryStopOrCreatOrNextPrevFindNextStop findNextStop')
            (Left _, _) -> do
              case parse parseQueryFindPreviousStop input of
                (Right findPreviousStop', rest) -> do
                  lift $ put rest
                  return (QueryStopOrCreatOrNextPrevFindPreviousStop findPreviousStop')
                (Left _, _) -> throwE "Cannot parse QueryStopOrCreatOrNextPrev"


-- <list_of_stop_or_creat_or_nextprev> ::= <stop_or_creat_or_nextprev> ", " <list_of_stop_or_creat_or_nextprev> | <stop_or_creat_or_nextprev>

parseQueryStopOrCreatOrNextPrevList :: Parser2 [QueryStopOrCreatOrNextPrev]
parseQueryStopOrCreatOrNextPrevList = parseManySeperated parseQueryStopOrCreatOrNextPrev parseSeperator
-- <trip> ::= <trip_id> | <create_trip>
data QueryTrip = Trip' TripId | CreateTrip' Query deriving (Show, Eq)
parseQueryTrip :: Parser2 QueryTrip
parseQueryTrip = do
  input <- lift get
  case parse parseTripId input of
    (Right tripId, rest) -> do
      lift $ put rest
      return (Trip' tripId)
    (Left _, _) -> do
      createTrip <- parseQueryCreateTrip
      return (CreateTrip' createTrip)
-- <route> ::= <route_id> | <create_route> | <join_two_routes> | <join_two_routes_at_stop> -- Route
data QueryRoute = Route' RouteId | CreateRoute' Query | JoinTwoRoutes' Query | JoinTwoRoutesAtStop' Query deriving (Show, Eq)
parseQueryRoute :: Parser2 QueryRoute
parseQueryRoute = do
  input <- lift get
  case parse parseRouteId input of
    (Right routeId, rest) -> do
      lift $ put rest
      return (Route' routeId)
    (Left _, _) -> do
      case parse parseQueryCreateRoute input of
        (Right createRoute, rest) -> do
          lift $ put rest
          return (CreateRoute' createRoute)
        (Left _, _) -> do
          case parse parseQueryJoinTwoRouter input of
            (Right joinTwoRoutes, rest) -> do
              lift $ put rest
              return (JoinTwoRoutes' joinTwoRoutes)
            (Left _, _) -> do
              case parse parseQueryJoinTwoRouterAtStop input of
                (Right joinTwoRoutesAtStop, rest) -> do
                  lift $ put rest
                  return (JoinTwoRoutesAtStop' joinTwoRoutesAtStop)
                (Left _, _) -> throwE "Cannot parse QueryRoute"
--
--  | Functions
-- \ /
-- <create_stop> ::= "create_stop(" <stop_id> ", " <name> ", " <point> ")"
parseQueryCreateStop :: Parser2 Query
parseQueryCreateStop = do
  _ <- parseExact "create_stop("
  stopId <- parseStopId
  _ <- parseSeperator
  name <- parseName
  _ <- parseSeperator
  point <- parsePoint
  _ <- parseExact ")"
  return (CreateStop stopId name point)

parseQueryViewState :: Parser2 Query
parseQueryViewState = do
  _ <- parseExact "VIEW"
  return View
-- <create_route> ::= "create_route(" <route_id> ", " <name> ", " <list_of_stop_ids> ")"
parseQueryCreateRoute :: Parser2 Query
parseQueryCreateRoute = do
  _ <- parseExact "create_route("
  routeId <- parseRouteId
  _ <- parseSeperator
  name <- parseName
  _ <- parseSeperator
  stopIds <- parseQueryStopOrCreatOrNextPrevList
  _ <- parseExact ")"
  return (CreateRoute routeId name stopIds)
-- <create_path> ::= "create_path(" <path_id> ", " <name> ", " <path_length> ", " <stop_id> ", " <stop_id> ")"
parseQueryCreatePath :: Parser2 Query
parseQueryCreatePath = do
  _ <- parseExact "create_path("
  pathId <- parsePathId
  _ <- parseSeperator
  name <- parseName
  _ <- parseSeperator
  pathLenght <- parsePathLenght
  _ <- parseSeperator
  stopId1 <- parseStopId
  _ <- parseSeperator
  stopId2 <- parseStopId
  _ <- parseExact ")"
  return (CreatePath pathId name pathLenght stopId1 stopId2)
-- <create_trip> ::= "create_trip(" <trip_id> ", " <name> ", " <list_of_stops_paths_creat> ")"
parseQueryCreateTrip :: Parser2 Query
parseQueryCreateTrip = do
  _ <- parseExact "create_trip("
  tripId <- parseTripId
  _ <- parseSeperator
  name <- parseName
  _ <- parseSeperator
  stops <- parseQueryStopOrPathOrCreatList
  _ <- parseExact ")"
  return (CreateTrip tripId name stops)

-- <find_next_stop> ::= "find_next_stop(" <stop_id> ", " <route_id> ")"
parseQueryFindNextStop :: Parser2 Query
parseQueryFindNextStop = do
  _ <- parseExact "find_next_stop("
  stopId <- parseStopId
  _ <- parseSeperator
  routeId <- parseRouteId
  _ <- parseExact ")"
  return (FindNextStop stopId routeId)
-- <find_previous_stop> ::= "find_previous_stop(" <stop_id> ", " <route_id> ")"
parseQueryFindPreviousStop :: Parser2 Query
parseQueryFindPreviousStop = do
  _ <- parseExact "find_previous_stop("
  stopId <- parseStopId
  _ <- parseSeperator
  routeId <- parseRouteId
  _ <- parseExact ")"
  return (FindPreviousStop stopId routeId)
-- <set_next_stop> ::= "set_next_stop(" <stop_id> ", " <route_id> ", " <next_stop_id> ")"
parseQuerySetNextStop :: Parser2 Query
parseQuerySetNextStop = do
  _ <- parseExact "set_next_stop("
  stopId <- parseStopId
  _ <- parseSeperator
  routeId <- parseRouteId
  _ <- parseSeperator
  nextStopId <- parseStopId
  _ <- parseExact ")"
  return (SetNextStop stopId routeId nextStopId)
-- <set_previous_stop> ::= "set_previous_stop(" <stop_id> ", " <route_id> ", " <previous_stop_id> ")"
parseQuerySetPreviousStop :: Parser2 Query
parseQuerySetPreviousStop = do
  _ <- parseExact "set_previous_stop("
  stopId <- parseStopId
  _ <- parseSeperator
  routeId <- parseRouteId
  _ <- parseSeperator
  previousStopId <- parseStopId
  _ <- parseExact ")"
  return (SetPreviousStop stopId routeId previousStopId)
-- <join_two_trips> ::= "join_two_trips(" <trip> ", " <trip> ", " <new_trip_id> ", " <new_name> ")"
parseQueryJoinTwoTrips :: Parser2 Query
parseQueryJoinTwoTrips = do
  _ <- parseExact "join_two_trips("
  trip1 <- parseQueryTrip
  _ <- parseSeperator
  trip2 <- parseQueryTrip
  _ <- parseSeperator
  newTripId <- parseTripId
  _ <- parseSeperator
  newName <- parseName
  _ <- parseExact ")"
  return (JoinTwoTrips trip1 trip2 newTripId newName)
-- <assign_stop_to_route> ::= "assign_stop_to_route(" <stopId> ", " <route_id ")"
parseQueryAssignStopToRoute :: Parser2 Query
parseQueryAssignStopToRoute = do
  _ <- parseExact "assign_stop_to_route("
  stopId <- parseStopId
  _ <- parseSeperator
  routeId <- parseRouteId
  _ <- parseExact ")"
  return (AssignStopToRoute stopId routeId)
-- <distance_between_stops> ::= "distance_between_stops(" <stop_id> ", " <stop_id> ")"
parseQueryDistanceBetweenStops :: Parser2 Query
parseQueryDistanceBetweenStops = do
  _ <- parseExact "distance_between_stops("
  stopId1 <- parseStopId
  _ <- parseSeperator
  stopId2 <- parseStopId
  _ <- parseExact ")"
  return (DistanceBetweenStops stopId1 stopId2)
-- <check_if_route_stops_connected> ::= "check_if_route_stops_connected(" <route_id> ")"
parseQueryCheckIfRouteStopsConnected :: Parser2 Query
parseQueryCheckIfRouteStopsConnected = do
  _ <- parseExact "check_if_route_stops_connected("
  routeId <- parseRouteId
  _ <- parseExact ")"
  return (CheckIfRouteStopsConnected routeId)
-- <connect_route_stops_by_min_dist> ::= "connect_route_stops_by_min_dist(" <route_id> ")"
parseQueryConnectRouteStopsByMinDistance :: Parser2 Query
parseQueryConnectRouteStopsByMinDistance = do
  _ <- parseExact "connect_route_stops_by_min_dist("
  routeId <- parseRouteId
  _ <- parseExact ")"
  return (ConnectRouteStopsByMinDistance routeId)
-- <trip_distance> ::= "trip_distance(" <trip> ")"
parseQueryTripDistance :: Parser2 Query
parseQueryTripDistance = do
  _ <- parseExact "trip_distance("
  trip <- parseQueryTrip
  _ <- parseExact ")"
  return (TripDistance trip)
-- <validate_trip> ::= "validate_trip(" <trip> ")"
parseQueryValidateTrip :: Parser2 Query
parseQueryValidateTrip = do
  _ <- parseExact "validate_trip("
  trip <- parseQueryTrip
  _ <- parseExact ")"
  return (ValidateTrip trip)
-- <cleanup_trip> ::= "cleanup_trip(" <trip> ")"
parseQueryCleanupTrip :: Parser2 Query
parseQueryCleanupTrip = do
  _ <- parseExact "cleanup_trip("
  trip <- parseQueryTrip
  _ <- parseExact ")"
  return (CleanupTrip trip)
-- <join_two_routes> ::= "join_two_routes(" <route> ", " <route> ", " <new_route_id> ", " <new_name> ")"
parseQueryJoinTwoRouter :: Parser2 Query
parseQueryJoinTwoRouter = do
  _ <- parseExact "join_two_routes("
  route1 <- parseQueryRoute
  _ <- parseSeperator
  route2 <- parseQueryRoute
  _ <- parseSeperator
  newRouteId <- parseRouteId
  _ <- parseSeperator
  newName <- parseName
  _ <- parseExact ")"
  return (JoinTwoRoutes route1 route2 newRouteId newName)
-- <join_two_routes_at_stop> ::= "join_two_routes_at_stop(" <route> ", " <route> ", " <stop_or_creat_or_nextprev> ", " <new_route_id> ", " <new_name> ")"
parseQueryJoinTwoRouterAtStop :: Parser2 Query
parseQueryJoinTwoRouterAtStop = do
  _ <- parseExact "join_two_routes_at_stop("
  route1 <- parseQueryRoute
  _ <- parseSeperator
  route2 <- parseQueryRoute
  _ <- parseSeperator
  stopOrCreatOrNextPrev <- parseQueryStopOrCreatOrNextPrev
  _ <- parseSeperator
  newRouteId <- parseRouteId
  _ <- parseSeperator
  newName <- parseName
  _ <- parseExact ")"
  return (JoinTwoRoutesAtStop route1 route2 stopOrCreatOrNextPrev newRouteId newName)

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String (Query, String)
parseQuery input = do
  let
    res = (parseQueryCreateStop `or2` parseQueryCreateRoute 
      `or2` parseQueryCreatePath `or2` parseQueryCreateTrip 
        `or2` parseQueryJoinTwoTrips `or2` parseQueryJoinTwoRouter 
          `or2` parseQueryJoinTwoRouterAtStop `or2` parseQueryCleanupTrip 
            `or2` parseQueryValidateTrip `or2` parseQueryFindNextStop 
              `or2` parseQueryFindPreviousStop `or2` parseQueryTripDistance 
                `or2` parseQuerySetNextStop `or2` parseQuerySetPreviousStop 
                  `or2` parseQueryConnectRouteStopsByMinDistance `or2` parseQueryCheckIfRouteStopsConnected 
                    `or2` parseQueryDistanceBetweenStops `or2` parseQueryAssignStopToRoute
                      `or2` parseQueryViewState)
    
    in case parse res input of
      (Left e1, _) -> Left "Unrecognized query format" 
      (Right r1, v1) -> Right (r1, v1)

        

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
createStop :: MyState -> Query -> Either String (Stop, MyState)
createStop state' query' =
  case query' of 
    (CreateStop stopId name point) -> 
      let
        stop = (Stop stopId name point [] [] [])
        sameStop = getByExtractorFromArray stopId (\(Stop sid _ _ _ _ _) -> sid) (stops state')
        in case sameStop of
          Right _ -> Left "Stop already exists"
          Left _ -> 
            let
              addStop' = addStop stop state'
              in case addStop' of
                Left e1 -> Left e1
                Right state'' -> Right (stop, state'')
    _ -> Left "Not a create stop query"
-- <create_route> ::= "create_route(" <route_id> ", " <name> ", " <list_of_stop_ids> ")"
data Route = Route RouteId Name [StopId] deriving (Show, Eq)
createRoute :: MyState -> Query -> Either String (Route, MyState)
createRoute state query' =
  case query' of
    (CreateRoute routeId name stopIds) ->
      let
        stopsExtracted = parseQueryStopOrCreatOrNextPrevListData stopIds state
        in case stopsExtracted of
          Left e1 -> Left e1
          Right (stops', state'') ->
            let
              extractStopIds = map (\(Stop sid _ _ _ _ _) -> sid) stops'
              route = Route routeId name extractStopIds
              sameRoute = getByExtractorFromArray routeId (\(Route rid _ _) -> rid) (routes state'')
              in case sameRoute of
                Right _ -> Left "Route already exists"
                Left _ -> 
                  let
                    routeStops = getStopsFromStopIdList (stops state'') extractStopIds
                    in case routeStops of
                      Left e1 -> Left e1
                      Right foundRouteStops ->
                        let
                          assigned = assignStopsToRoute foundRouteStops routeId
                          in case assigned of
                            Left e2 -> Left e2
                            Right foundStops -> 
                              let
                                updateStops = updateOrAddStops foundStops state''
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
data Trip = Trip TripId Name [QueryStopOrPath] deriving (Show, Eq)
createTrip :: MyState -> Query -> Either String (Trip, MyState)
createTrip state query' =
  case query' of
    (CreateTrip tripId name stopOrPathList) ->
      let
        sameTrip = getByExtractorFromArray tripId (\(Trip tid _ _) -> tid) (trips state)
        in case sameTrip of
          Right _ -> Left "Trip already exists"
          Left _ -> 
            let
              parseStopOrPathOrCreat = parseQueryStopOrPathOrCreatListData stopOrPathList state
              in case parseStopOrPathOrCreat of 
                Left e994 -> Left e994
                Right (stopsAndPaths, state'''') ->
                  let
                    getStopOrPathIDs = manyToQueryStopOrPath stopsAndPaths
                    trip = Trip tripId name getStopOrPathIDs
                    addTrip' = addTrip trip state''''
                    in case addTrip' of
                      Left e1 -> Left e1
                      Right newState -> Right (trip, newState)
    _ -> Left "Not a create trip query"
-- <create_path> ::= "create_path(" <path_id> ", " <name> ", " <path_length> ", " <stop_id> ", " <stop_id> ")"
data Path = Path PathId Name PathLenght StopId StopId deriving (Show, Eq)
createPath :: MyState -> Query -> Either String (Path, MyState)
createPath state query' =
  case query' of
    (CreatePath pathId name pathLenght stopId1 stopId2) ->
      let
        path = (Path pathId name pathLenght stopId1 stopId2)
        samePath = getByExtractorFromArray pathId (\(Path pid _ _ _ _) -> pid) (paths state)
        in case samePath of 
          Right _ -> Left "Path already exists"
          Left _ -> 
            let
              addPath' = addPath path state
              in case addPath' of
                Left e1 -> Left e1
                Right newState -> 
                  let
                    stop1 = getByExtractorFromArray stopId1 (\(Stop sid _ _ _ _ _) -> sid) (stops newState)
                    in case stop1 of
                      Left e2 -> Left e2
                      Right foundStop1 ->
                        let
                          stop2 = getByExtractorFromArray stopId2 (\(Stop sid _ _ _ _ _) -> sid) (stops newState)
                          in case stop2 of
                            Left e3 -> Left e3
                            Right foundStop2 -> Right (path, newState)
    _ -> Left "Not a create path query"
-- <assign_stop_to_route> ::= "assign_stop_to_route(" <stopId> ", " <route_id ")"
assignStopToRoute :: MyState -> Query -> Either String (Stop, MyState)
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
distanceBetweenStops :: MyState -> Query -> Either String (Float, MyState)
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
setNextStop :: MyState -> Query -> Either String MyState
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
parseSetNextStopData stop routeId nextStop' =
  let
    currentStop@(Stop sid name point nextStops prevStops routes') = stop
    in case L.find (== routeId) routes' of
      Nothing -> Left "Stop does not belong to the route"
      Just _ ->
        let
          nextStop@(Stop nextStopId _ _ _ _ routes'') = nextStop'
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
setPreviousStop :: MyState -> Query -> Either String MyState
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
parseSetPreviousStopData stop routeId previousStop' =
  let
    currentStop@(Stop sid name point nextStops prevStops routes') = stop
    in case L.find (== routeId) routes' of
      Nothing -> Left "Stop does not belong to the route"
      Just _ ->
        let
          previousStop@(Stop previousStopId _ _ _ _ routes'') = previousStop'
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
findNextStop :: MyState -> Query -> Either String (StopId, MyState)
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
findPreviousStop :: MyState -> Query -> Either String (StopId, MyState)
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
connectRouteStopsByMinDist :: MyState -> Query -> Either String MyState
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
                  in if stop' == prev' then connectStopsByMinDist' newStop' closestStop' t (acc ++ [newStop'])
                  else
                    let
                      newPrevStop = parseSetPreviousStopData newStop' route' prev'
                      in case newPrevStop of
                        Left e3 -> Left e3
                        Right newPrevStop' -> connectStopsByMinDist' newPrevStop' closestStop' t (acc ++ [newPrevStop'])
-- <check_if_route_stops_connected> ::= "check_if_route_stops_connected(" <route_id> ")" -- atleast n-1 previous connections and n-1 next connections
checkIfRouteStopsConnected :: MyState -> Query -> Either String (Bool, MyState)
checkIfRouteStopsConnected state query' =
  case query' of
    (CheckIfRouteStopsConnected routeId) ->
      let
        routeStops = getRouteStops routeId (stops state)
        in case routeStops of
          Left e1 -> Left e1
          Right foundRouteStops ->
            let
              connected = checkIfRouteStopsConnectedData foundRouteStops routeId
              in case connected of
                Left e2 -> Left e2
                Right (prevCount, nextCount) ->
                  let
                    in if prevCount >= (length foundRouteStops - 1) && nextCount >= (length foundRouteStops - 1)
                      then Right (True, state)
                      else Right (False, state)
    _ -> Left "Not a check if route stops connected query"
checkIfRouteStopsConnectedData :: [Stop] -> RouteId -> Either String (Int, Int)
checkIfRouteStopsConnectedData [] rid = Right (0, 0)
checkIfRouteStopsConnectedData [_] _ = Right (1, 1)
checkIfRouteStopsConnectedData (h':t') rid' = checkConnection' (h':t') 0 0 rid' 0
  where 
    checkConnection' [] prevCount nextCount _ _ = Right (prevCount, nextCount)
    checkConnection' (h:t) prevCount nextCount rid index =
      let
        Stop _ _ _ nextStop prevStops _ = h

        routeId' = getByExtractorFromArray rid (\(NextStop _ rid') -> rid') nextStop
        routeId'' = getByExtractorFromArray rid (\(PreviousStop _ rid') -> rid') prevStops
        in case routeId' of
          Left e1 -> 
             case routeId'' of
              Left e2 -> Left "Route stops are not connected"
              Right routeId2 -> 
                let in if index == nextCount
                  then checkConnection' t (prevCount + 1) nextCount rid (index+1)
                  else Left "Route stops are not connected"

          Right routeId1 ->
            case routeId'' of
              Left e2 -> 
                let in if index == 0
                  then checkConnection' t prevCount (nextCount + 1) rid (index+1)
                  else Left "Route stops are not connected"
              Right routeId2 ->
                let
                  routeId1' = case routeId1 of (NextStop _ rid'') -> rid''
                  routeId2' = case routeId2 of (PreviousStop _ rid'') -> rid''
                  in if routeId1' == rid && routeId2' == rid
                    then checkConnection' t (prevCount + 1) (nextCount + 1) rid (index+1)
                    else
                      if routeId1' == rid
                        then checkConnection' t prevCount (nextCount + 1) rid (index+1)
                        else
                          if routeId2' == rid
                            then checkConnection' t (prevCount + 1) nextCount rid (index+1)
                            else checkConnection' t prevCount nextCount rid (index+1)
-- <validate_trip> ::= "validate_trip(" <trip> ")" -- all stops and paths are connected
validateTrip :: MyState -> Query -> Either String (Bool, MyState)
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
                    stopOrPathEnt = getManyStopOrPath stopOrPathList state''
                    in case stopOrPathEnt of
                      Left e1 -> Left e1
                      Right stopOrPathEnt' ->
                        let
                          connected = validateTripData stopOrPathEnt'
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
cleanupTrip :: MyState -> Query -> Either String MyState
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
                    stopOrPathEnt = getManyStopOrPath stopOrPathList state''''
                    in case stopOrPathEnt of
                      Left e1 -> Left e1
                      Right stopOrPathEnt' ->
                        let
                          cleaned = cleanupTripData stopOrPathEnt'
                          in case cleaned of
                            Left e2 -> Left e2
                            Right cleaned' ->
                              let
                                newStopsAndPaths = manyToQueryStopOrPath cleaned'
                                updatedTrip = Trip tid' (case foundTrip of Trip _ name _ -> name) newStopsAndPaths
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
        connected = validateTripData acc
        in case connected of
          Left e1 -> Right acc
          Right _ -> cleanUpTripData t (acc ++ [h])
-- <trip_distance> ::= "trip_distance(" <trip> ")" -- sum of path lengths and distances between stop poitntssnsns
tripDistance :: MyState -> Query -> Either String (Float, MyState)
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
                    stopOrPathEntList = getManyStopOrPath stopOrPathList state'
                    in case stopOrPathEntList of 
                      Left e3 -> Left e3
                      Right stopOrPathEntList' ->
                        let
                          distance = tripDistanceData stopOrPathEntList'
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
joinTwoRoutes :: MyState -> Query -> Either String (Route, MyState)
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
joinTwoTrips :: MyState -> Query -> Either String (Trip, MyState)
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
joinTwoRoutesAtStop :: MyState -> Query -> Either String (Route, MyState)
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
parseQueryRouteData :: QueryRoute -> MyState -> Either String (Route, MyState) -- create new route if needed and return the route id
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
parseQueryStopOrPathOrCreateData :: QueryStopOrPathOrCreate -> MyState -> Either String (StopOrPath, MyState)
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
parseQueryStopOrPathData :: QueryStopOrPath -> MyState -> Either String (StopOrPath, MyState)
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
parseQueryStopOrCreatOrNextPrevData :: QueryStopOrCreatOrNextPrev -> MyState -> Either String (Stop, MyState)
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
parseQueryTripData :: QueryTrip -> MyState -> Either String  (Trip, MyState)
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
parseQueryStopOrPathOrCreatListData :: [QueryStopOrPathOrCreate] -> MyState -> Either String ([StopOrPath], MyState)
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


parseQueryStopOrCreatOrNextPrevListData :: [QueryStopOrCreatOrNextPrev] -> MyState -> Either String ([Stop], MyState)
parseQueryStopOrCreatOrNextPrevListData [] state = Right ([], state)
parseQueryStopOrCreatOrNextPrevListData (h:t) state =
  let
    parse = parseQueryStopOrCreatOrNextPrevData h state
    in case parse of
      Left e1 -> Left e1
      Right (stop, newState) ->
        let
          parse' = parseQueryStopOrCreatOrNextPrevListData t newState
          in case parse' of
            Left e2 -> Left e2
            Right (stopList, newState') -> Right (stop : stopList, newState')


   


-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data MyState = State {
  stops :: [Stop],
  routes :: [Route],
  paths :: [Path],
  trips :: [Trip]
} deriving (Show, Eq)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: MyState
emptyState = State [] [] [] []



-- STATE HELPER FUNCTIONS
addTrip :: Trip -> MyState -> Either String MyState
addTrip trip state =
  let
    trips' = addToArray trip (trips state)
    in case trips' of
      Left e1 -> Left e1
      Right v1 -> Right state {trips = v1}
getTrip :: TripId -> MyState -> Either String Trip
getTrip targetId state = getTrip' targetId (trips state)
  where
    getTrip' _ [] = Left "Trip not found"
    getTrip' targetId' (Trip tid name stopOrPath : rest)
      | targetId' == tid = Right (Trip tid name stopOrPath)
      | otherwise       = getTrip' targetId' rest
updateTrip :: Trip -> MyState -> Either String MyState
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
updateOrAddTrips :: [Trip] -> MyState -> Either String MyState
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
deleteTrip :: TripId -> MyState -> Either String MyState
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
tripInState :: Trip -> MyState -> Bool
tripInState trip state = elementInArray trip (trips state)
-- STATE HELPER FUNCTIONS
addStop :: Stop -> MyState -> Either String MyState
addStop stop state =
  let
    stops' = addToArray stop (stops state)
    in case stops' of
      Left e1 -> Left e1
      Right v1 -> Right state {stops = v1}
getStop :: StopId -> MyState -> Either String Stop
getStop targetId state = getStop' targetId (stops state)
  where
    getStop' _ [] = Left "Stop not found"
    getStop' targetId' (Stop sid name point prevStops nextStops routes : rest)
      | targetId' == sid = Right (Stop sid name point prevStops nextStops routes)
      | otherwise       = getStop' targetId' rest
updateStop :: Stop -> MyState -> Either String MyState
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
updateOrAddStops :: [Stop] -> MyState -> Either String MyState
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
deleteStop :: StopId -> MyState -> Either String MyState
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
stopInState :: Stop -> MyState -> Bool
stopInState stop state = elementInArray stop (stops state)
-- STATE HELPER FUNCTIONS
addRoute :: Route -> MyState -> Either String MyState
addRoute route state =
  let
    routes' = addToArray route (routes state)
    in case routes' of
      Left e1 -> Left e1
      Right v1 -> Right state {routes = v1}
getRoute :: RouteId -> MyState -> Either String Route
getRoute targetId state = getRoute' targetId (routes state)
  where
    getRoute' _ [] = Left "Route not found"
    getRoute' targetId' (Route rid name stopIds : rest)
      | targetId' == rid = Right (Route rid name stopIds)
      | otherwise       = getRoute' targetId' rest
updateRoute :: Route -> MyState -> Either String MyState
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
updateOrAddRoutes :: [Route] -> MyState -> Either String MyState
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
deleteRoute :: RouteId -> MyState -> Either String MyState
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
routeInState :: Route -> MyState -> Bool
routeInState route state = elementInArray route (routes state)
-- STATE HELPER FUNCTIONS
addPath :: Path -> MyState -> Either String MyState
addPath path state =
  let
    paths' = addToArray path (paths state)
    in case paths' of
      Left e1 -> Left e1
      Right v1 -> Right state {paths = v1}
getPath :: PathId -> MyState -> Either String Path
getPath targetId state = getPath' targetId (paths state)
  where
    getPath' _ [] = Left "Path not found"
    getPath' targetId' (Path pid name pathLenght startId endId : rest)
      | targetId' == pid = Right (Path pid name pathLenght startId endId)
      | otherwise       = getPath' targetId' rest
updatePath :: Path -> MyState -> Either String MyState
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
updateOrAddPaths :: [Path] -> MyState -> Either String MyState
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
deletePath :: PathId -> MyState -> Either String MyState
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
pathInState :: Path -> MyState -> Bool
pathInState path state = elementInArray path (paths state)



-- get stopOrPath from QueryStopOrPath
getStopOrPath :: QueryStopOrPath -> MyState -> Either String StopOrPath
getStopOrPath query' state =
  case query' of
    (StopId' stopId) ->
      let
        stop = getStop stopId state
        in case stop of
          Left e1 -> Left e1
          Right foundStop -> Right (Stop' foundStop)
    (PathId' pathId) ->
      let
        path = getPath pathId state
        in case path of
          Left e1 -> Left e1
          Right foundPath -> Right (Path' foundPath)
    _ -> Left "Not a stop or path query"

getManyStopOrPath :: [QueryStopOrPath] -> MyState -> Either String [StopOrPath]
getManyStopOrPath [] state = Right []
getManyStopOrPath (h:t) state =
  let
    stopOrPath = getStopOrPath h state
    in case stopOrPath of
      Left e1 -> Left e1
      Right foundStopOrPath ->
        let
          stopOrPathList = getManyStopOrPath t state
          in case stopOrPathList of
            Left e2 -> Left e2
            Right foundStopOrPathList -> Right (foundStopOrPath : foundStopOrPathList)

toQueryStopOrPath :: StopOrPath -> QueryStopOrPath
toQueryStopOrPath stopOrPath =
  case stopOrPath of
    Stop' (Stop sid _ _ _ _ _) -> StopId' sid
    Path' (Path pid _ _ _ _) -> PathId' pid

manyToQueryStopOrPath :: [StopOrPath] -> [QueryStopOrPath]
manyToQueryStopOrPath [] = []
manyToQueryStopOrPath (h:t) = (toQueryStopOrPath h) : (manyToQueryStopOrPath t)


combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing msg = msg
combineMessages msg Nothing = msg
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ msg2)


multiQuery :: (Either String (Maybe String, MyState)) -> String -> Either String (Maybe String, MyState)
multiQuery prevQuer stringInp =
  case prevQuer of
    Left e1 -> Left e1
    Right (msg, state') ->
      let
        query = parseQuery stringInp
        in case query of
          Left e1 -> Left e1
          Right query' -> 
            let
              newQuer = stateTransition state' (fst query')
              in case newQuer of
                Left e2 -> Left e2
                Right (msg2, state'') -> Right (msg2, state'')

              
              



-- parsequery with stateTransition
stateWithQuery :: MyState -> String -> Either String (Maybe String, MyState)
stateWithQuery state input = 
  let
    query = parseQuery input
    in
      case query of
        Left e1 -> Left e1
        Right query' -> stateTransition state (fst query')

getQuery :: Either String Query -> Either String Query
getQuery input =
  case input of
    Left e1 -> Left e1
    Right input' -> Right input'

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: MyState -> Query -> Either String (Maybe String, MyState)
stateTransition state query = 
  case query of
    (CreateStop _ _ _) -> case createStop state query of
      Right (stop, state') -> Right (Just ("Stop " ++ (case stop of Stop _ name _ _ _ _ -> show name) ++ " created"), state')
      Left e1 -> Left e1
    (CreateRoute _ _ _) -> case createRoute state query of
      Right (route, state') -> Right (Just ("Route " ++ (case route of Route _ name _ -> show name) ++ " created"), state')
      Left e1 -> Left e1
    (CreateTrip _ _ _) -> case createTrip state query of
      Right (trip, state') -> Right (Just ("Trip " ++ (case trip of Trip _ name _ -> show name) ++ " created"), state')
      Left e1 -> Left e1
    (CreatePath _ _ _ _ _) -> case createPath state query of
      Right (path, state') -> Right (Just ("Path " ++ (case path of Path _ name _ _ _ -> show name) ++ " created"), state')
      Left e1 -> Left e1
    (AssignStopToRoute _ _) -> case assignStopToRoute state query of
      Right (stop, state') -> Right (Just ("Stop " ++ (case stop of Stop _ name _ _ _ _ -> show name) ++ " assigned to route."), state')
      Left e1 -> Left e1
    (DistanceBetweenStops _ _) -> case distanceBetweenStops state query of
      Right (distance, state') -> Right (Just ("Distance between stops is " ++ show distance), state')
      Left e1 -> Left e1
    (SetNextStop _ _ _) -> case setNextStop state query of
      Right state' -> Right (Just ("Next stop set"), state')
      Left e1 -> Left e1
    (SetPreviousStop _ _ _) -> case setPreviousStop state query of
      Right state' -> Right (Just ("Previous stop set"), state')
      Left e1 -> Left e1
    (FindNextStop _ _) -> case findNextStop state query of
      Right (stopId, state') -> Right (Just ("Next stop found: " ++ show stopId), state')
      Left e1 -> Left e1
    (FindPreviousStop _ _) -> case findPreviousStop state query of
      Right (stopId, state') -> Right (Just ("Previous stop found: " ++ show stopId), state')
      Left e1 -> Left e1
    (ConnectRouteStopsByMinDistance _) -> case connectRouteStopsByMinDist state query of
      Right state' -> Right (Just ("Route stops connected by min distance"), state')
      Left e1 -> Left e1
    (CheckIfRouteStopsConnected _) -> case checkIfRouteStopsConnected state query of
      Right (connected, state') -> Right (Just ("Route stops connected: " ++ show connected), state')
      Left e1 -> Left e1
    (ValidateTrip _) -> case validateTrip state query of
      Right (connected, state') -> Right (Just ("Trip stops connected: " ++ show connected), state')
      Left e1 -> Left e1
    (CleanupTrip _) -> case cleanupTrip state query of
      Right state' -> Right (Just ("Trip cleaned up"), state')
      Left e1 -> Left e1
    (TripDistance _) -> case tripDistance state query of
      Right (distance, state') -> Right (Just ("Trip distance: " ++ show distance), state')
      Left e1 -> Left e1
    (JoinTwoRoutes _ _ _ _) -> case joinTwoRoutes state query of
      Right (route, state') -> Right (Just ("Routes joined"), state')
      Left e1 -> Left e1
    (JoinTwoTrips _ _ _ _) -> case joinTwoTrips state query of
      Right (trip, state') -> Right (Just ("Trips joined"), state')
      Left e1 -> Left e1
    (JoinTwoRoutesAtStop _ _ _ _ _) -> case joinTwoRoutesAtStop state query of
      Right (route, state') -> Right (Just ("Routes joined at stop"), state')
      Left e1 -> Left e1
    (View) -> Right (Just $ show state, state)
    _ -> Left "Invalid query"



