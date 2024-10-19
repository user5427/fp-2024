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







getByExtractorFromArray :: Eq a => a -> (b -> a) -> [b] -> Either String b
getByExtractorFromArray value extractor arr = getByIndexFromArray' value extractor arr
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
addToArray = addToArray' []
  where
    addToArray' acc target [] = Right (acc ++ [target])
    addToArray' acc target (h:t) = if target == h then Left "Element already exists" else Right (acc ++ [h] ++ t)

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

getStringFromParser :: Parser String -> Parser String
getStringFromParser parser input =
  let
    result = parser input
    in case result of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (v1, r1)

data TripId = TripId Char Int deriving (Show, Eq)

-- <trip_id> ::= "T" <integer>
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

data RouteId = RouteId Char Int deriving (Show, Eq)

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

data StopId = StopId Char Int deriving (Show, Eq)

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

-- <list_of_stop_ids> ::= <stop_id> "," <list_of_stop_ids> | <stop_id>
-- >>> parseStopIdList "S1, S2, S3"
-- Right ([StopId 'S' 1,StopId 'S' 2,StopId 'S' 3],"")
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

data PathId = PathId Char Int deriving (Show, Eq)

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

data PathLenght = PathLenght Float deriving (Show, Eq)

-- <path_length> ::= <float>
parsePathLenght :: Parser PathLenght
parsePathLenght [] = Left "empty input, cannot parse a path length"
parsePathLenght input =
  let
    pathLenght = parseFloat input
    in case pathLenght of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (PathLenght v1, r1)

data CoordX = CoordX Float deriving (Show, Eq)

-- <coord_x> ::= <float>
parseCoordX :: Parser CoordX
parseCoordX [] = Left "empty input, cannot parse a coord x"
parseCoordX input =
  let
    coordX = parseFloat input
    in case coordX of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (CoordX v1, r1)

data CoordY = CoordY Float deriving (Show, Eq)

-- <coord_y> ::= <float>
parseCoordY :: Parser CoordY
parseCoordY [] = Left "empty input, cannot parse a coord y"
parseCoordY input =
  let
    coordY = parseFloat input
    in case coordY of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (CoordY v1, r1)

data Point = Point CoordX CoordY deriving (Show, Eq)

-- <point> ::= <coord_x> ", " <coord_y>
parsePoint :: Parser Point
parsePoint input = (and3' (\a _ b -> Point a b) parseCoordX (parseSeperator) parseCoordY) input

data Name = Name String deriving (Show, Eq)

-- <name> ::= <string>
parseName :: Parser Name
parseName [] = Left "empty input, cannot parse a name"
parseName input =
  let
    name = parseString input
    in case name of
      Left e1 -> Left e1
      Right (v1, r1) -> Right (Name v1, r1)

data StopOrPath = StopId' StopId
                  | PathId' PathId deriving (Show, Eq)

-- <stop_or_path> ::= <stop_id> | <path_id>
parseStopOrPath :: Parser StopOrPath
parseStopOrPath input =
  let
    stopId = parseStopId input
    in case stopId of
      Right (v1, r1) -> Right (StopId' v1, r1)
      Left _ ->
        let
          pathId = parsePathId input
          in case pathId of
            Right (v1, r1) -> Right (PathId' v1, r1)
            Left e1 -> Left e1

parseStopOrPathList :: Parser [StopOrPath]
parseStopOrPathList [] = Left "empty input, cannot parse a stop or path list"
parseStopOrPathList input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input acc =
      case parseStopOrPath input of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ ->
          case parseSeperator input of
            Right (_, r1) -> many' r1 acc
            Left e1 -> Right (acc, input)

data NextStop = NextStop StopId RouteId deriving (Show, Eq)
data PreviousStop = PreviousStop StopId RouteId deriving (Show, Eq)
data Stop = Stop StopId Name Point [NextStop] [PreviousStop] [RouteId] deriving (Show, Eq)

-- <route_id> ", " <next_stop_id>
parseNextStop :: [Stop] -> Parser NextStop
parseNextStop stops input =
  let
    res = and2' (\a b -> NextStop b a)
          parseRouteId
          (and2' (\_ b -> b) parseSeperator parseStopId) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> 
        let 
          stop = getByExtractorFromArray (case r1 of (NextStop sid _) -> sid) (\(Stop sid _ _ _ _ _) -> sid) stops
          in case stop of
            Left e1 -> Left e1
            Right foundStop@(Stop _ _ _ nextStops _ routes') -> 
              let
                routeId = case r1 of (NextStop _ rid) -> rid
                in case L.find (\rid -> rid == routeId) routes' of
                  Nothing -> Left "Stop does not belong to the route"
                  Just (_) -> Right (r1, v1)

-- <route_id> ", " <previous_stop_id>
parsePreviousStop :: [Stop] -> Parser PreviousStop
parsePreviousStop stops input =
  let
    res = and2' (\a b -> PreviousStop b a)
          parseRouteId
          (and2' (\_ b -> b) parseSeperator parseStopId) input
    in case res of
      Left e1 -> Left e1
      Right (r1, v1) -> 
        let
          stop = getByExtractorFromArray (case r1 of (PreviousStop sid _) -> sid) (\(Stop sid _ _ _ _ _) -> sid) stops
          in case stop of
            Left e1 -> Left e1
            Right foundStop@(Stop _ _ _ _ _ routes') -> 
              let
                routeId = case r1 of (PreviousStop _ rid) -> rid
                in case L.find (\rid -> rid == routeId) routes' of
                  Nothing -> Left "Stop does not belong to the route"
                  Just (_) -> Right (r1, v1)


-- <create_stop> ::= "create_stop(" <stop_id> ", " <name> ", " <point> ")"
parseCreateStop :: Parser Stop
parseCreateStop [] = Left "empty input, cannot parse a create stop"
parseCreateStop input =
  let
    res = and3' (\a b c -> Stop a b c [] [] [])
          (and3' (\_ b _ -> b) (parseExact "create_stop(") parseStopId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator parsePoint (parseExact ")")) input
    in case res of
    Left e1 -> Left e1
    Right (r1, v1) -> Right (r1, v1)

assignStopToRouteData :: Stop -> RouteId -> Stop
assignStopToRouteData stop routeId =
  let
    (Stop sid name point nextStops prevStops routes') = stop
    in case L.find (\rid -> rid == routeId) routes' of
      Nothing -> Stop sid name point nextStops prevStops (routeId : routes')
      Just (_) -> stop

-- <assign_stop_to_route> ::= "assign_stop_to_route(" <stopId> ", " <route_id ")"
parseAssignStopToRoute :: [Stop] -> [Route] -> Parser (Stop, Route)
parseAssignStopToRoute _ _ [] = Left "empty input, cannot parse an assign stop to route"
parseAssignStopToRoute stops routes input =
  let
    res = and3' (\_ a b -> (a, b)) (parseExact "assign_stop_to_route(") 
          parseStopId
          (and3' (\_ c _ -> c) parseSeperator parseRouteId (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right ((a', b'), v1) -> 
        let
          stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops
          in case stop of
            Left e1 -> Left e1
            Right foundStop@(Stop sid name point nextStops prevStops routes') -> 
              let
                in case L.find (\rid -> rid == b') routes' of
                  Just (_) -> Left "Stop already belongs to the route"
                  Nothing -> let
                    route = getByExtractorFromArray b' (\(Route rid _ _) -> rid) routes
                    in case route of
                      Left e2 -> Left e2
                      Right foundRoute@(Route rid name' stops') -> 
                        let
                          updatedStop = assignStopToRouteData foundStop b'
                          updatedRoute = Route rid name' (stops' ++ [sid])
                          in Right ((updatedStop, updatedRoute), v1)

-- check if both stops belong to the same route
-- check if the stops are not the same
-- check if the stops are not already connected
-- check if the distance is not 0
parseSetNextStopData :: Stop -> RouteId -> Stop -> Either String Stop
parseSetNextStopData stop routeId nextStop =
  let
    currentStop@(Stop sid name point nextStops prevStops routes') = stop
    in case L.find (\rid -> rid == routeId) routes' of
      Nothing -> Left "Stop does not belong to the route"
      Just (_) ->
        let
          nextStop@(Stop nextStopId _ _ _ _ routes'') = nextStop
          in case L.find (\rid -> rid == routeId) routes'' of
            Nothing -> Left "Next stop does not belong to the route"
            Just (_) ->
              let
                sameNextStop = getByExtractorFromArray routeId (\(NextStop _ rid) -> rid) nextStops
                in case sameNextStop of
                  Right e5 -> Left "Next stop already exists for that route"
                  Left _ -> let
                    in if stop == nextStop then Left "Stop cannot be connected to itself" else
                      let
                        distance = distanceBetweenStopsData stop nextStop
                        in if distance == 0 then Left "Distance between stops is 0" else
                          Right (Stop sid name point ((NextStop nextStopId routeId) : nextStops) prevStops routes')

-- same for previous stop
parseSetPreviousStopData :: Stop -> RouteId -> Stop -> Either String Stop
parseSetPreviousStopData stop routeId previousStop =
  let
    currentStop@(Stop sid name point nextStops prevStops routes') = stop
    in case L.find (\rid -> rid == routeId) routes' of
      Nothing -> Left "Stop does not belong to the route"
      Just (_) ->
        let
          previousStop@(Stop previousStopId _ _ _ _ routes'') = previousStop
          in case L.find (\rid -> rid == routeId) routes'' of
            Nothing -> Left "Previous stop does not belong to the route"
            Just (_) ->
              let
                samePreviousStop = getByExtractorFromArray routeId (\(PreviousStop _ rid) -> rid) prevStops
                in case samePreviousStop of
                  Right e5 -> Left "Previous stop already exists for that route"
                  Left _ -> let
                    in if stop == previousStop then Left "Stop cannot be connected to itself" else
                      let
                        distance = distanceBetweenStopsData stop previousStop
                        in if distance == 0 then Left "Distance between stops is 0" else
                          Right (Stop sid name point nextStops ((PreviousStop previousStopId routeId) : prevStops) routes')


-- <set_next_stop> ::= "set_next_stop(" <stop_id> ", " <route_id> ", " <next_stop_id> ")"
parseSetNextStop :: [Stop] -> Parser Stop
parseSetNextStop _ [] = Left "empty input, cannot parse a set next stop"
parseSetNextStop stops' input =
  let
    res = (and3' (\_ b _ -> b) (parseExact "set_next_stop(") parseStopId parseSeperator) input
    in case res of
    Left e1 -> Left e1
    Right (a', v1) ->
      let
        stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop of
          Left e1 -> Left e1
          Right (stop1') -> 
            let
              stop2 = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
              in case stop2 of
                Left e1' -> Left e1'
                Right (stop2') ->
                  let
                    nextStop = parseNextStop stops' v1
                    in case nextStop of
                      Left e2 -> Left e2
                      Right (v2, r2) -> 
                        let 
                          parseClosing = parseExact ")" r2
                          in case parseClosing of
                            Left e3 -> Left e3
                            Right (_, r3) -> 
                                let 
                                  parseee = parseSetNextStopData stop1' (case v2 of (NextStop _ rid) -> rid) stop2'
                                  in case parseee of
                                    Left e4 -> Left e4
                                    Right stop' -> Right (stop', r3)


-- <set_previous_stop> ::= "set_previous_stop(" <stop_id> ", " <route_id> ", " <previous_stop_id> ")"
parseSetPreviousStop :: [Stop] -> Parser Stop
parseSetPreviousStop _ [] = Left "empty input, cannot parse a set previous stop"
parseSetPreviousStop stops' input =
  let
    res = (and3' (\_ b _ -> b) (parseExact "set_previous_stop(") parseStopId parseSeperator) input
    in case res of
    Left e1 -> Left e1
    Right (a', v1) ->
      let
        stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop of
          Left e1 -> Left e1
          Right (stop') -> 
            let
              stop2 = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
              in case stop2 of
                Left e1' -> Left e1'
                Right (stop2') ->
                  let
                    previousStop = parsePreviousStop stops' v1
                    in case previousStop of
                      Left e2 -> Left e2
                      Right (v2, r2) -> 
                        let 
                          parseClosing = parseExact ")" r2
                          in case parseClosing of
                            Left e3 -> Left e3
                            Right (_, r3) -> 
                              let 
                                parseee = parseSetPreviousStopData stop' (case v2 of (PreviousStop _ rid) -> rid) stop2'
                                in case parseee of
                                  Left e4 -> Left e4
                                  Right stop' -> Right (stop', r3)

-- data NextStop = NextStop StopId RouteId deriving (Show, Eq)
-- <find_next_stop> ::= "find_next_stop(" <stop_id> ", " <route> ")"
parseFindNextStop :: [Stop] -> Parser StopId
parseFindNextStop _ [] = Left "empty input, cannot parse a find next stop"
parseFindNextStop stops' input =
  let
    res = and3' (\a b _ -> (a, b))
          (and3' (\_ b _ -> b) (parseExact "find_next_stop(") parseStopId parseSeperator)
          parseRouteId
          (parseExact ")") input
    in case res of
    Left e1 -> Left e1
    Right ((a', b'), v1) -> 
      let
        stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop of
          Left e1 -> Left e1
          Right foundStop@(Stop _ _ _ nextStops _ _) -> 
            let
              nextStopId = findNextStop' b' nextStops
              in case nextStopId of
                Left e2 -> Left e2
                Right foundNextStopId -> Right (foundNextStopId, v1)
              
              where
                findNextStop' _ [] = Left "Next stop not found"
                findNextStop' route nextStops' =
                  case L.find (\(NextStop _ routeId) -> routeId == route) nextStops' of
                    Nothing -> Left "Next stop not found"
                    Just (NextStop nextStopId _) -> Right nextStopId

-- <find_previous_stop> ::= "find_previous_stop(" <stop_id> ", " <route> ")"
parseFindPreviousStop :: [Stop] -> Parser StopId
parseFindPreviousStop _ [] = Left "empty input, cannot parse a find previous stop"
parseFindPreviousStop stops' input =
  let
    res = and3' (\a b _ -> (a, b))
          (and3' (\_ b _ -> b) (parseExact "find_previous_stop(") parseStopId parseSeperator)
          parseRouteId
          (parseExact ")") input
    in case res of
    Left e1 -> Left e1
    Right ((a', b'), v1) -> 
      let
        stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop of
          Left e1 -> Left e1
          Right foundStop@(Stop _ _ _ _ prevStops _) -> 
            let
              previousStopId = findPreviousStop' b' prevStops
              in case previousStopId of
                Left e2 -> Left e2
                Right foundPreviousStopId -> Right (foundPreviousStopId, v1)
              
              where
                findPreviousStop' _ [] = Left "Previous stop not found"
                findPreviousStop' route prevStops' =
                  case L.find (\(PreviousStop _ routeId) -> routeId == route) prevStops' of
                    Nothing -> Left "Previous stop not found"
                    Just (PreviousStop previousStopId _) -> Right previousStopId

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

assignStopsToRoute :: [Stop] -> RouteId -> Either String [Stop]
assignStopsToRoute stops' routeId = assignStopsToRoute' stops' routeId []
  where
    assignStopsToRoute' [] _ acc = Right acc
    assignStopsToRoute' (h:t) routeId acc =
      let
        stop@(Stop sid name point nextStops prevStops routes') = h
        newStop = (Stop sid name point nextStops prevStops (routeId : routes'))
        in assignStopsToRoute' t routeId (acc ++ [newStop])

data Route = Route RouteId Name [StopId] deriving (Show, Eq)

-- <create_route> ::= "create_route(" <route_id> ", " <name> ", " <list_of_stop_ids> ")"
parseCreateRoute :: [Stop] -> Parser (Route, [Stop])
parseCreateRoute _ [] = Left "empty input, cannot parse a create route"
parseCreateRoute stops input =
  let
    res = and3' (\a b c -> (a, b, c))
          (and3' (\_ b _ -> b) (parseExact "create_route(") parseRouteId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator parseStopIdList (parseExact ")")) input
    in case res of
    Left e1 -> Left e1
    Right ((a', b', c'), v1) -> 
      let
        route = Route a' b' c'
        routeStops = getStopsFromStopIdList stops c'
        in case routeStops of
          Left e1 -> Left e1
          Right foundRouteStops ->
            let 
              assigned = assignStopsToRoute foundRouteStops a'
              in case assigned of
                Left e2 -> Left e2
                Right foundStops -> Right ((route, foundStops), v1)


        

-- HELPER FUNCTIONS
distanceBetweenPoints :: Point -> Point -> Float
distanceBetweenPoints (Point (CoordX x1) (CoordY y1)) (Point (CoordX x2) (CoordY y2)) = (sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

-- HELPER FUNCTIONS
findClosestStop :: Stop -> [Stop] -> Either String Stop
findClosestStop _ [] = Left "Closest stop not found"
findClosestStop stop [] = Left "Closest stop not found"
findClosestStop stop input =
  let
    closStop = findClosestStop' stop input (-1) stop
    in case closStop of
      Left e1 -> Left e1
      Right (v1) -> Right v1

    where
      findClosestStop' a [] _ b = if a == b then Left "Closest stop not found" else Right (b)
      findClosestStop' a (h:t) distance closestStop =
        let
          newDistance = distanceBetweenStopsData a h
          in if newDistance < distance || distance == (-1)
            then findClosestStop' a t newDistance h
            else findClosestStop' a t distance closestStop


distanceBetweenStopsData :: Stop -> Stop-> Float
distanceBetweenStopsData a b =
  let
    aPoint = (case a of (Stop _ _ a' _ _ _ ) -> a')
    bPoint = (case b of (Stop _ _ b' _ _ _ ) -> b')
    in (distanceBetweenPoints aPoint bPoint)
        
-- <distance_between_stops> ::= "distance_between_stops(" <stop_id> ", " <stop_id> ")"
distanceBetweenStops :: [Stop] -> Parser Float
distanceBetweenStops _ [] = Left "empty input, cannot parse a distance between stops"
distanceBetweenStops stops' input =
  let
    res = and3' (\a b _ -> (a, b))
          (and3' (\_ b _ -> b) (parseExact "distance_between_stops(") parseStopId parseSeperator)
          parseStopId
          (parseExact ")") input
    in case res of
    Left e1 -> Left e1
    Right ((a', b'), v1) -> 
      let
        stop1 = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop1 of
          Left e1 -> Left e1
          Right foundStop1@(Stop _ _ c' _ _ _) -> 
            let
              stop2 = getByExtractorFromArray b' (\(Stop sid _ _ _ _ _) -> sid) stops'
              in case stop2 of
                Left e2 -> Left e2
                Right foundStop2@(Stop _ _ d' _ _ _) -> 
                  Right (distanceBetweenPoints c' d', v1)

getRouteStops :: RouteId -> [Stop] -> Either String [Stop]
getRouteStops _ [] = Left "Route stops not found"
getRouteStops routeId stops' = getRouteStops' routeId stops' []
  where
    getRouteStops' _ [] [] = Left "Route stops not found"
    getRouteStops' _ [] acc = Right acc
    getRouteStops' routeId' (h:t) acc =
      let
        routesFromStop = case h of (Stop _ _ _ _ _ routes) -> routes
        in case L.find (\rid -> rid == routeId') routesFromStop of
          Nothing -> getRouteStops' routeId' t acc
          Just _ -> getRouteStops' routeId' t (acc ++ [h])

        -- in if routeId' == (case )
        --   then getRouteStops' routeId' t (acc ++ [h])
        --   else getRouteStops' routeId' t acc


connectStopsByMinDistData :: [Stop] -> RouteId -> Either String [Stop]
connectStopsByMinDistData [] _ = Right []
connectStopsByMinDistData [stop] _ = Right [stop]
connectStopsByMinDistData (stop:rest) route' =
  let
    connectedStops = connectStopsByMinDist' stop stop rest [] 
    in case connectedStops of
      Left e1 -> Left e1
      Right connectedStops' -> Right(connectedStops')


  where
    connectStopsByMinDist' prev' stop' [] acc =
      let
        newPrevStop = parseSetPreviousStopData stop' route' prev'
        in case newPrevStop of
          Left e1 -> Left e1
          Right newPrevStop' -> Right(acc ++ [newPrevStop'])

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

-- <connect_route_stops_by_min_dist> ::= "connect_route_stops_by_min_dist(" <route_id> ")"
connectRouteStopsByMinDist :: [Stop] -> Parser [Stop]
connectRouteStopsByMinDist _ [] = Left "empty input, cannot parse a connect route stops by min dist"
connectRouteStopsByMinDist stops' input =
  let
    res = and3' (\_ a _ -> a) (parseExact "connect_route_stops_by_min_dist(") parseRouteId (parseExact ")") input
    in case res of
    Left e1 -> Left e1
    Right (a', v1) -> 
      let
        routeStops = getRouteStops a' stops'
        in case routeStops of
          Left e1 -> Left e1
          Right routeStops' -> 
            let
              connectedStops = connectStopsByMinDistData routeStops' a'
              in case connectedStops of
                  Left e1 -> Left e1
                  Right connectedStops' -> Right (connectedStops', v1)

-- <join_two_routes> ::= "join_two_routes(" <route> ", " <route> ", " <new_route_id> ", " <new_name> ")"
parseJoinTwoRoutes :: [Route] -> [Stop] -> Parser (Route, [Stop])
parseJoinTwoRoutes _ _ [] = Left "empty input, cannot parse a join two routes"
parseJoinTwoRoutes routes' stops' input =
  let
    res = parseExact "join_two_routes(" input
    in case res of
      Left e1 -> Left e1
      Right (_, r1) -> 
        let
          route1 = parseRouteId r1
          in case route1 of
            Left e1 -> Left e1
            Right (v1, r2) -> 
              let
                route2 = parseRouteId r2
                in case route2 of
                  Left e2 -> Left e2
                  Right (v2, r3) -> 
                    let
                      newRouteId = parseRouteId r3
                      in case newRouteId of
                        Left e3 -> Left e3
                        Right (v3, r4) -> 
                          let
                            newName = parseName r4
                            in case newName of
                              Left e4 -> Left e4
                              Right (v4, r5) -> 
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
                                                            Right foundStops -> Right ((newRoute, foundStops), r5)


-- <stop_or_path_or_creat> ::= <create_stop> | <stop_or_path> | <find_next_stop> | <find_previous_stop>
parseStopOrPathOrCreate :: Parser StopOrPath
parseStopOrPathOrCreate [] = Left "empty input, cannot parse a stop or path or create"
parseStopOrPathOrCreate input =
  let
    stopOrPath = parseStopOrPath input
    in case stopOrPath of
      Right (v1, r1) -> Right (v1, r1)
      Left _ ->
        let
          createStop = parseCreateStop input
          in case createStop of
            Right (v1, r1) -> 
              Right (StopId' (case v1 of (Stop sid _ _ _ _ _) -> sid), r1)
            Left e1 -> let
              findNextStop = parseFindNextStop [] input
              in case findNextStop of
                Right (v1, r1) -> Right (StopId' v1, r1)
                Left e2 -> 
                  let
                    findPreviousStop = parseFindPreviousStop [] input
                    in case findPreviousStop of
                      Right (v1, r1) -> Right (StopId' v1, r1)
                      Left e3 -> Left e3


-- <list_of_stops_paths_creat> ::= <stop_or_path_or_creat> "," <list_of_stops_paths_creat> | <stop_or_path_or_creat> 
parseStopOrPathOrCreateList :: Parser [StopOrPath]
parseStopOrPathOrCreateList [] = Left "empty input, cannot parse a stop or path or create list"
parseStopOrPathOrCreateList input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input acc =
      case parseStopOrPathOrCreate input of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ ->
          case parseSeperator input of
            Right (_, r1) -> many' r1 acc
            Left e1 -> Right (acc, input)

-- <stop_or_creat_or_nextprev> ::= <create_stop> | <stop_id> | <find_next_stop> | <find_previous_stop>
parseStopOrCreateOrNextPrev :: Parser StopId
parseStopOrCreateOrNextPrev [] = Left "empty input, cannot parse a stop or create or next prev"
parseStopOrCreateOrNextPrev input =
  let
      stop = parseStopId input
      in case stop of
        Right (v1, r1) -> Right (v1, r1)
        Left _ ->
          let
            createStop = parseCreateStop input
            in case createStop of
              Right (v1, r1) -> 
                Right (case v1 of (Stop sid _ _ _ _ _) -> sid, r1)
              Left e1 -> let
                findNextStop = parseFindNextStop [] input
                in case findNextStop of
                  Right (v1, r1) -> Right (v1, r1)
                  Left e2 -> 
                    let
                      findPreviousStop = parseFindPreviousStop [] input
                      in case findPreviousStop of
                        Right (v1, r1) -> Right (v1, r1)
                        Left e3 -> Left e3

data Trip = Trip TripId Name [StopOrPath] deriving (Show, Eq)

-- <create_trip> ::= "create_trip(" <trip_id> ", " <name> ", " <list_of_stops_paths_creat> ")"
parseCreateTrip :: Parser Trip
parseCreateTrip [] = Left "empty input, cannot parse a create trip"
parseCreateTrip input =
  let
    res = and3' (\a b c -> Trip a b c)
          (and3' (\_ b _ -> b) (parseExact "create_trip(") parseTripId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator parseStopOrPathOrCreateList (parseExact ")")) input
    in case res of
    Left e1 -> Left e1
    Right (r1, v1) -> Right (r1, v1)

data Path = Path PathId Name PathLenght StopId StopId deriving (Show, Eq)

-- <create_path> ::= "create_path(" <path_id> ", " <name> ", " <path_length> ", " <stop_id> ", " <stop_id> ")"
parseCreatePath :: Parser Path
parseCreatePath [] = Left "empty input, cannot parse a create path"
parseCreatePath input =
  let
    res = and3' (\a b c -> Path a (fst b) (snd b) (fst c) (snd c))
          (and3' (\_ b _ -> b) (parseExact "create_path(") parsePathId parseSeperator)
          (and3' (\a b _ -> (a, b)) parseName (and2' (\_ b -> b) parseSeperator parsePathLenght) parseSeperator)
          (and3' (\a b _ -> (a, b)) parseStopId (and2' (\_ b -> b) parseSeperator parseStopId) (parseExact ")") ) input
    in case res of
    Left e1 -> Left e1
    Right (r1, v1) -> Right (r1, v1)

-- <trip> ::= <trip_id> | <create_trip>
parseTripIdOrCreate :: Parser TripId
parseTripIdOrCreate [] = Left "empty input, cannot parse a trip id or create"
parseTripIdOrCreate input =
  let
    tripId = parseTripId input
    in case tripId of
      Right (v1, r1) -> Right (v1, r1)
      Left _ ->
        let
          createTrip = parseCreateTrip input
          in case createTrip of
            Right (v1, r1) -> 
              Right (case v1 of (Trip tid _ _) -> tid, r1)
            Left e1 -> Left e1

-- <join_two_routes> ::= "join_two_routes(" <route> ", " <route> ", " <route_id> ", " <name> ")"
-- joinTwoRoutes :: Parser Route
-- joinTwoRoutes [] = Left "empty input, cannot parse a join two routes"


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



addStop :: Stop -> State -> Either String State
-- addStop stop state = state {stops = stops state ++ [stop]}
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


-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"


