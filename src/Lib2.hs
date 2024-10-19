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

elementInArray :: Eq a => a -> [a] -> Bool
elementInArray _ [] = False
elementInArray target (h:t) = if target == h then True else elementInArray target t


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

and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5' f a b c d e = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) ->
                                    case e r4 of
                                        Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
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
parsePoint = (and3' (\a _ b -> Point a b) parseCoordX (parseSeperator) parseCoordY)

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

data StopOrPath = Stop' Stop
                  | Path' Path deriving (Show, Eq)

-- <stop_or_path> ::= <stop_id> | <path_id>
parseStopOrPath :: [Stop] -> [Path] -> Parser StopOrPath
parseStopOrPath stops paths input =
  let
    stopId = parseStopId input
    in case stopId of
      Right (v1, r1) ->
        let
          stop = getByExtractorFromArray v1 (\(Stop sid _ _ _ _ _) -> sid) stops
          in case stop of
            Right foundStop -> Right (Stop' foundStop, r1)
            Left e1 -> Left e1
      Left _ ->
        let
          pathId = parsePathId input
          in case pathId of
            Right (v1', r1') ->
              let
                path = getByExtractorFromArray v1' (\(Path pid _ _ _ _) -> pid) paths
                in case path of
                  Right foundPath -> Right (Path' foundPath, r1')
                  Left e1 -> Left e1
            Left e1 -> Left e1

parseStopOrPathList :: [Stop] -> [Path] -> Parser [StopOrPath]
parseStopOrPathList _ _ [] = Left "empty input, cannot parse a stop or path list"
parseStopOrPathList stops' paths' input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input acc =
      case parseStopOrPath stops' paths' input of
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
    res = and2' (flip NextStop)
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
                in case L.find (== routeId) routes' of
                  Nothing -> Left "Stop does not belong to the route"
                  Just _ -> Right (r1, v1)

-- <route_id> ", " <previous_stop_id>
parsePreviousStop :: [Stop] -> Parser PreviousStop
parsePreviousStop stops input =
  let
    res = and2' (flip PreviousStop)
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
                in case L.find (== routeId) routes' of
                  Nothing -> Left "Stop does not belong to the route"
                  Just _ -> Right (r1, v1)


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
    in case L.find (== routeId) routes' of
      Nothing -> Stop sid name point nextStops prevStops (routeId : routes')
      Just _ -> stop

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
                in case L.find (== b') routes' of
                  Just _ -> Left "Stop already belongs to the route"
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

-- same for previous stop
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


-- <set_next_stop> ::= "set_next_stop(" <stop_id> ", " <route_id> ", " <next_stop_id> ")"
parseSetNextStop :: [Stop] -> Parser Stop
parseSetNextStop _ [] = Left "empty input, cannot parse a set next stop"
parseSetNextStop stops' input =
  let
    res = and3' (\_ b _ -> b) (parseExact "set_next_stop(") parseStopId parseSeperator input
    in case res of
    Left e1 -> Left e1
    Right (a', v1) ->
      let
        stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop of
          Left e1 -> Left e1
          Right stop1' ->
            let
              stop2 = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
              in case stop2 of
                Left e1' -> Left e1'
                Right stop2' ->
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
    res = and3' (\_ b _ -> b) (parseExact "set_previous_stop(") parseStopId parseSeperator input
    in case res of
    Left e1 -> Left e1
    Right (a', v1) ->
      let
        stop = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
        in case stop of
          Left e1 -> Left e1
          Right stop' ->
            let
              stop2 = getByExtractorFromArray a' (\(Stop sid _ _ _ _ _) -> sid) stops'
              in case stop2 of
                Left e1' -> Left e1'
                Right stop2' ->
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
        nextStop = findNextStopData a' b' stops'
        in case nextStop of
          Left e1 -> Left e1
          Right foundNextStopId -> Right (foundNextStopId, v1)

-- <find_previous_stop> ::= "find_previous_stop(" <stop_id> ", " <route> ")"

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
        previousStop = findPreviousStopData a' b' stops'
        in case previousStop of
          Left e1 -> Left e1
          Right foundPreviousStopId -> Right (foundPreviousStopId, v1)

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
        newStop = Stop sid name point nextStops prevStops (routeId : routes')
        in assignStopsToRoute' t routeId (acc ++ [newStop])

data Route = Route RouteId Name [StopId] deriving (Show, Eq)

-- <create_route> ::= "create_route(" <route_id> ", " <name> ", " <list_of_stop_ids> ")"
parseCreateRoute :: [Stop] -> Parser (Route, [Stop])
parseCreateRoute _ [] = Left "empty input, cannot parse a create route"
parseCreateRoute stops input =
  let
    res = and3' ((,,))
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
distanceBetweenPoints (Point (CoordX x1) (CoordY y1)) (Point (CoordX x2) (CoordY y2)) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

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


distanceBetweenStopsData :: Stop -> Stop-> Float
distanceBetweenStopsData a b =
  let
    aPoint = (case a of (Stop _ _ a' _ _ _ ) -> a')
    bPoint = (case b of (Stop _ _ b' _ _ _ ) -> b')
    in distanceBetweenPoints aPoint bPoint

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
        in case L.find (== routeId') routesFromStop of
          Nothing -> getRouteStops' routeId' t acc
          Just _ -> getRouteStops' routeId' t (acc ++ [h])


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

joinTwoRoutesData :: [Route] ->  [Stop] -> RouteId -> RouteId -> RouteId -> Name -> Either String (Route, [Stop])
joinTwoRoutesData _ _ _ _ _ _ = Left "empty input, cannot parse a join two routes"
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

-- <join_two_routes> ::= "join_two_routes(" <route> ", " <route> ", " <new_route_id> ", " <new_name> ")"
parseJoinTwoRoutes :: [Route] -> [Stop] -> Parser (Route, [Stop])
parseJoinTwoRoutes _ _ [] = Left "empty input, cannot parse a join two routes"
parseJoinTwoRoutes routes' stops' input =
  let
    res = and5' (\a b c d _ -> (a, b, c, d))
          (and3' (\_ b _ -> b) (parseExact "join_two_routes(") (parseRoute routes' stops') parseSeperator)
          (parseRoute routes' stops')
          (and3' (\_ c _ -> c) parseSeperator parseRouteId parseSeperator)
          parseName (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right ((a', b', c', d'), r1) ->
        let
          routeAId = case a' of (Route rid _ _, _) -> rid
          routeBId = case b' of (Route rid _ _, _) -> rid
          routeA = fst a'
          routeB = fst b'
          newRouteArray = [routeA, routeB]
          stopsA = snd a'
          stopsB = snd b'
          newStopsArrayWithOld = stops' ++ stopsA ++ stopsB
          joinTwo = joinTwoRoutesData newRouteArray newStopsArrayWithOld routeAId routeBId c' d'
          in case joinTwo of
            Left e5 -> Left e5
            Right foundJoinTwo -> Right (foundJoinTwo, r1)

-- <join_two_routes_at_stop> ::= "join_two_routes_at_stop(" <route> ", " <route> ", " <stop_or_creat_or_nextprev> ", " <new_route_id> ", " <new_name> ")" -- min distance order
parseJoinTwoRoutesAtStop :: [Route] -> [Stop] -> Parser (Route, [Stop])
parseJoinTwoRoutesAtStop _ _ [] = Left "empty input, cannot parse a join two routes at stop"
parseJoinTwoRoutesAtStop routes' stops' input =
  let
    res = and5' (\a b c d e -> (a, b, c, d, e))
          (and3' (\_ b _ -> b) (parseExact "join_two_routes_at_stop(") parseRouteId parseSeperator)
          parseRouteId
          (and3' (\_ c _ -> c) parseSeperator (parseStopOrCreateOrNextPrev stops') parseSeperator)
          parseRouteId
          (and3' (\_ e _ -> e) parseSeperator parseName (parseExact ")")) input
    in case res of
      Left e1 -> Left e1
      Right ((a', b', c', d', e'), r1) ->
        let
          joinTwo = joinTwoRoutesData routes' stops' a' b' d' e'
          in case joinTwo of
            Left e6 -> Left e6
            Right foundJoinTwo ->
              let
                stops' = snd foundJoinTwo
                assignCreatedStop = assignStopsToRoute [c'] d'
                in case assignCreatedStop of
                  Left e7 -> Left e7
                  Right [] -> Left "No stops were created or assigned"
                  Right (cstop:_) ->
                    let
                      addedStop = stops' ++ [cstop]
                      stopIds = map (\(Stop sid _ _ _ _ _) -> sid) addedStop
                      newRout = Route d' e' stopIds
                      in Right ((newRout, addedStop), r1)

-- <route> ::= <route_id> | <create_route> | <join_two_routes> | <join_two_routes_at_stop>
parseRoute :: [Route] -> [Stop] -> Parser (Route, [Stop])
parseRoute _ _ [] = Left "empty input, cannot parse a route"
parseRoute routes' stops' input =
  let
    route = parseRouteId input
    in case route of
      Right (v1, r1) ->
        let
          route = getByExtractorFromArray v1 (\(Route rid _ _) -> rid) routes'
          in case route of
            Right foundRoute -> Right ((foundRoute, []), r1)
            Left e1 -> Left e1
      Left _ ->
        let
          createRoute = parseCreateRoute stops' input
          in case createRoute of
            Right (v4, r4) -> Right (v4, r4)
            Left _ ->
              let
                joinTwoRoutes = parseJoinTwoRoutes routes' stops' input
                in case joinTwoRoutes of
                  Right (v2, r2) -> Right (v2, r2)
                  Left _ ->
                    let
                      joinTwoRoutesAtStop = parseJoinTwoRoutesAtStop routes' stops' input
                      in case joinTwoRoutesAtStop of
                        Right (v3, r3) -> Right (v3, r3)
                        Left e1 -> Left e1

-- <check_if_route_stops_connected> ::= "check_if_route_stops_connected(" <route_id> ")" -- atleast n-1 previous connections and n-1 next connections
parseCheckIfRouteStopsConnected :: [Route] -> [Stop] -> Parser Bool
parseCheckIfRouteStopsConnected _ _ [] = Left "empty input, cannot parse a check if route stops connected"
parseCheckIfRouteStopsConnected routes' stops' input =
  let
    res = and2' (\a _ -> a)
          (and3' (\_ b _ -> b) (parseExact "check_if_route_stops_connected(") parseRouteId parseSeperator)
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (a', v1) ->
        let
          route = getByExtractorFromArray a' (\(Route rid _ _) -> rid) routes'
          in case route of
            Left e1 -> Left e1
            Right foundRoute@(Route _ _ stops) ->
              let
                routeStops = getStopsFromStopIdList stops' stops
                in case routeStops of
                  Left e2 -> Left e2
                  Right routeStops' ->
                    let
                      connected = checkIfRouteStopsConnectedData routeStops' 0 0 a'
                      in case connected of
                        Left e3 -> Left e3
                        Right (prevCount, nextCount) ->
                          let
                            in if prevCount >= (length routeStops' - 1) && nextCount >= (length routeStops' - 1)
                              then Right (True, v1)
                              else Right (False, v1)

                    where
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



-- <stop_or_path_or_creat> ::= <create_stop> | <stop_or_path> | <find_next_stop> | <find_previous_stop>
parseStopOrPathOrCreate :: [Stop] -> [Path] -> Parser StopOrPath
parseStopOrPathOrCreate _ _ [] = Left "empty input, cannot parse a stop or path or create"
parseStopOrPathOrCreate stops' paths' input =
  let
    stopOrPath = parseStopOrPath stops' paths' input
    in case stopOrPath of
      Right (v1, r1) -> Right (v1, r1)
      Left _ ->
        let
          createStop = parseCreateStop input
          in case createStop of
            Right (v1, r1) ->
              Right (Stop' v1, r1)
            Left e1 -> let
              findNextStop = parseFindNextStop [] input
              in case findNextStop of
                Right (v1, r1) ->
                  let
                    findNextStop = getByExtractorFromArray v1 (\(Stop sid _ _ _ _ _) -> sid) stops'
                    in case findNextStop of
                      Right foundStop -> Right (Stop' foundStop, r1)
                      Left e2 -> Left e2
                Left e2 ->
                  let
                    findPreviousStop = parseFindPreviousStop [] input
                    in case findPreviousStop of
                      Right (v1, r1) ->
                        let
                          findPreviousStop = getByExtractorFromArray v1 (\(Stop sid _ _ _ _ _) -> sid) stops'
                          in case findPreviousStop of
                            Right foundStop -> Right (Stop' foundStop, r1)
                            Left e3 -> Left e3
                      Left e3 -> Left e3


-- <list_of_stops_paths_creat> ::= <stop_or_path_or_creat> "," <list_of_stops_paths_creat> | <stop_or_path_or_creat> 
parseStopOrPathOrCreateList :: [Stop] -> [Path] -> Parser [StopOrPath]
parseStopOrPathOrCreateList _ _ [] = Left "empty input, cannot parse a stop or path or create list"
parseStopOrPathOrCreateList stop' path' input = many' input []
  where
    many' [] acc = Right (acc, [])
    many' input' acc =
      case parseStopOrPathOrCreate stop' path' input' of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ ->
          case parseSeperator input' of
            Right (_, r1) -> many' r1 acc
            Left e1 -> Right (acc, input')

-- <stop_or_creat_or_nextprev> ::= <create_stop> | <stop_id> | <find_next_stop> | <find_previous_stop>
parseStopOrCreateOrNextPrev :: [Stop] -> Parser Stop
parseStopOrCreateOrNextPrev _ [] = Left "empty input, cannot parse a stop or create or next prev"
parseStopOrCreateOrNextPrev stops' input =
  let
      stop = parseStopId input
      in case stop of
        Right (v1, r1) ->
          let
            stop = getByExtractorFromArray v1 (\(Stop sid _ _ _ _ _) -> sid) stops'
            in case stop of
              Right foundStop -> Right (foundStop, r1)
              Left e1 -> Left e1
        Left _ ->
          let
            createStop = parseCreateStop input
            in case createStop of
              Right (v1, r1) ->
                Right (v1, r1)
              Left e1 -> let
                findNextStop = parseFindNextStop stops' input
                in case findNextStop of
                  Right (v1, r1) ->
                    let
                      findNextStop' = getByExtractorFromArray v1 (\(Stop sid _ _ _ _ _) -> sid) stops'
                      in case findNextStop' of
                        Right foundStop -> Right (foundStop, r1)
                        Left e2 -> Left e2
                  Left e2 ->
                    let
                      findPreviousStop = parseFindPreviousStop stops' input
                      in case findPreviousStop of
                        Right (v1, r1) ->
                          let
                            findPreviousStop' = getByExtractorFromArray v1 (\(Stop sid _ _ _ _ _) -> sid) stops'
                            in case findPreviousStop' of
                              Right foundStop -> Right (foundStop, r1)
                              Left e3 -> Left e3
                        Left e3 -> Left e3

data Trip = Trip TripId Name [StopOrPath] deriving (Show, Eq)

-- <create_trip> ::= "create_trip(" <trip_id> ", " <name> ", " <list_of_stops_paths_creat> ")"
parseCreateTrip :: [Stop] -> [Path] -> Parser Trip
parseCreateTrip _ _ [] = Left "empty input, cannot parse a create trip"
parseCreateTrip stops' paths' input =
  let
    res = and3' Trip
          (and3' (\_ b _ -> b) (parseExact "create_trip(") parseTripId parseSeperator)
          parseName
          (and3' (\_ c _ -> c) parseSeperator (parseStopOrPathOrCreateList stops' paths') (parseExact ")")) input
    in case res of
    Left e1 -> Left e1
    Right (r1, v1) -> Right (r1, v1)

data Path = Path PathId Name PathLenght StopId StopId deriving (Show, Eq)

-- <create_path> ::= "create_path(" <path_id> ", " <name> ", " <path_length> ", " <stop_id> ", " <stop_id> ")"
parseCreatePath :: Parser Path
parseCreatePath [] = Left "empty input, cannot parse a create path"
parseCreatePath input =
  let
    res = and3' (\a b c -> uncurry (Path a (fst b) (snd b)) c)
          (and3' (\_ b _ -> b) (parseExact "create_path(") parsePathId parseSeperator)
          (and3' (\a b _ -> (a, b)) parseName (and2' (\_ b -> b) parseSeperator parsePathLenght) parseSeperator)
          (and3' (\a b _ -> (a, b)) parseStopId (and2' (\_ b -> b) parseSeperator parseStopId) (parseExact ")") ) input
    in case res of
    Left e1 -> Left e1
    Right (r1, v1) -> Right (r1, v1)

-- <trip> ::= <trip_id> | <create_trip>
parseTripIdOrCreate :: [Trip] -> [Stop] -> [Path] -> Parser Trip
parseTripIdOrCreate _ _ _ [] = Left "empty input, cannot parse a trip id or create"
parseTripIdOrCreate trips' stops' paths' input =
  let
    tripId = parseTripId input
    in case tripId of
      Right (v1, r1) ->
        let
          trip = getByExtractorFromArray v1 (\(Trip tid _ _) -> tid) trips'
          in case trip of
            Right foundTrip -> Right (foundTrip, r1)
            Left e1 -> Left e1
      Left _ ->
        let
          createTrip = parseCreateTrip stops' paths' input
          in case createTrip of
            Right (v1, r1) ->
              Right (v1, r1)
            Left e1 -> Left e1

-- <join_two_trips> ::= "join_two_trips(" <trip> ", " <trip> ", " <new_trip_id> ", " <new_name> ")"
parseJoinTwoTrips :: [Trip] -> [Stop] -> [Path] -> Parser Trip
parseJoinTwoTrips _ _ _ [] = Left "empty input, cannot parse a join two trips"
parseJoinTwoTrip trips' stops' paths' input =
  let
    res = and5' (\a b c d _ -> (a, b, c, d))
          (and3' (\_ b _ -> b) (parseExact "join_two_trips(") (parseTripIdOrCreate trips' stops' paths') parseSeperator)
          (parseTripIdOrCreate trips' stops' paths')
          (and3' (\_ c _ -> c) parseSeperator parseTripId parseSeperator)
          parseName (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right ((a', b', c', d'), r1) ->
        case (a', b') of
          (Trip _ _ a'', Trip _ _ b'') ->
            let
              newTrip = Trip c' d' (a'' ++ b'')
              in Right (newTrip, r1)
          _ -> Left "Trips not found"

-- <validate_trip> ::= "validate_trip(" <trip> ")" -- all stops and paths are connected
parseValidateTrip :: [Trip] -> [Stop] -> [Path] -> Parser Bool
parseValidateTrip _ _ _ [] = Left "empty input, cannot parse a validate trip"
parseValidateTrip trips' stops' paths' input =
  let
    res = and2' (\a _ -> a)
          (and3' (\_ b _ -> b) (parseExact "validate_trip(") (parseTripIdOrCreate trips' stops' paths') parseSeperator)
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (foundTrip@(Trip _ _ stopOrPath), v1) ->
        let
          connected = validateTripData stopOrPath
          in case connected of
            Left e2 -> Left e2
            Right connected' ->
              let
                in if connected' then Right (True, v1)
                else Right (False, v1)

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

stopConnectedWithPath :: StopOrPath -> StopOrPath -> Bool
stopConnectedWithPath sop1 sop2 =
  case (sop1, sop2) of
    (Stop' (Stop sid _ _ _ _ _), Path' (Path _ _ _ stopId1 stopId2)) ->
      sid == stopId1 || sid == stopId2
    (Path' (Path _ _ _ stopId1' stopId2'), Stop' (Stop sid _ _ _ _ _)) ->
      sid == stopId1' || sid == stopId2'
    _ -> False

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


-- <cleanup_trip> ::= "cleanup_trip(" <trip> ")"
parseCleanupTrip :: [Trip] -> [Stop] -> [Path] -> Parser Trip
parseCleanupTrip _ _ _ [] = Left "empty input, cannot parse a cleanup trip"
parseCleanupTrip trips' stops' paths' input =
  let
    res = and2' (\a _ -> a)
          (and3' (\_ b _ -> b) (parseExact "cleanup_trip(") (parseTripIdOrCreate trips' stops' paths') parseSeperator)
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (foundTrip@(Trip _ _ stopOrPath), v1) ->
        let
          validate = validateTripData stopOrPath
          in case validate of
            Right _ -> Left "Trip is already connected"
            Left e1 -> 
              let
                cleaned = cleanupTripData stopOrPath
                in case cleaned of
                  Left e2 -> Left e2
                  Right cleaned' -> Right (Trip (case foundTrip of Trip tid _ _ -> tid) (case foundTrip of Trip _ name _ -> name) cleaned', v1)

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
parseTripDistance :: [Trip] -> [Stop] -> [Path] -> Parser Float
parseTripDistance _ _ _ [] = Left "empty input, cannot parse a trip distance"
parseTripDistance trips' stops' paths' input =
  let
    res = and2' (\a _ -> a)
          (and3' (\_ b _ -> b) (parseExact "trip_distance(") (parseTripIdOrCreate trips' stops' paths') parseSeperator)
          (parseExact ")") input
    in case res of
      Left e1 -> Left e1
      Right (foundTrip@(Trip _ _ stopOrPath), v1) ->
        let
          distance = tripDistanceData stopOrPath
          in case distance of
            Left e1 -> Left e1
            Right distance' -> Right (distance', v1)

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

tripInState :: Trip -> State -> Bool
tripInState trip state = elementInArray trip (trips state)



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

stopInState :: Stop -> State -> Bool
stopInState stop state = elementInArray stop (stops state)




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

routeInState :: Route -> State -> Bool
routeInState route state = elementInArray route (routes state)



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

pathInState :: Path -> State -> Bool
pathInState path state = elementInArray path (paths state)



-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"


