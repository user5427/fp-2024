{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent ( Chan )
import Control.Concurrent.STM(STM, TVar)
import qualified Lib2
import Lib2 (Query)

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop _ = do

  
  return ()

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = 
  let
    parseIT = parseStatements input
    in case parseIT of
      Right (v1, r1) -> Right (StatementCommand v1, r1)
      Left _ -> 
        case Lib2.parseExact "LOAD" input of
          Right (_, r2) -> Right (LoadCommand, r2)
          Left _ -> 
            case Lib2.parseExact "SAVE" input of
              Right (_, r3) -> Right (SaveCommand, r3)
              Left e -> Left e

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
  case initialBEGIN input of
    Right r1 -> 
      case many' r1 [] of
        Right (v2, r2) -> 
          case closingEND r2 of
            Right r3 -> Right (v2, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1
    
  where
    many' [] acc = 
      let
        size = length acc
        in case size of
          0 -> Left "No queries found"
          1 -> Right (Single (head acc), [])
          _ -> Right (Batch acc, []) 

    many' input acc =
      case Lib2.parseQuery input of
        Right (v1, r1) -> many' r1 (acc ++ [v1])
        Left _ -> 
          case Lib2.parseExact "; " input of
            Right (_, r2) -> many' r2 acc
            Left e2 -> Left e2

initialBEGIN :: String -> Either String String
initialBEGIN input = 
  case Lib2.parseExact "BEGIN " input of
    Right (_, r) -> Right r
    Left e -> Left e

closingEND :: String -> Either String String
closingEND input = 
  case Lib2.parseExact " END" input of
    Right (_, r) -> Right r
    Left e -> Left e

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState states =
  let
    stops = createAllStops states
    routes = createAllRoutes states
    paths = createAllPaths states
    trips = createAllTrips states
    connestions = createAllConnections states
    in Batch (stops ++ routes ++ paths ++ trips ++ connestions)




createAllStops :: Lib2.State -> [Query]
createAllStops state = 
  let
    stopCreatingCommand = stopCreatingCommand' (Lib2.stops state) [] -- "create_stop(S1, PlsHelp, 0.55, 0.66)"
    in stopCreatingCommand

  where 
    stopCreatingCommand' [] acc = acc
    stopCreatingCommand' (stop@(Lib2.Stop id name point _ _ _):xs) acc = 
      let 
        st = Lib2.CreateStop id name point
        in stopCreatingCommand' xs (acc ++ [st])
        
createAllRoutes :: Lib2.State -> [Query] -- "create_route(R1, imabouttodiefromhaskell, S1, S2, S3)"
createAllRoutes state = 
  let
    routeCreatingCommand = routeCreatingCommand' (Lib2.routes state) []
    in routeCreatingCommand

  where
    routeCreatingCommand' [] acc = acc
    routeCreatingCommand' (route@(Lib2.Route id name stops):xs) acc = 
      let
        transform = map (\id -> Lib2.QueryStopOrCreatOrNextPrevStop id) stops
        rt = Lib2.CreateRoute id name (transform)
        in routeCreatingCommand' xs (acc ++ [rt])

createAllPaths :: Lib2.State -> [Query] -- "create_path(P1, path, 1.0, S1, S2)"
createAllPaths state = 
  let
    pathCreatingCommand = pathCreatingCommand' (Lib2.paths state) []
    in pathCreatingCommand

  where
    pathCreatingCommand' [] acc = acc
    pathCreatingCommand' (path@(Lib2.Path id name length stop1 stop2):xs) acc = 
      let
        pt = Lib2.CreatePath id name length stop1 stop2
        in pathCreatingCommand' xs (acc ++ [pt])

createAllTrips :: Lib2.State -> [Query] -- "create_trip(T1, trip, S1, S2, P1, S3)" S1 S2 P1 S3 - list of StopOrPath
createAllTrips state = 
  let
    tripCreatingCommand = tripCreatingCommand' (Lib2.trips state) []
    in tripCreatingCommand

  where
    tripCreatingCommand' [] acc = acc
    tripCreatingCommand' (trip@(Lib2.Trip id name stopOrPathie):xs) acc = 
      let
        transform = map (\id -> Lib2.QueryStopOrPath' id) stopOrPathie
        tr = Lib2.CreateTrip id name transform
        in tripCreatingCommand' xs (acc ++ [tr])

createAllConnections :: Lib2.State -> [Query] -- "set_next_stop(S1, R1, S2)" "set_previous_stop(S1, R1, S2)" SetNextStop SetPreviousStop
createAllConnections state = 
  let
    stops = Lib2.stops state -- data Stop = Stop StopId Name Point [NextStop] [PreviousStop] [RouteId] deriving (Show, Eq)
    generateConnection = generateConnection' stops []
    in generateConnection

    where
      generateConnection' [] acc = acc
      generateConnection' (stop@(Lib2.Stop id name point nextStops previousStops routes):xs) acc = 
        let
          nextStop = map (\(Lib2.NextStop stopId routeId) -> Lib2.SetNextStop id routeId stopId) nextStops
          previousStop = map (\(Lib2.PreviousStop stopId routeId) -> Lib2.SetPreviousStop id routeId stopId) previousStops
          in generateConnection' xs (acc ++ nextStop ++ previousStop)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements batch = 
  let
    startCommand = "BEGIN "
    generatedCommands = loopOverCommands batch []
    endCommand = " END"
    in startCommand ++ show generatedCommands ++ endCommand

  where
    loopOverCommands (Batch []) acc = acc
    loopOverCommands (Batch (x:xs)) acc = loopOverCommands (Batch xs) (acc ++ [loopOverQuery x])
    loopOverCommands (Single x) acc = acc ++ [loopOverQuery x]

    loopOverQuery (Lib2.CreateStop id name point@(Lib2.Point x y)) = "create_stop(" ++ show id ++ ", " ++ show name ++ ", " ++ show x ++ ", " ++ show y ++ ")"
    loopOverQuery (Lib2.CreateRoute id name stops) = "create_route(" ++ show id ++ ", " ++ show name ++ ", " ++ loopOverStops stops ++ ")"
    loopOverQuery (Lib2.CreatePath id name length stop1 stop2) = "create_path(" ++ show id ++ ", " ++ show name ++ ", " ++ show length ++ ", " ++ show stop1 ++ ", " ++ show stop2 ++ ")"
    loopOverQuery (Lib2.CreateTrip id name stopOrPathie) = "create_trip(" ++ show id ++ ", " ++ show name ++ ", " ++ loopOverStopOrPath stopOrPathie ++ ")"
    loopOverQuery (Lib2.SetNextStop stopId routeId nextStopId) = "set_next_stop(" ++ show stopId ++ ", " ++ show routeId ++ ", " ++ show nextStopId ++ ")"
    loopOverQuery (Lib2.SetPreviousStop stopId routeId previousStopId) = "set_previous_stop(" ++ show stopId ++ ", " ++ show routeId ++ ", " ++ show previousStopId ++ ")"
    loopOverQuery _ = error "Error in loopOverQuery"

loopOverStops :: [Lib2.QueryStopOrCreatOrNextPrev] -> String
loopOverStops [] = ""
loopOverStops [x] = show x
loopOverStops (x:xs) = show x ++ ", " ++ loopOverStops xs

loopOverStopOrPath :: [Lib2.QueryStopOrPathOrCreate] -> String
loopOverStopOrPath [] = ""
loopOverStopOrPath [x] = show x
loopOverStopOrPath (x:xs) = show x ++ ", " ++ loopOverStopOrPath xs

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition _ _ ioChan = return $ Left "Not implemented 6"
