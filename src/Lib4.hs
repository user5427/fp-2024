{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}


module Lib4 (
    MyDomainTransport(..),
    MyDomain,
    QSCNP(..),
    QPSCNP(..),
    QTrip(..),
    QRoute(..),
    interpretHttp,
    smartInterpretHttp,
    inMemoryInterpreter,
    createStop,
    createRoute,
    createPath,
    createTrip,
    joinTwoTrips,
    joinTwoRoutes,
    joinTwoRoutesAtStop,
    cleanupTrip,
    validateTrip,
    findNextStop,
    findPreviousStop,
    tripDistance,
    setNextStop,
    setPreviousStop,
    connectRouteStopsByMinDistance,
    checkIfRouteStopsConnected,
    distanceBetweenStops,
    assignStopToRoute,
    view,
    save,
    load,
) where

import Control.Monad.Free (Free (..), liftF)

import Data.ByteString ( ByteString )
import Network.Wreq ( post, responseBody )
import Data.String.Conversions
import Control.Lens hiding (view)
import qualified Lib2

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, evalStateT)
import Control.Monad (unless)
import Data.List (intercalate)
import Text.Read.Lex (Number)


data QSCNP = Stop Lib2.StopId | CreateStop' (MyDomainTransport ()) | FindNextStop' (MyDomainTransport ()) | FindPreviousStop' (MyDomainTransport ())
data QPSCNP = Path Lib2.PathId | SCNP QSCNP
data QTrip = Trip Lib2.TripId | CreateTrip' (MyDomainTransport ())
data QRoute = Route Lib2.RouteId | CreateRoute' (MyDomainTransport ()) | JoinTwoRoutes' (MyDomainTransport ()) | JoinTwoRoutesAtStop' (MyDomainTransport ())

data MyDomainTransport next = CreateStop Lib2.StopId Lib2.Name Lib2.Point (() -> next)
                            | CreateRoute Lib2.RouteId Lib2.Name [QSCNP] (() -> next)
                            | CreatePath Lib2.PathId Lib2.Name Lib2.PathLenght Lib2.StopId Lib2.StopId (() -> next)
                            | CreateTrip Lib2.TripId Lib2.Name [QPSCNP] (() -> next)
                            | JoinTwoTrips QTrip QTrip Lib2.TripId Lib2.Name (() -> next)
                            | JoinTwoRoutes QRoute QRoute Lib2.RouteId Lib2.Name (() -> next)
                            | JoinTwoRoutesAtStop QRoute QRoute QSCNP Lib2.RouteId Lib2.Name (() -> next)
                            | CleanupTrip QTrip (() -> next)
                            | ValidateTrip QTrip (Bool -> next)
                            | FindNextStop Lib2.StopId Lib2.RouteId (Lib2.StopId -> next)
                            | FindPreviousStop Lib2.StopId Lib2.RouteId (Lib2.StopId -> next)
                            | TripDistance QTrip (Float -> next)
                            | SetNextStop Lib2.StopId Lib2.RouteId Lib2.StopId (() -> next)
                            | SetPreviousStop Lib2.StopId Lib2.RouteId Lib2.StopId (() -> next)
                            | ConnectRouteStopsByMinDistance Lib2.RouteId (() -> next)
                            | CheckIfRouteStopsConnected Lib2.RouteId (Bool -> next)
                            | DistanceBetweenStops Lib2.StopId Lib2.StopId (Float -> next)
                            | AssignStopToRoute Lib2.StopId Lib2.RouteId (() -> next)
                            | View (() -> next)
                            | Save (() -> next)
                            | Load (() -> next)
                            deriving Functor

type MyDomain = Free MyDomainTransport

createStop :: Lib2.StopId -> Lib2.Name -> Lib2.Point -> MyDomain ()
createStop stopId name point = liftF $ CreateStop stopId name point id

createRoute :: Lib2.RouteId -> Lib2.Name -> [QSCNP] -> MyDomain ()
createRoute routeId name qscnps = liftF $ CreateRoute routeId name qscnps id

createPath :: Lib2.PathId -> Lib2.Name -> Lib2.PathLenght -> Lib2.StopId -> Lib2.StopId -> MyDomain ()
createPath pathId name pathLenght stop1 stop2 = liftF $ CreatePath pathId name pathLenght stop1 stop2 id

createTrip :: Lib2.TripId -> Lib2.Name -> [QPSCNP] -> MyDomain ()
createTrip tripId name qpscnp = liftF $ CreateTrip tripId name qpscnp id

joinTwoTrips :: QTrip -> QTrip -> Lib2.TripId -> Lib2.Name -> MyDomain ()
joinTwoTrips qt1 qt2 tripId name = liftF $ JoinTwoTrips qt1 qt2 tripId name id

joinTwoRoutes :: QRoute -> QRoute -> Lib2.RouteId -> Lib2.Name -> MyDomain ()
joinTwoRoutes qr1 qr2 routeId name = liftF $ JoinTwoRoutes qr1 qr2 routeId name id

joinTwoRoutesAtStop :: QRoute -> QRoute -> QSCNP -> Lib2.RouteId -> Lib2.Name -> MyDomain ()
joinTwoRoutesAtStop qr1 qr2 qscnp routeId name = liftF $ JoinTwoRoutesAtStop qr1 qr2 qscnp routeId name id

cleanupTrip :: QTrip -> MyDomain ()
cleanupTrip qt = liftF $ CleanupTrip qt id

validateTrip :: QTrip -> MyDomain Bool
validateTrip qt = liftF $ ValidateTrip qt id

findNextStop :: Lib2.StopId -> Lib2.RouteId -> MyDomain Lib2.StopId
findNextStop stopId routeId = liftF $ FindNextStop stopId routeId id

findPreviousStop :: Lib2.StopId -> Lib2.RouteId -> MyDomain Lib2.StopId
findPreviousStop stopId routeId = liftF $ FindPreviousStop stopId routeId id

tripDistance :: QTrip -> MyDomain Float
tripDistance qt = liftF $ TripDistance qt id

setNextStop :: Lib2.StopId -> Lib2.RouteId -> Lib2.StopId -> MyDomain ()
setNextStop stopId routeId nextStopId = liftF $ SetNextStop stopId routeId nextStopId id

setPreviousStop :: Lib2.StopId -> Lib2.RouteId -> Lib2.StopId -> MyDomain ()
setPreviousStop stopId routeId previousStopId = liftF $ SetPreviousStop stopId routeId previousStopId id

connectRouteStopsByMinDistance :: Lib2.RouteId -> MyDomain ()
connectRouteStopsByMinDistance routeId = liftF $ ConnectRouteStopsByMinDistance routeId id

checkIfRouteStopsConnected :: Lib2.RouteId -> MyDomain Bool
checkIfRouteStopsConnected routeId = liftF $ CheckIfRouteStopsConnected routeId id

distanceBetweenStops :: Lib2.StopId -> Lib2.StopId -> MyDomain Float
distanceBetweenStops stopId1 stopId2 = liftF $ DistanceBetweenStops stopId1 stopId2 id

assignStopToRoute :: Lib2.StopId -> Lib2.RouteId -> MyDomain ()
assignStopToRoute stopId routeId = liftF $ AssignStopToRoute stopId routeId id

view :: MyDomain ()
view = liftF $ View id

save :: MyDomain ()
save = liftF $ Save id

load :: MyDomain ()
load = liftF $ Load id


class ToCommandString a where
    toCommandString :: a -> String

instance ToCommandString QSCNP where
    toCommandString :: QSCNP -> String
    toCommandString (Stop (Lib2.StopId s i)) = [s] ++ show i -- Assuming `StopId` has a `Show` instance
    toCommandString (CreateStop' myDomainTransport) = toCommandString myDomainTransport
    toCommandString (FindNextStop' myDomainTransport) = toCommandString myDomainTransport
    toCommandString (FindPreviousStop' myDomainTransport) = toCommandString myDomainTransport


instance ToCommandString QPSCNP where
    toCommandString :: QPSCNP -> String
    toCommandString (Path (Lib2.PathId p i)) = [p] ++ show i -- Assuming `PathId` has a `Show` instance
    toCommandString (SCNP qscnp) = toCommandString qscnp

instance ToCommandString QTrip where
    toCommandString :: QTrip -> String
    toCommandString (Trip (Lib2.TripId t i)) = [t] ++ show i -- Assuming `TripId` has a `Show` instance
    toCommandString (CreateTrip' myDomainTransport) = toCommandString myDomainTransport

instance ToCommandString QRoute where
    toCommandString :: QRoute -> String
    toCommandString (Route (Lib2.RouteId r i)) = [r] ++ show i -- Assuming `RouteId` has a `Show` instance
    toCommandString (CreateRoute' myDomainTransport) = toCommandString myDomainTransport
    toCommandString (JoinTwoRoutes' myDomainTransport) = toCommandString myDomainTransport
    toCommandString (JoinTwoRoutesAtStop' myDomainTransport) = toCommandString myDomainTransport
-- todo check if the arguments are correct type

instance ToCommandString (MyDomainTransport next) where
    toCommandString (CreateStop (Lib2.StopId s i) (Lib2.Name n) (Lib2.Point (Lib2.CoordX x) (Lib2.CoordY y)) next) = "create_stop(" ++ [s] ++ show i ++ ", " ++ n ++ ", " ++ show x ++ ", " ++ show y ++ "); "
    toCommandString (CreateRoute (Lib2.RouteId r i) (Lib2.Name n) qscnps next) = "create_route(" ++ [r] ++ show i ++ ", " ++ n ++ ", " ++ (convertQSCNPToString qscnps) ++ "); "
    toCommandString (CreatePath (Lib2.PathId p i) (Lib2.Name n) (Lib2.PathLenght l) (Lib2.StopId s1 i1) (Lib2.StopId s2 i2) next) = "create_path(" ++ [p] ++ show i ++ ", " ++ n ++ ", " ++ show l ++ ", " ++ [s1] ++ show i1 ++ ", " ++ [s2] ++ show i2 ++ "); "
    toCommandString (CreateTrip (Lib2.TripId t i) (Lib2.Name n) qpscnp next) = "create_trip(" ++ [t] ++ show i ++ ", " ++ n ++ ", " ++ (convertQPSCNPToString qpscnp) ++ "); "
    toCommandString (JoinTwoTrips qt1 qt2 (Lib2.TripId t i) (Lib2.Name n) next) = "join_two_trips(" ++ (toCommandString qt1) ++ ", " ++ (toCommandString qt2) ++ ", " ++ [t] ++ show i ++ ", " ++ n ++ "); "
    toCommandString (JoinTwoRoutes qr1 qr2 (Lib2.RouteId r i) (Lib2.Name n) next) = "join_two_routes(" ++ (toCommandString qr1) ++ ", " ++ (toCommandString qr2) ++ ", " ++ [r] ++ show i ++ ", " ++ n ++ "); "
    toCommandString (JoinTwoRoutesAtStop qr1 qr2 qscnp (Lib2.RouteId r i) (Lib2.Name n) next) = "join_two_routes_at_stop(" ++ [r] ++ show i ++ ", " ++ n ++ ", " ++ (toCommandString qr1) ++ ", " ++ (toCommandString qr2) ++ ", " ++ (toCommandString qscnp) ++ "); "
    toCommandString (CleanupTrip qt next) = "cleanup_trip(" ++ (toCommandString qt) ++ "); "
    toCommandString (ValidateTrip qt next) = "validate_trip(" ++ (toCommandString qt) ++ "); "
    toCommandString (FindNextStop (Lib2.StopId s i) (Lib2.RouteId r i1) next) = "find_next_stop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ "); "
    toCommandString (FindPreviousStop (Lib2.StopId s i) (Lib2.RouteId r i1) next) = "find_previous_stop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ "); "
    toCommandString (TripDistance qt next) = "trip_distance(" ++ (toCommandString qt) ++ "); "
    toCommandString (SetNextStop (Lib2.StopId s i) (Lib2.RouteId r i1) (Lib2.StopId s1 i2) next) = "set_next_stop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ", " ++ show s1 ++ show i2 ++ "); "
    toCommandString (SetPreviousStop (Lib2.StopId s i) (Lib2.RouteId r i1) (Lib2.StopId s1 i2) next) = "set_previous_stop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ", " ++ show s1 ++ show i2 ++ "); "
    toCommandString (ConnectRouteStopsByMinDistance (Lib2.RouteId r i) next) = "connect_route_stops_by_min_dist(" ++ [r] ++ show i ++ "); "
    toCommandString (CheckIfRouteStopsConnected (Lib2.RouteId r i) next) = "check_if_route_stops_connected(" ++ [r] ++ show i ++ "); "
    toCommandString (DistanceBetweenStops (Lib2.StopId s i) (Lib2.StopId s1 i1) next) = "distance_between_stops(" ++ [s] ++ show i ++ ", " ++ [s1] ++ show i1 ++ "); "
    toCommandString (AssignStopToRoute (Lib2.StopId s i) (Lib2.RouteId r i1) next) = "assign_stop_to_route(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ "); "

    toCommandString (View _) = "VIEW; "
    toCommandString _ = error "Unsupported command"



convertQSCNPToString :: [QSCNP] -> String
convertQSCNPToString [] = ""
convertQSCNPToString (x:xs) =
    let
        xStr = toCommandString x
        xsStr = convertQSCNPToString xs
    in if null xsStr
        then xStr
        else xStr ++ ", " ++ xsStr

convertQPSCNPToString :: [QPSCNP] -> String
convertQPSCNPToString [] = ""
convertQPSCNPToString (x:xs) =
    let
        xStr = toCommandString x
        xsStr = convertQPSCNPToString xs
    in if null xsStr
        then xStr
        else xStr ++ ", " ++ xsStr

-- send each command separately
interpretHttp :: MyDomain a -> IO a
interpretHttp (Pure a) = return a
interpretHttp (Free step) = do
    next <- runStep step
    interpretHttp next

    where
        runStep :: MyDomainTransport a -> IO a
        runStep a = do
            let
                formComString = case a of
                    Save _ -> "SAVE"
                    Load _ -> "LOAD"
                    _ -> "BEGIN " ++ toCommandString a ++ "END"

            putStrLn $ "Sending command: " ++ formComString
            resp <- post "http://localhost:3000" (cs formComString :: ByteString)
            let serverResponse = cs $ resp ^. responseBody :: String
            putStrLn $ "Server response: " ++ serverResponse

            case a of
                Save next -> return $ next ()
                Load next -> return $ next ()
                TripDistance _ _ -> processTripDistance a serverResponse
                FindNextStop _ _ _ -> processNextStop a serverResponse
                FindPreviousStop _ _ _ -> processPreviousStop a serverResponse
                CheckIfRouteStopsConnected _ _ -> processRouteConnected a serverResponse
                ValidateTrip _ _ -> processTripStopsConnected a serverResponse
                DistanceBetweenStops _ _ _ -> processDistanceBetweenStops a serverResponse
                _ -> return $ extractNext a

processTripDistance :: MyDomainTransport a -> String -> IO a
processTripDistance (TripDistance _ next) serverResponse = 
    let
        distanceStr = drop (length "Trip distance: ") serverResponse
        (result, remainingState) = Lib2.parse Lib2.parseFloat distanceStr
    in case result of
        Left e -> return $ next (-1)
        Right d -> return $ next d
processTripDistance _ _ = error "Unsupported command"

-- Process "Next stop found" command
processNextStop :: MyDomainTransport a -> String -> IO a
processNextStop (FindNextStop _ _ next) serverResponse =
    let
        stopIdStr = drop (length "Next stop found: StopId 'S' ") serverResponse
        (result, remainingState) = Lib2.parse Lib2.parseInteger stopIdStr
    in case result of
        Left e -> return $ next (Lib2.StopId 'S' (-1))
        Right stopId -> return $ next (Lib2.StopId 'S' (fromInteger stopId))
processNextStop _ _ = error "Unsupported command"

-- Process "Previous stop found" command
processPreviousStop :: MyDomainTransport a -> String -> IO a
processPreviousStop (FindPreviousStop _ _ next) serverResponse =
    let
        stopIdStr = drop (length "Previous stop found: StopId 'S' ") serverResponse
        (result, remainingState) = Lib2.parse Lib2.parseInteger stopIdStr
    in case result of
        Left e -> return $ next (Lib2.StopId 'S' (-1))
        Right stopId -> return $ next (Lib2.StopId 'S' (fromInteger stopId))
processPreviousStop _ _ = error "Unsupported command"

parseBool :: String -> Either String Bool
parseBool "True"  = Right True
parseBool "False" = Right False
parseBool _       = Left "Invalid boolean value"

-- Process "Trip stops connected" command
processTripStopsConnected :: MyDomainTransport a -> String -> IO a
processTripStopsConnected (ValidateTrip _ next) serverResponse =
    let
        boolStr = drop (length "Trip stops connected: ") serverResponse
        result = parseBool boolStr
    in case result of
        Left e -> return $ next False
        Right isConnected -> return $ next isConnected
processTripStopsConnected _ _ = error "Unsupported command"

-- Process "Distance between stops" command
processDistanceBetweenStops :: MyDomainTransport a -> String -> IO a
processDistanceBetweenStops (DistanceBetweenStops _ _ next) serverResponse =
    let
        distanceStr = drop (length "Distance between stops is ") serverResponse
        (result, remainingState) = Lib2.parse Lib2.parseFloat distanceStr
    in case result of
        Left e -> return $ next (-1)
        Right distance -> return $ next distance
processDistanceBetweenStops _ _ = error "Unsupported command"

-- Process "Route stops connected" command
processRouteConnected :: MyDomainTransport a -> String -> IO a
processRouteConnected (CheckIfRouteStopsConnected _ next) serverResponse =
    let
        boolStr = drop (length "Route stops connected: ") serverResponse
        result = parseBool boolStr
    in case result of
        Left e -> return $ next False
        Right isConnected -> return $ next isConnected
processRouteConnected _ _ = error "Unsupported command"

-- Extract the continuation from the command
extractNext :: MyDomainTransport a -> a
extractNext (CreateStop _ _ _ next) = next ()
extractNext (CreateRoute _ _ _ next) = next ()
extractNext (CreatePath _ _ _ _ _ next) = next ()
extractNext (CreateTrip _ _ _ next) = next ()
extractNext (JoinTwoTrips _ _ _ _ next) = next ()
extractNext (JoinTwoRoutes _ _ _ _ next) = next ()
extractNext (JoinTwoRoutesAtStop _ _ _ _ _ next) = next ()
extractNext (CleanupTrip _ next) = next ()
extractNext (ValidateTrip _ next) = next False
extractNext (FindNextStop _ _ next) = next (Lib2.StopId 'S' (-1))
extractNext (FindPreviousStop _ _ next) = next (Lib2.StopId 'S' (-1))
extractNext (TripDistance _ next) = next (-1)
extractNext (SetNextStop _ _ _ next) = next ()
extractNext (SetPreviousStop _ _ _ next) = next ()
extractNext (ConnectRouteStopsByMinDistance _ next) = next ()
extractNext (CheckIfRouteStopsConnected _ next) = next False
extractNext (DistanceBetweenStops _ _ next) = next (-1)
extractNext (AssignStopToRoute _ _ next) = next ()
extractNext (View next) = next ()
extractNext _ = error "Unsupported command"

type BatchState a = StateT [String] IO a

-- smart interpreter that can batch commands
smartInterpretHttp :: MyDomain a -> IO a
smartInterpretHttp a = evalStateT (runProgram a) []
    where
    runProgram :: MyDomain a -> BatchState a
    runProgram (Pure a) = do
        flushBatch -- Ensure all batched commands are sent
        return a
    runProgram (Free step) = do
        next <- processCommand step
        runProgram next

    processCommand :: MyDomainTransport a -> BatchState a
    processCommand cmd = case cmd of
        Save next -> do
            flushBatch
            _ <- lift $ sendCommand "SAVE"
            return $ next ()
        Load next -> do
            flushBatch
            _ <- lift $ sendCommand "LOAD"
            return $ next ()
        View next -> do
            flushBatch
            _ <- lift $ sendCommand "VIEW"
            return $ next ()
        TripDistance _ _ -> do
            flushBatch
            let commandStr = "BEGIN " ++ (toCommandString cmd) ++ "END"
            response <- lift $ sendCommand commandStr
            lift $ processTripDistance cmd response
        FindNextStop _ _ _ -> do
            flushBatch
            let commandStr = "BEGIN " ++ (toCommandString cmd) ++ "END"
            response <- lift $ sendCommand commandStr
            lift $ processNextStop cmd response
        FindPreviousStop _ _ _ -> do
            flushBatch
            let commandStr = "BEGIN " ++ (toCommandString cmd) ++ "END"
            response <- lift $ sendCommand commandStr
            lift $ processPreviousStop cmd response
        CheckIfRouteStopsConnected _ _ -> do
            flushBatch
            let commandStr = "BEGIN " ++ (toCommandString cmd) ++ "END"
            response <- lift $ sendCommand commandStr
            lift $ processRouteConnected cmd response
        DistanceBetweenStops _ _ _ -> do
            flushBatch
            let commandStr = "BEGIN " ++ (toCommandString cmd) ++ "END"
            response <- lift $ sendCommand commandStr
            lift $ processDistanceBetweenStops cmd response
        ValidateTrip _ _ -> do
            flushBatch
            let commandStr = "BEGIN " ++ (toCommandString cmd) ++ "END"
            response <- lift $ sendCommand commandStr
            lift $ processTripStopsConnected cmd response
        -- Batchable commands
        _ -> do
            let commandStr = toCommandString cmd
            batch <- get
            put $ commandStr : batch
            return $ extractNext cmd



    -- Flush the batch of commands
    flushBatch :: BatchState ()
    flushBatch = do
        batch <- get
        unless (null batch) $ do
            let batchCommand = "BEGIN " ++ intercalate "" (reverse batch) ++ "END"
            lift $ sendCommand batchCommand
            put []

    -- Send a single command to the server
    sendCommand :: String -> IO String
    sendCommand command = do
        putStrLn $ "Sending command(s): " ++ command
        resp <- post "http://localhost:3000" (cs command :: ByteString)
        let serverResponse = cs $ resp ^. responseBody :: String
        putStrLn $ "Server response: " ++ serverResponse
        return serverResponse



inMemoryInterpreter :: MyDomain a -> IO a
inMemoryInterpreter a = evalStateT (runProgram a) Lib2.emptyState
    where
        runProgram :: MyDomain a -> StateT Lib2.MyState IO a
        runProgram (Pure a) = return a
        runProgram (Free step) = do
            next <- runStep step
            runProgram next

        runStep :: MyDomainTransport a -> StateT Lib2.MyState IO a
        runStep a = do
            let
                formComString = case a of
                    Save _ -> "SAVE"
                    Load _ -> "LOAD"
                    _ -> take (length (toCommandString a) - 2) (toCommandString a)

            lift $ putStrLn $ "Sending command: " ++ formComString

            case a of
                Save next -> return $ next ()
                Load next -> return $ next ()

                _ -> do
                    case Lib2.parseQuery formComString of
                        Left e -> do
                            lift $ putStrLn $ "PARSE ERROR:" ++ e
                            error "PARSE ERROR"
                        Right e -> do
                            st <- get
                            case Lib2.stateTransition st (fst e) of
                                Left e2 -> do 
                                    lift $ putStrLn $ "ERROR:" ++ e2
                                    return $ extractNext a
                                Right (m, ns) -> do
                                    put ns
                                    lift $ mapM_ (putStrLn) m
                                    case m of 
                                        Just msg -> case a of
                                            TripDistance _ _ -> lift $ processTripDistance a msg
                                            FindNextStop _ _ _ -> lift $ processNextStop a msg
                                            FindPreviousStop _ _ _ -> lift $ processPreviousStop a msg
                                            CheckIfRouteStopsConnected _ _ -> lift $ processRouteConnected a msg
                                            ValidateTrip _ _ -> lift $ processTripStopsConnected a msg
                                            DistanceBetweenStops _ _ _ -> lift $ processDistanceBetweenStops a msg
                                            _ -> return $ extractNext a
                                        Nothing -> return $ extractNext a
                                
                    