{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)

import Data.ByteString ( ByteString )
import Network.Wreq ( post, responseBody )
import Data.String.Conversions
import Control.Lens
import qualified Lib2 
import Data.IORef
import Lib2 (QueryStopOrCreatOrNextPrev)



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
                            | ValidateTrip QTrip (() -> next)
                            | FindNextStop Lib2.StopId Lib2.RouteId (() -> next)
                            | FindPreviousStop Lib2.StopId Lib2.RouteId (() -> next)
                            | TripDistance QTrip (() -> next)
                            | SetNextStop Lib2.StopId Lib2.RouteId Lib2.StopId (() -> next)
                            | SetPreviousStop Lib2.StopId Lib2.RouteId Lib2.StopId (() -> next)
                            | ConnectRouteStopsByMinDistance Lib2.RouteId (() -> next)
                            | CheckIfRouteStopsConnected Lib2.RouteId (() -> next)
                            | DistanceBetweenStops Lib2.StopId Lib2.StopId (() -> next)
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

validateTrip :: QTrip -> MyDomain ()
validateTrip qt = liftF $ ValidateTrip qt id

findNextStop :: Lib2.StopId -> Lib2.RouteId -> MyDomain ()
findNextStop stopId routeId = liftF $ FindNextStop stopId routeId id

findPreviousStop :: Lib2.StopId -> Lib2.RouteId -> MyDomain ()
findPreviousStop stopId routeId = liftF $ FindPreviousStop stopId routeId id

tripDistance :: QTrip -> MyDomain ()
tripDistance qt = liftF $ TripDistance qt id

setNextStop :: Lib2.StopId -> Lib2.RouteId -> Lib2.StopId -> MyDomain ()
setNextStop stopId routeId nextStopId = liftF $ SetNextStop stopId routeId nextStopId id

setPreviousStop :: Lib2.StopId -> Lib2.RouteId -> Lib2.StopId -> MyDomain ()
setPreviousStop stopId routeId previousStopId = liftF $ SetPreviousStop stopId routeId previousStopId id

connectRouteStopsByMinDistance :: Lib2.RouteId -> MyDomain ()
connectRouteStopsByMinDistance routeId = liftF $ ConnectRouteStopsByMinDistance routeId id

checkIfRouteStopsConnected :: Lib2.RouteId -> MyDomain ()
checkIfRouteStopsConnected routeId = liftF $ CheckIfRouteStopsConnected routeId id

distanceBetweenStops :: Lib2.StopId -> Lib2.StopId -> MyDomain ()
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


instance ToCommandString (MyDomainTransport next) where
    toCommandString (CreateStop (Lib2.StopId s i) (Lib2.Name n) (Lib2.Point (Lib2.CoordX x) (Lib2.CoordY y)) next) = "CreateStop(" ++ [s] ++ show i ++ ", " ++ show n ++ ", " ++ show x ++ ", " ++ show y ++ ")"
    toCommandString (CreateRoute (Lib2.RouteId r i) (Lib2.Name n) qscnps next) = "CreateRoute(" ++ [r] ++ show i ++ ", " ++ show n ++ ", " ++ (convertQSCNPToString qscnps) ++ ")"
    toCommandString (CreatePath (Lib2.PathId p i) (Lib2.Name n) (Lib2.PathLenght l) (Lib2.StopId s1 i1) (Lib2.StopId s2 i2) next) = "CreatePath(" ++ [p] ++ show i ++ ", " ++ show n ++ ", " ++ show l ++ ", " ++ show s1 ++ show i1 ++ ", " ++ show s2 ++ show i2 ++ ")"
    toCommandString (CreateTrip (Lib2.TripId t i) (Lib2.Name n) qpscnp next) = "CreateTrip(" ++ [t] ++ show i ++ ", " ++ show n ++ ", " ++ (convertQPSCNPToString qpscnp) ++ ")"
    toCommandString (JoinTwoTrips qt1 qt2 (Lib2.TripId t i) (Lib2.Name n) next) = "JoinTwoTrips(" ++ [t] ++ show i ++ ", " ++ show n ++ ", " ++ (toCommandString qt1) ++ ", " ++ (toCommandString qt2) ++ ")"
    toCommandString (JoinTwoRoutes qr1 qr2 (Lib2.RouteId r i) (Lib2.Name n) next) = "JoinTwoRoutes(" ++ [r] ++ show i ++ ", " ++ show n ++ ", " ++ (toCommandString qr1) ++ ", " ++ (toCommandString qr2) ++ ")"
    toCommandString (JoinTwoRoutesAtStop qr1 qr2 qscnp (Lib2.RouteId r i) (Lib2.Name n) next) = "JoinTwoRoutesAtStop(" ++ [r] ++ show i ++ ", " ++ show n ++ ", " ++ (toCommandString qr1) ++ ", " ++ (toCommandString qr2) ++ ", " ++ (toCommandString qscnp) ++ ")"
    toCommandString (CleanupTrip qt next) = "CleanupTrip(" ++ (toCommandString qt) ++ ")"
    toCommandString (ValidateTrip qt next) = "ValidateTrip(" ++ (toCommandString qt) ++ ")"
    toCommandString (FindNextStop (Lib2.StopId s i) (Lib2.RouteId r i1) next) = "FindNextStop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ")"
    toCommandString (FindPreviousStop (Lib2.StopId s i) (Lib2.RouteId r i1) next) = "FindPreviousStop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ")"
    toCommandString (TripDistance qt next) = "TripDistance(" ++ (toCommandString qt) ++ ")"
    toCommandString (SetNextStop (Lib2.StopId s i) (Lib2.RouteId r i1) (Lib2.StopId s1 i2) next) = "SetNextStop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ", " ++ show s1 ++ show i2 ++ ")"
    toCommandString (SetPreviousStop (Lib2.StopId s i) (Lib2.RouteId r i1) (Lib2.StopId s1 i2) next) = "SetPreviousStop(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ", " ++ show s1 ++ show i2 ++ ")"
    toCommandString (ConnectRouteStopsByMinDistance (Lib2.RouteId r i) next) = "ConnectRouteStopsByMinDistance(" ++ [r] ++ show i ++ ")"
    toCommandString (CheckIfRouteStopsConnected (Lib2.RouteId r i) next) = "CheckIfRouteStopsConnected(" ++ [r] ++ show i ++ ")"
    toCommandString (DistanceBetweenStops (Lib2.StopId s i) (Lib2.StopId s1 i1) next) = "DistanceBetweenStops(" ++ [s] ++ show i ++ ", " ++ [s1] ++ show i1 ++ ")"
    toCommandString (AssignStopToRoute (Lib2.StopId s i) (Lib2.RouteId r i1) next) = "AssignStopToRoute(" ++ [s] ++ show i ++ ", " ++ [r] ++ show i1 ++ ")"

    toCommandString (View _) = "VIEW"
    toCommandString (Save _) = "SAVE"
    toCommandString (Load _) = "LOAD"
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

interpretHttp :: MyDomain a -> IO a
interpretHttp (Pure a) = return a
interpretHttp (Free step) = do
    next <- runStep step
    interpretHttp next

    where
        runStep :: MyDomainTransport a -> IO a
        runStep a = do
            let commandString = toCommandString a
            putStrLn $ "Sending command: " ++ commandString

            resp <- post "http://localhost:3000" (cs commandString :: ByteString)
            let serverResponse = cs $ resp ^. responseBody :: String
            putStrLn $ "Server response: " ++ serverResponse

            case a of
                CreateStop _ _ _ next -> return $ next ()
                CreateRoute _ _ _ next -> return $ next ()
                CreatePath _ _ _ _ _ next -> return $ next ()
                CreateTrip _ _ _ next -> return $ next ()
                JoinTwoTrips _ _ _ _ next -> return $ next ()
                JoinTwoRoutes _ _ _ _ next -> return $ next ()
                JoinTwoRoutesAtStop _ _ _ _ _ next -> return $ next ()
                CleanupTrip _ next -> return $ next ()
                ValidateTrip _ next -> return $ next ()
                FindNextStop _ _ next -> return $ next ()
                FindPreviousStop _ _ next -> return $ next ()
                TripDistance _ next -> return $ next ()
                SetNextStop _ _ _ next -> return $ next ()
                SetPreviousStop _ _ _ next -> return $ next ()
                ConnectRouteStopsByMinDistance _ next -> return $ next ()
                CheckIfRouteStopsConnected _ next -> return $ next ()
                DistanceBetweenStops _ _ next -> return $ next ()
                AssignStopToRoute _ _ next -> return $ next ()
                View next -> return $ next ()
                Save next -> return $ next ()
                Load next -> return $ next ()

-- >>> interpretHttp program
-- ("","")
program :: MyDomain (String, String)
program = do
    createStop (Lib2.StopId 'S' 1) (Lib2.Name "StopName") (Lib2.Point (Lib2.CoordX 10.0) (Lib2.CoordY 20.0))
    createRoute (Lib2.RouteId 'R' 2) (Lib2.Name "RouteName") [Stop (Lib2.StopId 'S' 1)]
    createTrip (Lib2.TripId 'T' 1) (Lib2.Name "R") [Path (Lib2.PathId 'P' 1)]
    joinTwoTrips (Trip (Lib2.TripId 'T' 2)) (Trip (Lib2.TripId 'T' 3)) (Lib2.TripId 'T' 99) (Lib2.Name "1X4R")
    save
    return ("", "")



main :: IO ()
main = do
    interpretHttp program >>= print
