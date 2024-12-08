

module Main (main) where

import qualified Lib4
import qualified Lib2 

-- >>> interpretHttp program
-- ("","")
program :: Lib4.MyDomain (String, String)
program = do
    Lib4.save
    Lib4.createStop (Lib2.StopId 'S' 1) (Lib2.Name "StopName") (Lib2.Point (Lib2.CoordX 10.0) (Lib2.CoordY 20.0))
    Lib4.createStop (Lib2.StopId 'S' 2) (Lib2.Name "stop") (Lib2.Point (Lib2.CoordX 15.0) (Lib2.CoordY 25.0))
    Lib4.createRoute (Lib2.RouteId 'R' 2) (Lib2.Name "route") [Lib4.Stop (Lib2.StopId 'S' 1), Lib4.Stop (Lib2.StopId 'S' 2)]
    Lib4.view
    -- createRoute (Lib2.RouteId 'R' 3) (Lib2.Name "routee") [Stop (Lib2.StopId 'S' 2)]
    -- createPath (Lib2.PathId 'P' 1) (Lib2.Name "path") (Lib2.PathLenght 10.0) (Lib2.StopId 'S' 1) (Lib2.StopId 'S' 2)
    -- view
    -- joinTwoRoutes (Route (Lib2.RouteId 'R' 2)) (Route (Lib2.RouteId 'R' 3)) (Lib2.RouteId 'R' 99) (Lib2.Name "test")
    -- view
    Lib4.connectRouteStopsByMinDistance (Lib2.RouteId 'R' 2)
    Lib4.view
    _ <- Lib4.findNextStop (Lib2.StopId 'S' 1) (Lib2.RouteId 'R' 2)
    _ <- Lib4.findPreviousStop (Lib2.StopId 'S' 2) (Lib2.RouteId 'R' 2)
    _ <- Lib4.checkIfRouteStopsConnected (Lib2.RouteId 'R' 2)
    Lib4.createTrip (Lib2.TripId 'T' 1) (Lib2.Name "trippy") [Lib4.SCNP (Lib4.Stop (Lib2.StopId 'S' 1))]
    _ <- Lib4.validateTrip (Lib4.Trip (Lib2.TripId 'T' 1))
    _ <- Lib4.distanceBetweenStops (Lib2.StopId 'S' 1) (Lib2.StopId 'S' 2)
    return ("", "")



main :: IO ()
main = do
    Lib4.smartInterpretHttp program >>= print
