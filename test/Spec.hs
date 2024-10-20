{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    -- Test parsing an empty string
    testCase "Parsing empty string returns error" $
      Lib2.parseQuery "" @?= Left "Empty input is not a valid query",

    -- Test parsing invalid input
    testCase "Parsing an invalid input returns error" $
      Lib2.parseQuery "o" @?= Left "Unrecognized query format",

    -- Test parsing query with extra character after
    testCase "Parsing a query with extra character after returns error" $
      Lib2.parseQuery "create_stop(S1, PlsHelp, 0.55, 0.66)a" @?= Left "Extra characters after the query", 

    -- Test parsing a valid create_stop query
    testCase "Parsing a valid create_stop query" $
      Lib2.parseQuery "create_stop(S1, StopName, 10.0, 20.0)" @?= 
        Right (Lib2.CreateStop (Lib2.StopId 'S' 1) (Lib2.Name "StopName") (Lib2.Point (Lib2.CoordX 10.0) (Lib2.CoordY 20.0))),

    -- Test parsing a valid create_route query
    testCase "Parsing a valid create_route query" $
      Lib2.parseQuery "create_route(R2, RouteName, S1, S2, S3)" @?= 
        Right (Lib2.CreateRoute (Lib2.RouteId 'R' 2) (Lib2.Name "RouteName") [Lib2.QueryStopOrCreatOrNextPrevStop (Lib2.StopId 'S' 1), Lib2.QueryStopOrCreatOrNextPrevStop  (Lib2.StopId 'S' 2), Lib2.QueryStopOrCreatOrNextPrevStop  (Lib2.StopId 'S' 3)]),

    -- Test parsing a valid create_trip query
    testCase "Parsing a valid create_trip query" $
      Lib2.parseQuery "create_trip(T1, R, S1)" @?=
        Right (Lib2.CreateTrip (Lib2.TripId 'T' 1) (Lib2.Name "R")  [Lib2.QueryStopOrPath' (Lib2.StopId' (Lib2.StopId 'S' 1))]),

    -- Test parsing a valid CreatePath query
    testCase "Parsing a valid CreatePath query" $
      Lib2.parseQuery "create_path(P1, path, 10.0, S1, S2)" @?=
        Right (Lib2.CreatePath (Lib2.PathId 'P' 1) (Lib2.Name "path") (Lib2.PathLenght 10.0) (Lib2.StopId 'S' 1) (Lib2.StopId 'S' 2) ),

    -- Test parsing a large query
    testCase "Parsing large query" $
      Lib2.parseQuery "join_two_trips(T5, create_trip(T2, B, create_stop(S65, 1, -48.952, 0.6), find_next_stop(S050, R65)), T99, 1X4R)" @?=
        Right (Lib2.JoinTwoTrips (Lib2.Trip' (Lib2.TripId 'T' 5)) (Lib2.CreateTrip' (Lib2.CreateTrip (Lib2.TripId 'T' 2) (Lib2.Name "B") [Lib2.CreateStop' (Lib2.CreateStop (Lib2.StopId 'S' 65) (Lib2.Name "1") (Lib2.Point (Lib2.CoordX (-48.952)) (Lib2.CoordY 0.6))),Lib2.FindNextStop' (Lib2.FindNextStop (Lib2.StopId 'S' 50) (Lib2.RouteId 'R' 65))])) (Lib2.TripId 'T' 99) (Lib2.Name "1X4R"))

  ]