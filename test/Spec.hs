{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import qualified Test.Tasty.QuickCheck as QC
import Control.Concurrent
import GHC.Conc
import Foreign.Marshal (new)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 and Lib2 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    -- Test parsing an empty string
    testCase "Parsing empty string returns error" $
      Lib2.parseQuery "" @?= Left "Empty input is not a valid query",

    -- Test parsing invalid input
    testCase "Parsing an invalid input returns error" $
      Lib2.parseQuery "o" @?= Left "Unrecognized query format",

    -- Test parsing query with extra character after
    -- testCase "Parsing a query with extra character after returns error" $
    --   Lib2.parseQuery "create_stop(S1, PlsHelp, 0.55, 0.66)a" @?= Left "Extra characters after the query", 

    -- Test parsing a valid create_stop query
    testCase "Parsing a valid create_stop query" $
      Lib2.parseQuery "create_stop(S1, StopName, 10.0, 20.0)" @?= 
        Right ((Lib2.CreateStop (Lib2.StopId 'S' 1) (Lib2.Name "StopName") (Lib2.Point (Lib2.CoordX 10.0) (Lib2.CoordY 20.0))), ""),

    -- Test parsing a valid create_route query
    testCase "Parsing a valid create_route query" $
      Lib2.parseQuery "create_route(R2, RouteName, S1, S2, S3)" @?= 
        Right ((Lib2.CreateRoute (Lib2.RouteId 'R' 2) (Lib2.Name "RouteName") [Lib2.QueryStopOrCreatOrNextPrevStop (Lib2.StopId 'S' 1), Lib2.QueryStopOrCreatOrNextPrevStop  (Lib2.StopId 'S' 2), Lib2.QueryStopOrCreatOrNextPrevStop  (Lib2.StopId 'S' 3)]), ""),

    -- Test parsing a valid create_trip query
    testCase "Parsing a valid create_trip query" $
      Lib2.parseQuery "create_trip(T1, R, S1)" @?=
        Right ((Lib2.CreateTrip (Lib2.TripId 'T' 1) (Lib2.Name "R")  [Lib2.QueryStopOrPath' (Lib2.StopId' (Lib2.StopId 'S' 1))]), ""),

    -- Test parsing a valid CreatePath query
    testCase "Parsing a valid CreatePath query" $
      Lib2.parseQuery "create_path(P1, path, 10.0, S1, S2)" @?=
        Right ((Lib2.CreatePath (Lib2.PathId 'P' 1) (Lib2.Name "path") (Lib2.PathLenght 10.0) (Lib2.StopId 'S' 1) (Lib2.StopId 'S' 2) ), ""),

    -- Test parsing a large query
    testCase "Parsing large query" $
      Lib2.parseQuery "join_two_trips(T5, create_trip(T2, B, create_stop(S65, 1, -48.952, 0.6), find_next_stop(S050, R65)), T99, 1X4R)" @?=
        Right ((Lib2.JoinTwoTrips (Lib2.Trip' (Lib2.TripId 'T' 5)) (Lib2.CreateTrip' (Lib2.CreateTrip (Lib2.TripId 'T' 2) (Lib2.Name "B") [Lib2.CreateStop' (Lib2.CreateStop (Lib2.StopId 'S' 65) (Lib2.Name "1") (Lib2.Point (Lib2.CoordX (-48.952)) (Lib2.CoordY 0.6))),Lib2.FindNextStop' (Lib2.FindNextStop (Lib2.StopId 'S' 50) (Lib2.RouteId 'R' 65))])) (Lib2.TripId 'T' 99) (Lib2.Name "1X4R")), "")
  ]


propertyTests :: TestTree
propertyTests = testGroup "state saving tests"
  [
    QC.testProperty "parse statements and renderStatements produces the same result" $
      let 
          statements = "BEGIN create_stop(S1, Seskine, 0.55, 0.66); create_stop(S2, Gelvoneliu, 1.55, 1.66); create_stop(S3, Gelvonu, 2.55, 2.66); create_stop(S4, Jovaro, 3.55, 3.66); END"
          parsed = Lib3.parseStatements statements
          in case parsed of
            Left _ -> False
            Right statements -> 
              let
                rendered = Lib3.renderStatements $ fst statements
                parse2 = Lib3.parseStatements rendered
              in case parse2 of
                Left _ -> False
                Right statements2 -> statements == statements2,
                

    QC.testProperty "state saving and loading produces the same result" $
    let 
      commands ="BEGIN create_stop(S1, Seskine, 0.55, 0.66); create_stop(S2, Gelvoneliu, 1.55, 1.66); create_stop(S3, Gelvonu, 2.55, 2.66); create_stop(S4, Jovaro, 3.55, 3.66); END"
      parsed = Lib3.parseCommand commands
      in case parsed of
        Left _ -> QC.ioProperty $ return $ QC.property False 
        Right commands -> QC.ioProperty $ do
            chan <- newChan :: IO (Chan Lib3.StorageOp)
            state <- newTVarIO Lib2.emptyState 
            _ <- forkIO $ Lib3.storageOpLoop chan
          
            _ <- Lib3.stateTransition state (fst commands) chan
            initialState <- readTVarIO state

            _ <- Lib3.stateTransition state Lib3.SaveCommand chan
            _ <- Lib3.stateTransition state Lib3.LoadCommand chan

            loadedState <- readTVarIO state

            return (initialState == loadedState),

    QC.testProperty "marshalling and unmarshalling produces the same result" $
      let 
         commands ="BEGIN create_stop(S1, Seskine, 0.55, 0.66); create_stop(S2, Gelvoneliu, 1.55, 1.66); create_stop(S3, Gelvonu, 2.55, 2.66); create_stop(S4, Jovaro, 3.55, 3.66); END"
         parsed = Lib3.parseCommand commands
          in case parsed of
            Left _ -> QC.ioProperty $ return $ QC.property False 
            Right commands -> QC.ioProperty $ do
              chan <- newChan :: IO (Chan Lib3.StorageOp)
              state <- newTVarIO Lib2.emptyState 
              _ <- forkIO $ Lib3.storageOpLoop chan

              _ <- Lib3.stateTransition state (fst commands) chan

              initialState <- readTVarIO state

              newState <- newTVarIO Lib2.emptyState
              _ <- Lib3.stateTransition newState (Lib3.StatementCommand $ Lib3.marshallState initialState) chan
  
              marshState <- readTVarIO newState
              
              return (initialState == marshState)
      
       
  ]