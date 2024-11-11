{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import qualified Test.Tasty.QuickCheck as QC
import Control.Concurrent
import GHC.Conc
import Foreign.Marshal (new)
import Test.Tasty.QuickCheck


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

-- Stop
instance Arbitrary Lib2.CoordX where
  arbitrary = Lib2.CoordX <$> suchThat (arbitrary `suchThat` (\x -> x >= 0.01 && x <= 100.0)) (\x -> not (elem 'e' (show x) || elem 'E' (show x)))

instance Arbitrary Lib2.CoordY where
  arbitrary = Lib2.CoordY <$> suchThat (arbitrary `suchThat` (\x -> x >= 0.01 && x <= 100.0)) (\x -> not (elem 'e' (show x) || elem 'E' (show x)))
  
instance Arbitrary Lib2.Point where
  arbitrary = Lib2.Point <$> arbitrary <*> arbitrary

instance Arbitrary Lib2.StopId where
  arbitrary = Lib2.StopId <$> elements ['S'] <*> (getPositive <$> arbitrary)

instance Arbitrary Lib2.Name where
  arbitrary = Lib2.Name <$> listOf1 (elements ['a'..'z'])

genStop :: Int -> Gen Lib2.Query
genStop id = do
  stopId <- Lib2.StopId <$> elements ['S'] <*> pure id
  name <- arbitrary
  point <- arbitrary
  return $ Lib2.CreateStop stopId name point

genStops :: Gen [Lib2.Query]
genStops = do
  mapM genStop [1..10]

-- Route
instance Arbitrary Lib2.RouteId where
  arbitrary = Lib2.RouteId <$> elements ['R'] <*> (getPositive <$> arbitrary)

genQueryStopOrCreatOrNextPrevStop :: [Lib2.StopId] -> Gen [Lib2.QueryStopOrCreatOrNextPrev]
genQueryStopOrCreatOrNextPrevStop stops = do
  qsocnp <- mapM (\stopId -> return $ Lib2.QueryStopOrCreatOrNextPrevStop stopId) stops
  return qsocnp

genRoute :: [Lib2.QueryStopOrCreatOrNextPrev] -> Gen Lib2.Query
genRoute stops = do
  routeId <- arbitrary
  name <- arbitrary
  return $ Lib2.CreateRoute routeId name stops

-- Path
instance Arbitrary Lib2.PathId where
  arbitrary = Lib2.PathId <$> elements ['P'] <*> (getPositive <$> arbitrary)

instance Arbitrary Lib2.PathLenght where
  arbitrary = Lib2.PathLenght <$> arbitrary

genPath :: [Lib2.StopId] -> Gen Lib2.Query
genPath stops = do
  pathId <- arbitrary
  name <- arbitrary
  pathLenght <- arbitrary
  stop1 <- elements stops
  stop2 <- elements stops
  return $ Lib2.CreatePath pathId name pathLenght stop1 stop2

-- Trip
genQueryStopOrPath :: [Lib2.StopId] -> Gen [Lib2.QueryStopOrPathOrCreate]
genQueryStopOrPath stops = do
  qsop <- mapM (\stopId -> return $ Lib2.QueryStopOrPath' (Lib2.StopId' stopId)) stops
  return qsop

instance Arbitrary Lib2.TripId where
  arbitrary = Lib2.TripId <$> elements ['T'] <*> (getPositive <$> arbitrary)

genTrip :: [Lib2.QueryStopOrPathOrCreate] -> Gen Lib2.Query
genTrip stops = do
  tripId <- arbitrary
  name <- arbitrary
  return $ Lib2.CreateTrip tripId name stops

-- QUERY GENERATOR
genMultiQuery :: Gen [Lib2.Query]
genMultiQuery = do
  stops <- genStops
  stopIds <- mapM (\(Lib2.CreateStop stopId _ _) -> return stopId) stops
  route <- genRoute =<< genQueryStopOrCreatOrNextPrevStop stopIds
  path <- genPath stopIds
  trip <- genTrip =<< genQueryStopOrPath stopIds
  return $ stops ++ [route] ++ [path] ++ [trip]


instance Arbitrary Lib2.Query where
  arbitrary :: Gen Lib2.Query
  arbitrary = oneof [ 
    Lib2.CreateStop <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Lib3.Statements where
  arbitrary = oneof [
      Lib3.Batch <$> genMultiQuery,
      Lib3.Single <$> arbitrary
    ]

instance Arbitrary Lib3.Command where
  arbitrary = oneof [
      Lib3.StatementCommand <$> arbitrary
    ]

propertyTests :: TestTree
propertyTests = testGroup "state saving tests"
  [
    QC.testProperty "parse statements and renderStatements produces the same result" $
      QC.ioProperty $ do
        randomStatements <- generate arbitrary :: IO Lib3.Statements
        let
          rendered = Lib3.renderStatements randomStatements
          parse2 = Lib3.parseStatements rendered
          in case parse2 of
          Left err -> do
            putStrLn $ "Parsing failed with error: " ++ err
            putStrLn $ "Rendered statements: " ++ rendered
            return False
          Right statements2 -> 
            if randomStatements == fst statements2
            then return True
            else do 
              putStrLn "Statements do not match after parsing and rendering."
              putStrLn $ "Original statements: " ++ show randomStatements
              putStrLn $ "Parsed statements: " ++ show (fst statements2)
              return False,
                

    QC.testProperty "state saving and loading produces the same result" $
      QC.ioProperty $ do
        chan <- newChan :: IO (Chan Lib3.StorageOp)
        state <- newTVarIO Lib2.emptyState 
        _ <- forkIO $ Lib3.storageOpLoop chan

        -- Generate a random Command using QuickCheck
        randomCommand <- generate arbitrary :: IO Lib3.Command
      
        _ <- Lib3.stateTransition state (randomCommand) chan
        initialState <- readTVarIO state
        _ <- Lib3.stateTransition state Lib3.SaveCommand chan


        newState <- newTVarIO Lib2.emptyState
        _ <- Lib3.stateTransition newState Lib3.LoadCommand chan
        loadedState <- readTVarIO newState

        return (initialState == loadedState),

    QC.testProperty "marshalling and unmarshalling produces the same result" $
         QC.ioProperty $ do
              chan <- newChan :: IO (Chan Lib3.StorageOp)
              state <- newTVarIO Lib2.emptyState 
              _ <- forkIO $ Lib3.storageOpLoop chan

              randomCommand <- generate arbitrary :: IO Lib3.Command

              _ <- Lib3.stateTransition state (randomCommand) chan

              initialState <- readTVarIO state

              newState <- newTVarIO Lib2.emptyState
              _ <- Lib3.stateTransition newState (Lib3.StatementCommand $ Lib3.marshallState initialState) chan
  
              marshState <- readTVarIO newState
              
              return (initialState == marshState)
      
       
  ]