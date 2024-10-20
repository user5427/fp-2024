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
      Lib2.parseQuery "o" @?= Left "Unrecognized query format"


    -- Test parsing a valid CreateStop query
    -- testCase "Parsing a valid CreateStop query" $
    --   Lib2.parseQuery "createStop(S1 StopName (10.0, 20.0))" @?= 
    --     Right (Lib2.CreateStop 1 "StopName" 10.0 20.0 0 0)

    -- -- Test parsing a valid CreateRoute query
    -- testCase "Parsing a valid CreateRoute query" $
    --   Lib2.parseQuery "createRoute(R2 RouteName [1,2,3])" @?=
    --     Right (Lib2.CreateRoute 2  [1, 2, 3])
  ]