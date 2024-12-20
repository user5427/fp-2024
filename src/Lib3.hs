{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command(..),
    Statements(..),
    ) where

import Numeric (showFFloat)
import Control.Concurrent ( Chan, readChan, writeChan )
import Control.Concurrent.STM(STM, TVar)
import qualified Lib2
import Lib2 (Query)
import Control.Applicative
import Control.Concurrent.STM.TVar
import GHC.Conc.Sync
import Control.Concurrent.Chan

data Parser a = Parser { -- like data State...
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f functor = Parser $ \input ->
        case runParser functor input of -- like stops State 
            Left e -> Left e
            Right (v, r) -> Right (f v, r)

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = Parser $ \input -> Right (a, input)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    ff <*> fa = Parser $ \input ->
        case runParser ff input of
            Left e1 -> Left e1
            Right (f, r1) -> case runParser fa r1 of
                                Left e2 -> Left e2
                                Right (a, r2) -> Right (f a , r2)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) -> case runParser (mf a) r1 of
                                Left e2 -> Left e2
                                Right (b, r2) -> Right (b, r2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \input -> Left $ "Could not parse " ++ input
    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \inp ->
        case (runParser p1 inp) of
            Right r1 -> Right r1
            Left e1 -> case (runParser p2 inp) of
                            Right r2 -> Right r2
                            Left e2 -> Left $ "Failed twise: " ++ e1 ++ " AND " ++ e2

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save dataString dataChan -> do
      writeFile "state.txt" dataString
      writeChan dataChan ()
    Load dataChan -> do
      dataString <- readFile "state.txt"
      writeChan dataChan dataString
  storageOpLoop chan
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
  case runParser (parseLoadingCommand <|> parseStatementsCommand) input of
    Right (v1, r1) -> Right (v1, r1)
    Left e -> Left e

parseLoadingCommand :: Parser Command
parseLoadingCommand = Parser {
  runParser = \input ->
    case Lib2.parse (Lib2.parseExact "LOAD") input of
      (Right _, r) -> Right (LoadCommand, r)
      (Left _, _) -> case Lib2.parse (Lib2.parseExact "SAVE") input of
        (Right _, r) -> Right (SaveCommand, r)
        (Left e2, _) -> Left e2
}

parseStatementsCommand :: Parser Command
parseStatementsCommand = Parser {
  runParser = \input ->
    case parseStatements input of
      Right (v1, r1) -> Right (StatementCommand v1, r1)
      Left e -> Left e
}

-- Right (Batch [CreateStop StopId 'S' 5 Name "PLSWORK" Point (CoordX 0.585) (CoordY 0.866),CreateStop StopId 'S' 6 Name "stoppp" Point (CoordX 0.5585) (CoordY 0.76)],"")
-- >>> parseStatements "BEGIN\ncreate_stop(S5, PLSWORK, 0.585, 0.866); create_stop(S6, stoppp, 0.5585, 0.76); END"
-- Left "Unrecognized query format or Expected BEGIN received: BEGINcreate_stop(S5, PLSWORK, 0.585, 0.866); create_stop(S6, stoppp, 0.5585, 0.76); END"
multiStringParser :: Parser Statements
multiStringParser = Parser {
  runParser = \input ->
    do
    example <- (,) <$> (closingEND =<< (snd <$> (many' =<< initialBEGIN input))) <*> (fst <$> (many' =<< initialBEGIN input))
    case length $ fst example of
      0 -> Right (snd example, fst example)
      c -> Left $ "Input not fully parsed: " ++ show c
}
  where
      many' input' =
        let
          parseMany = parseMany' input' []
          in case parseMany of
            Left e -> Left e
            Right (v1, r1) ->
              let
                size = length v1
                in case size of
                  0 -> Left "No queries found"
                  1 -> Right (Single (head v1), r1)
                  _ -> Right (Batch v1, r1)

      parseMany' input' acc =
        case Lib2.parseQuery input' of
          Left _ -> Right (acc, input')
          Right (v1, r1) ->
            case runParser parseSeperator r1 of
              Right (_, r2) -> parseMany' r2 (acc ++ [v1])
              Left e2 -> Left e2


singleStatementParser :: Parser Statements
singleStatementParser = Parser {
  runParser = \input' ->
    case Lib2.parseQuery input' of
      Right (v1, r1) -> Right (Single v1, r1)
      Left e -> Left e
}

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  -- remove ALL NEW LINES SYMBOLS
  let input' = concatMap (\c -> if c == '\n' then " " else [c]) input
    in case runParser (multiStringParser <|> singleStatementParser) input' of
      Right (v1, r1) -> Right (v1, r1)
      Left e -> Left e



parseExact :: String -> Parser String
parseExact expected = Parser {
  runParser = \input ->
    let len = length expected
    in if take len input == expected
         then Right (expected, drop len input)
         else Left $ "Expected " ++ expected
}

parseSeperator :: Parser String
parseSeperator = (parseExact "; ") <|> (parseExact ";")


initialBEGIN :: String -> Either String String
initialBEGIN input =
  case Lib2.parse (Lib2.parseExact "BEGIN ") input of
    (Right _, r) -> Right r
    (Left e, _) -> Left e


closingEND :: String -> Either String String
closingEND input =
  case runParser ((parseExact "END ") <|> (parseExact "END")) input of
    Right (_, r) -> Right r
    Left e -> Left (e ++ "received: " ++ input)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.MyState -> Statements
marshallState states =
  let
    stops = createAllStops states
    routes = createAllRoutes states
    paths = createAllPaths states
    trips = createAllTrips states
    connestions = createAllConnections states
    in Batch (stops ++ routes ++ paths ++ trips ++ connestions)

createAllStops :: Lib2.MyState -> [Query]
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

createAllRoutes :: Lib2.MyState -> [Query] -- "create_route(R1, imabouttodiefromhaskell, S1, S2, S3)"
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

createAllPaths :: Lib2.MyState -> [Query] -- "create_path(P1, path, 1.0, S1, S2)"
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

createAllTrips :: Lib2.MyState -> [Query] -- "create_trip(T1, trip, S1, S2, P1, S3)" S1 S2 P1 S3 - list of StopOrPath
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

createAllConnections :: Lib2.MyState -> [Query] -- "set_next_stop(S1, R1, S2)" "set_previous_stop(S1, R1, S2)" SetNextStop SetPreviousStop
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
    startCommand = "BEGIN\n"
    generatedCommands = loopOverCommands batch []
    endCommand = "END\n"
    in startCommand ++ generatedCommands ++ endCommand

  where
    loopOverCommands (Batch []) acc = acc
    loopOverCommands (Batch (x:xs)) acc = loopOverCommands (Batch xs) (acc ++ (loopOverQuery x) ++ ";\n")
    loopOverCommands (Single x) acc = (acc ++ (loopOverQuery x) ++ ";\n")

    loopOverQuery (Lib2.CreateStop a@(Lib2.StopId cid id) n@(Lib2.Name name) point@(Lib2.Point x@(Lib2.CoordX xp) y@(Lib2.CoordY yp))) = "create_stop(" ++ [cid] ++ show id ++ ", " ++ name ++ ", " ++ (showFFloat Nothing xp "")  ++ ", " ++ (showFFloat Nothing yp "") ++ ")"
    loopOverQuery (Lib2.CreateRoute a@(Lib2.RouteId cid id) n@(Lib2.Name name) stops) = ---"create_route(" ++ show cid ++ show id ++ ", " ++ show name ++ ", " ++ loopOverStops stops ++ ")"
      let
        stopsOnly = map (\stop -> case stop of
                                    Lib2.QueryStopOrCreatOrNextPrevStop cid -> cid
                                    Lib2.QueryStopOrCreatOrNextPrevCreateStop _ -> error "Unexpected CreateStop"
                                    Lib2.QueryStopOrCreatOrNextPrevFindNextStop _ -> error "Unexpected FindNextStop"
                                    Lib2.QueryStopOrCreatOrNextPrevFindPreviousStop _ -> error "Unexpected FindPreviousStop") stops
        manyStops = parseManyStops' stopsOnly []
        in "create_route(" ++ [cid] ++ show id ++ ", " ++ name ++ manyStops ++ ")"

      where
        parseManyStops' [] acc = acc
        parseManyStops' [a@(Lib2.StopId cid id)] acc = acc ++ ", " ++ [cid] ++ show id
        parseManyStops' (a@(Lib2.StopId cid id):xs) acc = parseManyStops' xs (acc ++ ", " ++ [cid] ++ show id)

    loopOverQuery (Lib2.CreatePath a@(Lib2.PathId cid id) n@(Lib2.Name name) p@(Lib2.PathLenght length''') b@(Lib2.StopId cid' id') c@(Lib2.StopId cid'' id'')) = "create_path(" ++ [cid] ++ show id ++ ", " ++ name ++ ", " ++ (showFFloat Nothing length''' "") ++ ", " ++ [cid'] ++ show id' ++ ", " ++  [cid''] ++ show id'' ++ ")"
    loopOverQuery (Lib2.CreateTrip a@(Lib2.TripId cid id) n@(Lib2.Name name) stopOrPathie) = --"create_trip(" ++ show cid ++ show id ++ ", " ++ show name ++ ", " ++ loopOverStopOrPath stopOrPathie ++ ")"
      let
        stopOrPath = map (\stop -> case stop of
                                    Lib2.QueryStopOrPath' cid -> cid
                                    _ -> error "Unexpected stopOrPathie") stopOrPathie

        stringStopOrPath = parseStopOrPath stopOrPath []


        in "create_trip(" ++ [cid] ++ show id ++ ", " ++ name ++ stringStopOrPath ++ ")"

        where
          parseStopOrPath [] acc = acc
          parseStopOrPath [Lib2.StopId' a@(Lib2.StopId cid id)] acc = acc ++ ", " ++ [cid] ++ show id
          parseStopOrPath (Lib2.StopId' a@(Lib2.StopId cid id):xs) acc = parseStopOrPath xs (acc ++ ", " ++ [cid] ++ show id)
          parseStopOrPath [Lib2.PathId' a@(Lib2.PathId cid id)] acc = acc ++ ", " ++ show cid ++ show id
          parseStopOrPath (Lib2.PathId' a@(Lib2.PathId cid id):xs) acc = parseStopOrPath xs (acc ++ ", " ++ [cid] ++ show id)


    loopOverQuery (Lib2.SetNextStop a@(Lib2.StopId cid id) c@(Lib2.RouteId cid'' id'') b@(Lib2.StopId cid' id')) = "set_next_stop(" ++ [cid] ++ show id ++ ", " ++ [cid''] ++ show id'' ++ ", " ++ [cid'] ++ show id' ++ ")"
    loopOverQuery (Lib2.SetPreviousStop a@(Lib2.StopId cid id) c@(Lib2.RouteId cid'' id'') b@(Lib2.StopId cid' id')) = "set_previous_stop(" ++ [cid] ++ show id ++ ", " ++ [cid''] ++ show id'' ++ ", " ++ [cid'] ++ show id' ++ ")"
    loopOverQuery _ = error "Error in loopOverQuery"



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
stateTransition :: TVar Lib2.MyState -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar cmd ioChan = case cmd of
  LoadCommand -> do
    ackChan <- newChan
    writeChan ioChan (Load ackChan)
    loadedData <- readChan ackChan
    atomically $ case parseStatements loadedData of
      Left e2 -> return $ Left ("Load failed: " ++ e2 ++ ". \nData: " ++ loadedData)
      Right (v, _) ->
        case applyStatementsToState v (Lib2.emptyState) of
          Left e -> return $ Left ("Load failed: " ++ e)
          Right (_, s) -> do
            writeTVar stateVar s
            return $ Right (Just "Loaded")

  SaveCommand -> do
    currentState <- readTVarIO stateVar
    let statements = marshallState currentState
    let serializedState = renderStatements statements
    ackChan <- newChan
    writeChan ioChan (Save serializedState ackChan)
    _ <- readChan ackChan  -- Wait for confirmation from storageOpLoop
    return $ Right (Just "Saved")

  StatementCommand statements -> atomically $ do
    currentState <- readTVar stateVar
    case applyStatementsToState statements currentState of -- applynimo metu gaunam arba klaida arba nauja state
      Left e -> return $ Left e
      Right (m, s) -> do
        writeTVar stateVar s
        return $ Right m



applyStatementsToState :: Statements -> Lib2.MyState -> Either String (Maybe String, Lib2.MyState)
applyStatementsToState statements state = applyStatementsToState' statements state [] 0
  where
    applyStatementsToState' (Single st) state' _ _ =
      case Lib2.stateTransition state' st of
        Left e -> Left e
        Right (m, s) ->
          case m of
            Just message -> Right (Just message, s)
            Nothing -> Right (Nothing, s)

    applyStatementsToState' (Batch []) state' messages _ =
      let finalMessage = if null messages then Nothing else Just (unlines messages)
      in Right (finalMessage, state')

    applyStatementsToState' (Batch (x:xs)) state' messages depth =
      case Lib2.stateTransition state' x of
        Left e -> Left e
        Right (m, s) ->
          case m of
            Just message -> applyStatementsToState' (Batch xs) s (messages ++ ["[" ++ show depth ++ "] " ++ message]) (depth + 1)
            Nothing -> applyStatementsToState' (Batch xs) s (messages ++ ["[" ++ show depth ++ "]"]) (depth + 1)
