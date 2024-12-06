{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where
    
import Lib2 qualified
import Lib3 qualified

import Control.Monad.IO.Class(liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM(TVar, newTVarIO)
import Control.Monad.IO.Class ()
-- import Control.Monad.State.Strict
import Data.String.Conversions
import Control.Concurrent.Chan
import Web.Scotty



-- cmd :: String
-- cmd str = do
--   case Lib3.parseCommand str of
--     Left e -> liftIO $ putStrLn $ "PARSE ERROR:" ++ e
--     Right (c, "") -> do
--       (st, chan) <- lift get
--       tr <- liftIO $ Lib3.stateTransition st c chan
--       case tr of
--         Left e2 -> liftIO $ putStrLn $ "ERROR:" ++ e2
--         Right m -> mapM_ (liftIO . putStrLn) m
--     Right (_, r) -> liftIO $ putStrLn $ "PARSE ERROR: string is not fully consumed - " ++ r

cmd :: TVar Lib2.MyState -> Chan Lib3.StorageOp -> String -> IO ()
cmd state chan str = do
    case Lib3.parseCommand str of
        Left e -> putStrLn $ "PARSE ERROR:" ++ e
        Right (c, "") -> do
            -- Use the state and chan explicitly
            tr <- Lib3.stateTransition state c chan
            case tr of
                Left e2 -> putStrLn $ "ERROR:" ++ e2
                Right m -> mapM_ putStrLn m
        Right (_, r) -> putStrLn $ "PARSE ERROR: string is not fully consumed - " ++ r


main :: IO ()
main = do
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    state <- newTVarIO Lib2.emptyState
    _ <- forkIO $ Lib3.storageOpLoop chan

    scotty 3000 $ do
        post "/" $ do
            b <- body
            liftIO $ do
                putStrLn $ concat ["Request was: ", cs b]
                -- Call the new cmd with state and chan
                cmd state chan (cs b)
            text "This is response"
