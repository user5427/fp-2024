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

cmd :: TVar Lib2.MyState -> Chan Lib3.StorageOp -> String -> IO String
cmd state chan str = do
    case Lib3.parseCommand str of
        Left e -> return $ "PARSE ERROR:" ++ e
        Right (c, "") -> do
            -- Use the state and chan explicitly
            tr <- Lib3.stateTransition state c chan
            case tr of
                Left e2 -> return $ "ERROR:" ++ e2
                Right Nothing -> return "No output generated"
                Right (Just msg) -> return msg
        Right (_, r) -> return $ "PARSE ERROR: string is not fully consumed - " ++ r


main :: IO ()
main = do
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    state <- newTVarIO Lib2.emptyState
    _ <- forkIO $ Lib3.storageOpLoop chan

    scotty 3001 $ do
        post "/" $ do
            b <- body
            output  <- liftIO $ do
                putStrLn $ concat ["Request was: ", cs b]
                -- Call the new cmd with state and chan
                output <- cmd state chan (cs b)
                putStrLn $ concat ["Response is: ", output]
                return output
            text (cs output)
