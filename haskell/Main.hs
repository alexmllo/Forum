
{-# LANGUAGE OverloadedStrings #-}

module Main
where
import Forums.App

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Control.Exception
import System.Environment

import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

-- ****************************************************************

main :: IO ()
main = do
    -- La funcio 'makeApp' (definida en el modul App) construeix una aplicacio WAI
    -- a partir d'una aplicacio de tipus Forum (instancia de WebApp de DatFw)
    r <- try makeApp
    case r of
        Right app -> do
            -- Warp adapter
            args <- getArgs
            case listToMaybe args >>= readMaybe of
                Just port -> do
                    putStrLn $ "HTTP port is " <> show port
                    run port app
                Nothing -> do
                    prog <- getProgName
                    putStrLn $ "Usage: " <> prog <> " PORT"
        Left exc -> do
            -- Exception on initialization
            putStrLn "Exception on initialization (while excution of 'makeApp'): "
            putStrLn $ "    " ++ show (exc :: SomeException)

