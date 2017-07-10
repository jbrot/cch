{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config

import Control.Monad.IO.Class

import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text.IO as T

import Options.Applicative

compile :: (MonadIO m, HasEnv m) => String -> m ()
compile f = do
    env
    liftIO . putStrLn $ "Compiling: " <> f
    contents <- liftIO . T.readFile $ f
    liftIO . T.putStrLn $ "Contents: \n" <> contents
    return ()

main :: IO ()
main = do
    (conf, trgts) <- execParser cmdlnParserInfo
    runEnv (mapM compile trgts) conf
