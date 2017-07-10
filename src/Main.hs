{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Options.Applicative

import System.IO.Error

resolveTriglyphs :: Text -> Text
resolveTriglyphs s = foldr (uncurry T.replace) s
    [ ("??=", "#"),  ("??)", "]"), ("??!", "|")
    , ("??(", "["),  ("??'", "^"), ("??>", "}")
    , ("??/", "\\"), ("??<", "{"), ("??-", "~")
    ]

compile :: (MonadIO m, MonadPlus m, HasEnv m) => String -> m ()
compile f = do
    env
    liftIO . putStrLn $ "Compiling: " <> f
    let isDNE e = if isDoesNotExistError e then Just e else Nothing
    res <- liftIO . tryJust isDNE $ T.readFile f
    case res of
        (Left e) -> do
            liftIO . putStrLn $ "File not found: " <> f
            return ()
        (Right t) -> do
            let contents = resolveTriglyphs t
            liftIO . T.putStrLn $ "Contents: \n" <> contents
            return ()

main :: IO ()
main = do
    (conf, trgts) <- execParser cmdlnParserInfo
    runEnv (mapM compile trgts) conf
    pure ()
