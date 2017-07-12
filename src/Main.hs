{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Main where

import Config

import Control.Exception (tryJust)
import Control.Monad
import Control.Monad.IO.Class

import Data.Foldable (asum)
import Data.Semigroup

import Options.Applicative (execParser)

import System.IO.Error

import Text.Parsec

preplace :: Stream s m Char => String -> String -> ParsecT s u m String
preplace f r = const r <$> string f

triglyphs :: [(String, String)]
triglyphs = [ ("??=", "#"),  ("??)", "]"), ("??!", "|")
            , ("??(", "["),  ("??'", "^"), ("??>", "}")
            , ("??/", "\\"), ("??<", "{"), ("??-", "~")
            , ("\\\n", "") -- Not technically a triglyph
            ]

-- The first pass of the source file. This resolves triglyphs and
-- escaped newlines while also annotating the original positions.
firstPass :: SourceName -> String -> Either ParseError [(SourcePos, Char)]
firstPass = parse $ fmap mconcat $
    many (fmap sequence $ (,) <$> getPosition <*> (asum (try . uncurry preplace <$> triglyphs) <|> (fmap pure anyChar)))

--resolveTriglyphs :: Text -> Text
--resolveTriglyphs s = foldr (uncurry T.replace) s
--    [ ("??=", "#"),  ("??)", "]"), ("??!", "|")
--    , ("??(", "["),  ("??'", "^"), ("??>", "}")
--    , ("??/", "\\"), ("??<", "{"), ("??-", "~")
--    ]

-- This function eliminates any new line characters immediately preceded by a
-- backslash. Said backslash is also eliminated.
--combineLines :: Text -> Text
--combineLines = T.replace "\\\n" ""

compile :: (MonadIO m, MonadPlus m, HasEnv m) => String -> m ()
compile f = do
    env
    liftIO . putStrLn $ "Compiling: " <> f
    let isDNE e = if isDoesNotExistError e then Just e else Nothing
    res <- liftIO . tryJust isDNE $ readFile f
    case res of
        (Left e) -> do
            liftIO . putStrLn $ "File not found: " <> f
            return ()
        (Right t) -> do
            let contents = firstPass f t
            liftIO . putStrLn $ "Contents: \n" 
            liftIO . print $ contents
            liftIO . print $ fmap (fmap snd) contents
            return ()

main :: IO ()
main = do
    (conf, trgts) <- execParser cmdlnParserInfo
    runEnv (mapM compile trgts) conf
    pure ()
