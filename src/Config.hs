module Config where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Semigroup
import Data.Text (Text)

import Options.Applicative

-- This structure contains the general configuration options describing how
-- the compilation should take place
data Config = Config

newtype Env m a = Env (ReaderT Config m a)

instance Functor m => Functor (Env m) where
    fmap f (Env a) = Env (fmap f a)
instance Applicative m => Applicative (Env m) where
    pure = Env . pure
    Env a <*> Env b = Env (a <*> b)
instance Alternative m => Alternative (Env m) where
    empty = Env empty
    Env a <|> Env b = Env (a <|> b)
instance Monad m => Monad (Env m) where
    Env a >>= f = Env $ (\(Env x) -> x) . f =<< a
instance MonadPlus m => MonadPlus (Env m) where
    mzero = Env mzero
    mplus (Env a) (Env b) = Env (mplus a b)

instance MonadIO m => MonadIO (Env m) where
    liftIO = Env . liftIO

runEnv :: Env m a -> Config -> m a
runEnv (Env r) a = runReaderT r a

class Monad m => HasEnv m where
    env :: m Config
instance Monad m => HasEnv (Env m) where
    env = Env ask

configParser :: Parser Config
configParser = pure Config

cmdlnParser :: Parser (Config, [String])
cmdlnParser = (,) <$> configParser
                  <*> some (strArgument $ metavar "FILES ...")

cmdlnParserInfo :: ParserInfo (Config, [String])
cmdlnParserInfo = info (cmdlnParser <**> helper)
   ( fullDesc
  <> header "cch - a toy c compiler"
   )
