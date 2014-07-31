{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Types
    ( IDState(..)
    , idStateGen
    , idGen

    , IDConfig(..)
    , idConfigPopulation
    , idConfigMutation
    , idConfigCrossOver
    , idConfigGenerate
    , idConfigMaxDepth
    , IDReader

    , ID(..)
    , runID
    , runID'

    , ReadFile
    ) where


import           Codec.Picture
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Aeson
import           Filesystem.Path.CurrentOS
import           Prelude                    hiding (FilePath)
import           System.Random.MWC


type ReadFile a = (FilePath, Image a)

data IDState = IDState
             { _idStateGen :: GenIO
             , _idGen      :: !Int
             }
makeLenses ''IDState

data IDConfig = IDConfig
              { _idConfigPopulation :: !Int
              , _idConfigMutation   :: !Double
              , _idConfigCrossOver  :: !Double
              , _idConfigGenerate   :: !Double
              , _idConfigMaxDepth   :: !Int
              } deriving (Show)
makeLenses ''IDConfig

instance FromJSON IDConfig where
    parseJSON (Object v) =  IDConfig
                        <$> v .: "populationSize"
                        <*> v .: "mutationRate"
                        <*> v .: "crossOverRate"
                        <*> v .: "generateFactor"
                        <*> v .: "maxDepth"
    parseJSON _       = mzero

type IDReader = (IDConfig, FilePath)

newtype ID a = ID { unID :: LoggingT (ReaderT IDReader (StateT IDState IO)) a }
               deriving (Functor, Applicative, Monad)

instance MonadLogger ID where
    monadLoggerLog a b c d = ID $ monadLoggerLog a b c d

instance MonadIO ID where
    liftIO = ID . lift . lift . lift

instance MonadReader IDReader ID where
    ask       = ID . lift $ ask
    local f m = do
        r <- f <$> ask
        s <- get
        (a, s') <- liftIO $ runID' m r s
        put s'
        return a
    reader    = ID . lift . reader

instance MonadState IDState ID where
    get   = ID . lift $ get
    put   = ID . lift . put
    state = ID . lift . state

runID :: ID a -> IDConfig -> FilePath -> IO (a, Int)
runID m c out =   fmap (fmap _idGen)
              .   runStateT (runReaderT (runStdoutLoggingT (unID m)) (c, out))
              .   (`IDState` 0)
              =<< create

runID' :: ID a -> IDReader -> IDState -> IO (a, IDState)
runID' m r s = runStateT (runReaderT (runStdoutLoggingT (unID m)) r) s
