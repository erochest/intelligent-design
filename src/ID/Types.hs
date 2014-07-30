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

    , ReadFile
    ) where


import           Codec.Picture
import           Control.Applicative
import           Control.Lens
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

newtype ID a = ID { unID :: ReaderT IDReader (StateT IDState IO) a }
               deriving ( Functor, Applicative, Monad, MonadIO
                        , MonadReader IDReader, MonadState IDState)

runID :: ID a -> IDConfig -> FilePath -> IO (a, Int)
runID m c out =   fmap (fmap _idGen)
              .   runStateT (runReaderT (unID m) (c, out))
              .   (`IDState` 0)
              =<< create
