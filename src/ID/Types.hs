{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Types
    ( IDState(..)
    , idStateGen
    , idGen

    , IDConfig(..)
    , idConfigMutation
    , idConfigPopulation
    , idConfigMating
    , idConfigGenerate
    , idConfigMaxDepth
    , idConfigOutput

    , ID(..)
    , runID
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Filesystem.Path.CurrentOS
import           Prelude                    hiding (FilePath)
import           System.Random.MWC


data IDState = IDState
             { _idStateGen :: GenIO
             , _idGen      :: !Int
             }
makeLenses ''IDState

data IDConfig = IDConfig
              { _idConfigMutation   :: !Double
              , _idConfigPopulation :: !Int
              , _idConfigMating     :: !Double
              , _idConfigGenerate   :: !Double
              , _idConfigMaxDepth   :: !Int
              , _idConfigOutput     :: !FilePath
              }
makeLenses ''IDConfig

newtype ID a = ID { unID :: ReaderT IDConfig (StateT IDState IO) a }
               deriving ( Functor, Applicative, Monad, MonadIO
                        , MonadReader IDConfig, MonadState IDState)

runID :: ID a -> IDConfig -> IO (a, Int)
runID m c =   fmap (fmap _idGen)
          .   runStateT (runReaderT (unID m) c)
          .   (`IDState` 0)
          =<< create
