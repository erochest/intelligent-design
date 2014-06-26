{-# LANGUAGE DeriveFunctor              #-}
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
    , idConfigOutput

    , ID(..)
    , runID

    , GeneId
    , Genesis(..)

    , Gene(..)
    , geneId
    , geneGenesis
    , geneData

    , GeneData(..)
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import           Filesystem.Path.CurrentOS
import           Prelude                    hiding (FilePath)
import           System.Random.MWC
import           Text.XML

import           ID.Html5


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

type GeneId  = T.Text
data Genesis = Spontaneous | Copy GeneId | Mated GeneId GeneId
             deriving (Eq)

data Gene a = Gene
            { _geneId      :: !GeneId
            , _geneGenesis :: !Genesis
            , _geneData    :: !a
            }
            deriving (Eq, Functor)
makeLenses ''Gene

class GeneData a where
    generate :: ID a
    mutate   :: a -> ID a
    mate     :: a -> a -> ID a

withGen :: (r -> GenIO -> IO a) -> (IDConfig -> r) -> ID a
withGen f ratep = do
    rate <- asks ratep
    gen  <- gets _idStateGen
    liftIO $ f rate gen

instance GeneData Element where
    generate = withGen generateElement _idConfigGenerate
    mutate   = undefined
    mate     = undefined
