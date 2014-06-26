{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}


module ID.Types
    ( Mutatable(..)
    , Mateable(..)
    , Generateable(..)

    , IDState(..)
    , idStateGen
    , idGen

    , IDConfig(..)
    , idConfigMutation
    , idConfigPopulation
    , idConfigMating
    , idConfigOutput

    , ID(..)
    , runID

    , GeneId
    , Genesis(..)

    , Gene(..)
    , geneId
    , geneGenesis
    , geneData

    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
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

type GeneId = T.Text

data Genesis = Spontaneous | Copy GeneId | Mated GeneId GeneId
             deriving (Eq)

data Gene a = Gene
            { _geneId      :: !GeneId
            , _geneGenesis :: !Genesis
            , _geneData    :: !a
            }
            deriving (Eq, Functor)
makeLenses ''Gene

class Mutatable a where
    mutate :: a -> ID a

class Mateable a where
    mate :: a -> a -> ID a

class Generateable a where
    generate :: ID a

