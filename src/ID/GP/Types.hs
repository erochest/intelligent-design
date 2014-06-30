{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}


module ID.GP.Types
    ( GeneId
    , Genesis(..)

    , Gene(..)
    , geneId
    , geneGenesis
    , geneData

    , GeneData(..)
    ) where


import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import           System.Random.MWC
import           Text.XML

import           ID.Html5
import           ID.Types


type GeneId  = T.Text
data Genesis = Spontaneous | Copy GeneId | XOver GeneId GeneId
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
    generate = do
        maxDepth <- asks _idConfigMaxDepth
        withGen (`generateElement` maxDepth) _idConfigGenerate
    mutate   = undefined
    mate     = undefined

