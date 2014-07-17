{-# LANGUAGE OverloadedStrings #-}


module ID.Utils
    ( hoistStrError
    , hoistStrErrorM
    ) where


import Control.Error
import qualified Data.Text as T


hoistStrError :: Monad m => Either String a -> EitherT T.Text m a
hoistStrError = hoistEither . fmapL T.pack

hoistStrErrorM :: Functor f => f (Either String a) -> EitherT T.Text f a
hoistStrErrorM e = EitherT (fmap (fmapL T.pack) e)

