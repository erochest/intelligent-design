{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module Main where


import           Codec.Picture
import           Codec.Picture.Types
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem                hiding (writeFile)
import           Filesystem.Path.CurrentOS hiding (decode)
import           Prelude                   hiding (FilePath, writeFile)
import           System.Random.MWC
import           Text.XML

import           ID.Fitness
import           ID.GP.Types
import           ID.Html5
import           ID.Opts
import           ID.Types


outputDir :: FilePath
outputDir = "output"

targetFile :: FilePath
targetFile = "plain.png"

wrapEl :: Element -> Document
wrapEl el = Document (Prologue [] Nothing []) el []

procFile :: GenIO -> (FilePath, Image PixelRGBA8) -> Int -> FitnessM ()
procFile g target i = do
    let i'      = T.justifyRight 5 '0' . T.pack $ show i
        instDir = outputDir </> fromText i'
        score   = instDir   </> fromText i' <.> "txt"

    el <- liftIO $ createDirectory True instDir >> generateElement 0.4 10 g
    fitness <- matchScreen target instDir $ Gene i' Spontaneous el
    liftIO . TIO.writeFile (encodeString score) . T.pack $ show fitness


generateRandom :: IO ()
generateRandom = withSystemRandom $ \g -> eitherT onError onOK $ do
    target <- EitherT . fmap (join . bimap T.pack getRGB8) . readImage
           $  encodeString targetFile
    let target' = promoteImage target
    forM_ [1..100] $ procFile g (targetFile, target')
    where onError = TIO.putStrLn
          onOK    = const $ return ()

main :: IO ()
main = do
    opts <- execParser options
    cfg' <- fmap decodeConfig . BSL.readFile $ optConfig opts
    case cfg' of
        Just cfg -> do
            print cfg
            void $ runID greetings cfg "output"
        Nothing -> putStrLn "<ERROR>"
    where decodeConfig :: BSL.ByteString -> Maybe IDConfig
          decodeConfig = decode

          greetings :: ID ()
          greetings = $(logInfo) "How can I do something."