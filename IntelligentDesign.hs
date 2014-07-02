{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module Main where


import           Codec.Picture
import           Control.Applicative
import           Control.Monad
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem                hiding (writeFile)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, writeFile)
import           System.Random.MWC
import           Text.XML

import           ID.Fitness
import           ID.GP.Types
import           ID.Html5
import           ID.Types


outputDir :: FilePath
outputDir = "output"

targetFile :: FilePath
targetFile = "plain.png"

wrapEl :: Element -> Document
wrapEl el = Document (Prologue [] Nothing []) el []


main :: IO ()
main = withSystemRandom $ \g -> do
    target' <- fmap ((,) targetFile) <$> readImage (encodeString targetFile)
    case target' of
        Right target -> do
            forM_ ([1..1000] :: [Int]) $ \i -> do
                let i'      = T.justifyRight 5 '0' . T.pack $ show i
                    instDir = outputDir </> fromText i'
                    score   = instDir   </> fromText i' <.> "txt"

                createDirectory True instDir
                el <- generateElement 0.4 10 g
                fitness <- matchScreen target instDir (Gene i' Spontaneous el)
                TIO.writeFile (encodeString score) . T.pack $ show fitness
        Left err -> error $ "Unable to read image: " ++ err
