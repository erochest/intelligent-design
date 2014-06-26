{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module Main where


import           Control.Monad
import           Filesystem                hiding (writeFile)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, writeFile)
import           System.IO                 hiding (FilePath, writeFile)
import           System.Random.MWC
import           Text.XML

import           ID.Html5


outputDir :: FilePath
outputDir = "output"


wrapEl :: Element -> Document
wrapEl el = Document (Prologue [] Nothing []) el []


main :: IO ()
main = withSystemRandom $ \g -> do
    createDirectory True outputDir
    forM_ ([1..1000] :: [Int]) $ \i ->
        let fn = outputDir </> decodeString (show i) <.> "html"
        in  writeFile def fn . wrapEl =<< generateElement 0.4 10 g
