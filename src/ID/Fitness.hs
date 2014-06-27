{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Fitness
    ( matchScreen
    ) where


import           Codec.Picture
import           Control.Applicative
import qualified Data.Text                 as T
import           Filesystem                hiding (writeFile)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, writeFile)
import           System.Process
import           Text.XML

import           ID.GP.Types
import           Paths_intelligent_design


matchScreen :: Image a -> FilePath -> Gene Element -> IO Double
matchScreen target outputDir g@(Gene geneId genesis el) = do
    outputHtml html el
    screenshot html png $ imageWidth target
    fitness <- comparePage target png
    return fitness
    where geneId' = decode geneId
          html    = outputDir </> geneId' <.> "html"
          png     = outputDir </> geneId' <.> "png"
          json    = outputDir </> geneId' <.> "json"

outputHtml :: FilePath -> Element -> IO ()
outputHtml html = writeFile settings html . wrapEl
    where settings  = def  -- { rsPretty = True }
          wrapEl el = Document (Prologue [] Nothing []) el []

screenshot :: FilePath -> FilePath -> Int -> IO ()
screenshot html png width = do
    rasterize_js <- getDataFileName "bin/rasterize.js"
    callProcess "phantomjs" [ rasterize_js
                            , encodeString html
                            , encodeString png
                            , show width ++ "px"
                            ]

-- TODO Compare PNG and original.
comparePage :: FilePath -> FilePath -> IO Double
comparePage = undefined

imageDistance :: Image a -> Image a -> Double
imageDistance = undefined
