{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Fitness
    ( matchScreen
    ) where


import qualified Data.Vector as V
import           Codec.Picture
import           Control.Error
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, writeFile)
import           System.Process
import           Text.XML

import           ID.GP.Types
import           ID.Types


matchScreen :: ReadFile a -> FilePath -> Gene Element -> IO Double
matchScreen (target, targetDImage) outputDir (Gene gId genesis el) = do
    outputHtml html el
    screenshot html base $ dimageWidth targetDImage
    -- targetHash <- imageHash target'
    return 42
    {-
     - maybe (return $ fromIntegral (maxBound :: Int))
     -       (`comparePage` png)
     -       targetHash
     -}
    where geneId' = decode gId
          base    = outputDir </> geneId'
          html    = base <.> "html"
          png     = decodeString $ encodeString base ++ "-full.png"
          -- json    = outputDir </> geneId' <.> "json"
          target' = encodeString target

dimageWidth :: DynamicImage -> Int
dimageWidth (ImageY8 i)     = imageWidth i
dimageWidth (ImageY16 i)    = imageWidth i
dimageWidth (ImageYF i)     = imageWidth i
dimageWidth (ImageYA8 i)    = imageWidth i
dimageWidth (ImageYA16 i)   = imageWidth i
dimageWidth (ImageRGB8 i)   = imageWidth i
dimageWidth (ImageRGB16 i)  = imageWidth i
dimageWidth (ImageRGBF i)   = imageWidth i
dimageWidth (ImageRGBA8 i)  = imageWidth i
dimageWidth (ImageRGBA16 i) = imageWidth i
dimageWidth (ImageYCbCr8 i) = imageWidth i
dimageWidth (ImageCMYK8 i)  = imageWidth i
dimageWidth (ImageCMYK16 i) = imageWidth i

outputHtml :: FilePath -> Element -> IO ()
outputHtml html = writeFile settings html . wrapEl
    where settings  = def  -- { rsPretty = True }
          wrapEl el = Document (Prologue [] Nothing []) el []

screenshot :: FilePath -> FilePath -> Int -> IO ()
screenshot html base width =
    callProcess "webkit2png" [ "-W", show width
                             , "-o", encodeString base
                             , encodeString html
                             ]

-- TODO Compare PNG and original.
{-
 - comparePage :: PHash -> FilePath -> IO Double
 - comparePage target test = do
 -     hash <- imageHash $ encodeString test
 -     return . fromIntegral $ maybe (maxBound :: Int) (hammingDistance target) hash
 -}
