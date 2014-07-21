{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}
{-# OPTIONS_GHC -Wall #-}


module ID.Fitness
    ( matchScreen
    , getRGB8
    , getRGBA8
    ) where


import           Codec.Picture
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath, writeFile)
import           System.Process
import           Text.XML

import           ID.GP.Types
import           ID.Types
import           ID.Utils


matchScreen :: ReadFile PixelRGBA8 -> FilePath -> FitnessF Element
matchScreen (_, targetDImage) outputDir (Gene gId _ el) = do
    liftIO $ do
        outputHtml html el
        screenshot html base $ imageWidth targetDImage
    screen <- hoistEither . getRGBA8 =<< hoistStrErrorM (readImage $ encodeString png)
    return $ comparePage targetDImage screen
    where geneId' = decode gId
          base    = outputDir </> geneId'
          html    = base <.> "html"
          png     = decodeString $ encodeString base ++ "-full.png"
          -- json    = outputDir </> geneId' <.> "json"

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

comparePage :: Image PixelRGBA8 -> Image PixelRGBA8 -> Double
comparePage targetImg imgImg =
    sum [ dist (pixelAt imgImg x y) (pixelAt targetImg x y) | x <- [0..w], y <- [0..h] ] / c
    where w = pred $ min (imageWidth targetImg) (imageWidth imgImg)
          h = pred $ min (imageHeight targetImg) (imageHeight imgImg)
          c = fromIntegral w * fromIntegral h

dist :: PixelRGBA8 -> PixelRGBA8 -> Double
dist (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) =
    let r = fromIntegral $ r0 - r1
        g = fromIntegral $ g0 - g1
        b = fromIntegral $ b0 - b1
        a = fromIntegral $ a0 - a1
    in  r^(2 :: Int) + g^(2 :: Int) + b^(2 :: Int) + a^(2 :: Int)

getRGB8 :: DynamicImage -> Either T.Text (Image PixelRGB8)
getRGB8 (ImageRGB8 i) = Right i
getRGB8 _             = Left "Not RGB8."

getRGBA8 :: DynamicImage -> Either T.Text (Image PixelRGBA8)
getRGBA8 (ImageRGBA8 i) = Right i
getRGBA8 _              = Left "Not RGBA8."
