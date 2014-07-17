{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Fitness
    ( matchScreen
    , getRGB8
    , getRGBA8
    ) where


import           Codec.Picture
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                 as T
import qualified Data.Vector.Storable      as V
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
comparePage targetImg imgImg = undefined
    where (target, img) = commonData (imageData targetImg) (imageData imgImg)

getRGB8 :: DynamicImage -> Either T.Text (Image PixelRGB8)
getRGB8 (ImageRGB8 i) = Right i
getRGB8 _             = Left "Not RGB8."

getRGBA8 :: DynamicImage -> Either T.Text (Image PixelRGBA8)
getRGBA8 (ImageRGBA8 i) = Right i
getRGBA8 _              = Left "Not RGBA8."

commonData :: V.Storable a => V.Vector a -> V.Vector a -> (V.Vector a, V.Vector a)
commonData x y = (f *** f) (x, y)
    where f   = V.take len
          len = min (V.length x) (V.length y)

sqDist :: Locatable a => a -> a -> Double
sqDist x y = let d = x `dist` y
             in  d * d

class Locatable a where
    dist :: a -> a -> Double
