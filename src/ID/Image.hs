{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Image
    ( imageVector
    , ImageVector
    ) where


import           Codec.Picture
import qualified Data.Vector.Storable as VS
import           Data.Word


type ImageVector = VS.Vector Double

imageVector :: (PixelValue (PixelBaseComponent a), VS.Storable (PixelBaseComponent a))
            => Image a -> ImageVector
imageVector = VS.map pixelValue . imageData

mse :: ImageVector -> ImageVector -> Double
mse = undefined

psnr :: ImageVector -> ImageVector -> Double
psnr = undefined

maxPixelValue :: Image a -> Int
maxPixelValue = undefined

class PixelValue a where
    pixelValue :: a -> Double

-- Pixel8
instance PixelValue Word8 where
    pixelValue p = undefined -- pixelValue (promotePixel p :: Pixel16)
-- Pixel16
instance PixelValue Word16 where
    pixelValue = undefined
-- PixelF
instance PixelValue Float where
    pixelValue = undefined
instance PixelValue PixelYA8 where
    pixelValue = undefined
instance PixelValue PixelYA16 where
    pixelValue = undefined
instance PixelValue PixelRGB8 where
    pixelValue = undefined
instance PixelValue PixelRGB16 where
    pixelValue = undefined
instance PixelValue PixelRGBF where
    pixelValue = undefined
instance PixelValue PixelYCbCr8 where
    pixelValue = undefined
instance PixelValue PixelCMYK8 where
    pixelValue = undefined
instance PixelValue PixelCMYK16 where
    pixelValue = undefined
instance PixelValue PixelRGBA8 where
    pixelValue = undefined
instance PixelValue PixelRGBA16 where
    pixelValue = undefined
