module Config (colorChannels, bufferWidth, bufferHeight, poolName, colorFormat, image) where

import Data.ByteString.Lazy
import Relude hiding (ByteString)
import Sayland.Protocols.Wayland (Enum_wl_shm_format (Enum_wl_shm_format_argb8888))

bufferWidth :: Int -- WlInt
bufferWidth = 1920

bufferHeight :: Int -- WlInt
bufferHeight = 1080

poolName :: String
poolName = "saywallpaper-shared-pool"

colorFormat :: Enum_wl_shm_format -- WlColorFormat
colorFormat = Enum_wl_shm_format_argb8888

colorChannels :: Int -- WlInt
colorChannels = 4

-- | Rainbow image :D
image :: ByteString
image =
  generateBGRA8 $ \x y ->
    let tx = fi x / fi @Int (fi bufferWidth - 1) :: Double
        ty = fi y / fi @Int (fi bufferHeight - 1) :: Double
        b = round $ tx * 255 -- left -> right
        g = round $ ty * 255 -- top -> bottom
        r = round $ (1 - tx) * 255 -- right -> left
        a = round $ (1 - ty) * 255 -- bottom -> top
     in (b, g, r, a)
  where
    fi :: forall a b. (Integral a, Num b) => a -> b
    fi = fromIntegral
    generateBGRA8 :: (Int -> Int -> (Word8, Word8, Word8, Word8)) -> ByteString
    generateBGRA8 pixelFn =
      pack
        [ byte
        | y <- [0 .. fi bufferHeight - 1]
        , x <- [0 .. fi bufferWidth - 1]
        , let (b, g, r, a) = pixelFn x y
        , byte <- [b, g, r, a]
        ]
