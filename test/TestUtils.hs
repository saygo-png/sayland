module TestUtils (rainbowImage, c) where

import Data.ByteString (pack)
import Relude

fi :: forall a b. (Integral a, Num b) => a -> b
fi = fromIntegral

c :: (Coercible a b) => a -> b
c = coerce

-- | :D
rainbowImage :: Int32 -> Int32 -> ByteString
rainbowImage bufferWidth bufferHeight =
  generateBGRA8 $ \x y ->
    let tx = fi x / fi @Int (fi bufferWidth - 1) :: Double
        ty = fi y / fi @Int (fi bufferHeight - 1) :: Double
        b = round $ tx * 255 -- left -> right
        g = round $ ty * 255 -- top -> bottom
        r = round $ (1 - tx) * 255 -- right -> left
        a = round $ (1 - ty) * 255 -- bottom -> top
     in (b, g, r, a)
  where
    generateBGRA8 :: (Int -> Int -> (Word8, Word8, Word8, Word8)) -> ByteString
    generateBGRA8 pixelFn =
      pack
        [ byte
        | y <- [0 .. fi bufferHeight - 1]
        , x <- [0 .. fi bufferWidth - 1]
        , let (b, g, r, a) = pixelFn x y
        , byte <- [b, g, r, a]
        ]
