{-# LANGUAGE DeriveFunctor #-}
module Main where

import Codec.Picture
import Control.Comonad
import Data.Array
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.Word

--- Data Type
data V i a = V i (Array i a) deriving (Functor, Show)

instance Ix i => Comonad (V i) where
    extract (V i a)   = a ! i
    duplicate (V i a) = V i $ listArray (bounds a) (flip V a <$> (range $ bounds a))
    extend f v        = fmap f $ duplicate v 

left, right :: (Ix i, Num i) => V i a -> V i a
left  (V i a)
    | i == 0    = V (snd . bounds $ a) a
    | otherwise = V (i-1) a
right (V i a)
    | i == (snd . bounds $ a) = V 0     a
    | otherwise               = V (i+1) a

arr :: Ix i => V i a -> Array i a
arr (V _ a) = a

run :: Ix i => (V i a -> a) -> V i a -> Int -> [V i a]
run rule w n = take n $ iterate (extend rule) w


-- Show as string
stringShow :: [V Int Bool] -> [String]
stringShow ws = map (concat . map showCell . list) ws
    where 
        showCell True  = "██"
        showCell False = "  "
        list (V i a) = elems a

-- Wolfram rules
wolframWorld :: Int -> V Int Bool
wolframWorld d = V d $ listArray (0, 2*d) $ (take d $ repeat False) ++ [True] ++ (take d $ repeat False)

wolframRule :: Word8 -> V Int Bool -> Bool
wolframRule x w = testBit x $ fromListBE (fmap extract [left w, w, right w])

wolframRun :: Word8 -> Int -> Int -> IO ()
wolframRun r n d = mapM_ putStrLn generations
    where
        generations = stringShow $ run (wolframRule r) (wolframWorld d) n 


-- Show as image
imageShow :: [V Int Bool] -> Int -> Int -> DynamicImage
imageShow ws h w = ImageRGB8 $ generateImage getPixel h w
    where
        getPixel x y = showPixel $ pixelArray ! x ! y
        pixelArray = listArray (0, h-1) $ map arr ws
        showPixel True  = PixelRGB8 0x00 0x00 0x00
        showPixel False = PixelRGB8 0xff 0xff 0xff

main :: IO()
main = sequence_ $ map save [0..255]
    where
        save r = savePngImage (imgPath r) (img r)
        imgPath r = "img/rule" ++ show r ++ ".png"
        img r = imageShow (run (wolframRule r) (wolframWorld d) n) n (2*d+1)
        n = 1024
        d = 1024
