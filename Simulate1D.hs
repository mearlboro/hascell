{-# LANGUAGE DeriveFunctor #-}
module Main where

import Codec.Picture
import Control.Comonad
import Data.Array
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.Word

--- Data Type
data W a = W [a] a [a] deriving (Functor, Show)

left, right :: W a -> W a
left  (W (l:ls) x    rs ) = W    ls  l (x:rs)
right (W    ls  x (r:rs)) = W (x:ls) r rs

instance Comonad W where
    extract (W _ x _) = x
    duplicate w = W (tail $ iterate left w) w (tail $ iterate right w)
    extend rule w = fmap rule $ duplicate w


--- Run a simulation
experiment :: (W a -> a) -> W a -> Int -> [ W a ]
experiment rule w n = take n $ iterate (extend rule) w

run :: (W a -> a) -> W a -> Int -> Int -> [[a]]
run rule w n d = map (list . (truncateD d)) $ experiment rule w n
    where
        list (W ls x rs) = reverse ls ++ [x] ++ rs
        truncateD d (W ls x rs) = W (take d ls) x (take d rs)


--- Generic Wolfram Rules
wolframWorld :: W Bool
wolframWorld = W (repeat False) True (repeat False)

wolframRule :: Word8 -> W Bool -> Bool
wolframRule x w = testBit x $ fromListBE (fmap extract [left w, w, right w])


--- Print to terminal
stringShow :: Word8 -> W Bool -> Int -> Int -> IO ()
stringShow r w n d = mapM_ putStrLn generations
    where
        generations = map showGen $ run (wolframRule r) w n d
        showGen cs = concat $ map showCell cs
        showCell True  = "██"
        showCell False = "  "

--- Generate image
imgShow :: Word8 -> W Bool -> Int -> Int -> DynamicImage
imgShow r w n d = ImageRGB8 $ generateImage getPixel (d * 2 + 1) n
    where
        getPixel x y = showPixel $ image ! y ! x
        image = fmap (listArray (0, d * 2)) $ listArray (0, n - 1) generations
        generations = run (wolframRule r) w n d
        showPixel True  = PixelRGB8 0x00 0x00 0x00
        showPixel False = PixelRGB8 0xff 0xff 0xff

main :: IO()
main = sequence_ $ map save [0..255]
    where
        save r = savePngImage (imgPath r) (imgShow r wolframWorld n d)
        imgPath r = "img/rule" ++ show r ++ ".png"
        rs = [0..255]
        n  = 1024
        d  = 1024
