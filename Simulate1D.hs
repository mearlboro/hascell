module Main where

import Codec.Picture
import Control.Comonad
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.Word

--- Data Type
data W a = W [a] a [a]

left, right :: W a -> W a
left  (W (l:ls) x    rs ) = W    ls  l (x:rs)
right (W    ls  x (r:rs)) = W (x:ls) r rs

instance Functor W where
    fmap f (W as x bs) = W (fmap f as) (f x) (fmap f bs)

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
showCell :: Bool -> String
showCell True  = "██"
showCell False = "  "

printRun :: Word8 -> W Bool -> Int -> Int -> IO ()
printRun r w n d = mapM_ putStrLn generations
    where
        generations = map showGen $ run (wolframRule r) w n d
        showGen cs = concat $ map showCell cs


--- Print to image
showPixel :: Bool -> PixelRGB8
showPixel True  = PixelRGB8 0x00 0x00 0x00
showPixel False = PixelRGB8 0xff 0xff 0xff

imgRun :: Word8 -> W Bool -> Int -> Int -> DynamicImage
imgRun r w n d = ImageRGB8 $ generateImage getPixel (d * 2+1) n
    where
        getPixel x y = showPixel $ generations !! y !! x
        generations  = run (wolframRule r) w n d

main :: IO()
main = sequence_ $ map save [0..255]
    where
        save r = savePngImage (imgPath r) (imgRun r wolframWorld n d)
        imgPath r = "img/rule" ++ show r ++ ".png"
        rs = [0..255]
        n  = 1024
        d  = 1024
