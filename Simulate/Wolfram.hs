{-# LANGUAGE DeriveFunctor #-}
module Hascell.Simulate1D.Wolfram where

    import Hascell.Simulate
    import Hascell.Simulate1D

    import Codec.Picture
    import Control.Comonad
    import Data.Array
    import Data.Bits
    import Data.Bits.Bitwise (fromListBE)
    import Data.Word


    wolframWorld :: Int -> U Int Bool
    wolframWorld d = U d $ listArray (0, 2 * d) $ (take d $ repeat False) ++ [True] ++ (take d $ repeat False)

    wolframRule :: Word8 -> U Int Bool -> Bool
    wolframRule x u = testBit x $ fromListBE (fmap extract [left u, u, right u])

    wolframRun :: Word8 -> Int -> Int -> IO ()
    wolframRun r n d = mapM_ putStrLn generations
        where
            generations = stringShow $ run (wolframRule r) (wolframWorld d) n


    stringShow :: [U Int Bool] -> [String]
    stringShow us = map (concat . map showCell . list) us
        where
            showCell True  = "██"
            showCell False = "  "
            list (U i a) = elems a


    imageShow :: [U Int Bool] -> Int -> Int -> DynamicImage
    imageShow us h u = ImageRGB8 $ generateImage getPixel h u
        where
            getPixel x y = showPixel $ pixelArray ! x ! y
            pixelArray = listArray (0, h-1) $ map arr us
            showPixel True  = PixelRGB8 0x00 0x00 0x00
            showPixel False = PixelRGB8 0xff 0xff 0xff
            arr (U _ a) = a

    exportAllToPNG :: Int -> Int -> IO()
    exportAllToPNG n d = sequence_ $ map save [0..255]
        where
            save r = savePngImage (imgPath r) (img r)
            imgPath r = "img/wolfram/rule" ++ show r ++ ".png"
            img r = imageShow (run (wolframRule r) (wolframWorld d) n) n (2 * d + 1)
