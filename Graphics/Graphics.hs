module Hascell.Graphics where

    import Hascell.Simulate
    import Hascell.Simulate1D.Wolfram
    import Hascell.Simulate2D
    import Hascell.Simulate2D.Conway

    import Codec.Picture
    import Data.Array
    import Data.Word

    type ColourMap a = (a -> PixelRGB8)

    pngFrom1D :: ColourMap a -> [U Int a] -> DynamicImage
    pngFrom1D colours us = ImageRGB8 $ generateImage getPixel w h
        where
            getPixel x y = colours $ pixelArray ! y ! x
            pixelArray = listArray (0, h) $ map arr us
            w = length $ arr $ head us
            h = length us

    pngFrom2D :: ColourMap a -> U (Int, Int) a -> DynamicImage
    pngFrom2D colours u = ImageRGB8 $ generateImage getPixel w h
        where
            getPixel x y = colours $ (arr u) ! (x, y)
            h = height u
            w = width  u

    exportGifFrom2D :: [U (Int, Int) a] -> ColourMap a -> Int -> String -> Either String (IO ())
    exportGifFrom2D us colours delay path = writeGifAnimation path delay LoopingForever imgs
        where
            imgs = map (dynToImg . pngFrom2D colours) us
            dynToImg (ImageRGB8 img) = img

--- Export Wolfram
    wolframPixels :: ColourMap Bool
    wolframPixels True  = PixelRGB8 0x00 0x00 0x00
    wolframPixels False = PixelRGB8 0xff 0xff 0xff

    wolframPath :: Word8 -> String
    wolframPath r = "img/wolfram/rule_" ++ show r ++ ".png"

    wolframExport :: Int -> Int -> Word8 -> IO ()
    wolframExport n d r = savePngImage (wolframPath r) img
        where
            img = pngFrom1D wolframPixels (run (wolframRule r) (wolframWorld d) n)

    wolframExportAll :: Int -> Int -> IO ()
    wolframExportAll n d = sequence_ $ map (wolframExport n d) [0..255]

--- Export Conway
    conwayPixels :: ColourMap Bool
    conwayPixels True  = PixelRGB8 0x00 0x00 0x00
    conwayPixels False = PixelRGB8 0xff 0xff 0xff

    conwayPath :: Show a => a -> String -> String
    conwayPath w ext = "img/conway/pattern_" ++ show w ++ "." ++ ext

    conwayExportStep :: GameOfLife -> Int -> IO ()
    conwayExportStep w n = savePngImage (conwayPath n "png") img
        where
            img = pngFrom2D conwayPixels (run gameOfLife w n !! (n - 1))

    conwayExport :: GameOfLife -> Int -> IO ()
    conwayExport w n = either print id $ exportGifFrom2D (run gameOfLife w n) conwayPixels 10 (conwayPath n "gif")
