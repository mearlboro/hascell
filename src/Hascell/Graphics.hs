module Hascell.Graphics where

    import Hascell.Simulate
    import Hascell.Wolfram
    import Hascell.Simulate2D
    import Hascell.Conway

    import Codec.Picture
    import Data.Array
    import Data.Word

    type ColourMap a = (a -> PixelRGB8)

    pngFrom1D :: ColourMap a -> Int -> [U Int a] -> DynamicImage
    pngFrom1D colours zoom us = ImageRGB8 $ generateImage getPixel w h
        where
            getPixel x y = colours $ pixelArray ! (y `div` zoom) ! (x `div` zoom)
            pixelArray = listArray (0, h) $ map arr us
            w = zoom * (length $ arr $ head us)
            h = zoom * length us

    pngFrom2D :: ColourMap a -> Int -> U (Int, Int) a -> DynamicImage
    pngFrom2D colours zoom u = ImageRGB8 $ generateImage getPixel w h
        where
            getPixel x y = colours $ (arr u) ! (x `div` zoom, y `div` zoom)
            h = zoom * height u
            w = zoom * width  u

    exportGifFrom2D :: [U (Int, Int) a] -> ColourMap a -> Int -> Int -> String -> Either String (IO ())
    exportGifFrom2D us colours zoom delay path = writeGifAnimation path delay LoopingForever imgs
        where
            imgs = map (dynToImg . pngFrom2D colours zoom) us
            dynToImg (ImageRGB8 img) = img

--- Export Wolfram
    wolframPixels :: ColourMap Bool
    wolframPixels True  = PixelRGB8 0x00 0x00 0x00
    wolframPixels False = PixelRGB8 0xff 0xff 0xff

    wolframPath :: Word8 -> String
    wolframPath r = "img/wolfram/rule_" ++ show r ++ ".png"

    wolframExport :: Int -> Int -> Int -> Word8 -> IO ()
    wolframExport n d zoom r = savePngImage (wolframPath r) img
        where
            img = pngFrom1D wolframPixels zoom (run (wolframRule r) (wolframWorld d) n)

    wolframExportAll :: Int -> Int -> Int -> IO ()
    wolframExportAll n d zoom = sequence_ $ map (wolframExport n d zoom) [0..255]

--- Export Conway
    conwayPixels :: ColourMap Bool
    conwayPixels True  = PixelRGB8 0x00 0x00 0x00
    conwayPixels False = PixelRGB8 0xff 0xff 0xff

    conwayPath :: Show a => a -> String -> String
    conwayPath w ext = "img/conway/pattern_" ++ show w ++ "." ++ ext

    conwayExportStep :: GameOfLife -> Int -> Int -> IO ()
    conwayExportStep w n zoom = savePngImage (conwayPath n "png") img
        where
            img = pngFrom2D conwayPixels zoom (run gameOfLife w n !! (n - 1))

    conwayExport :: GameOfLife -> Int -> Int -> Int -> IO ()
    conwayExport w n zoom delay = either print id $ exportGifFrom2D (run gameOfLife w n) conwayPixels zoom delay (conwayPath n "gif")