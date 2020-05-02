module Hascell.Graphics where

    import Hascell.Simulate
    import Hascell.Wolfram
    import Hascell.Simulate2D
    import qualified Hascell.SimulateRand2D as R
    import Hascell.Conway
    import Hascell.Excitable
    import Hascell.ForestFire

    import Codec.Picture
    import Data.Array
    import Data.Word
    import Control.Comonad.Trans.Env

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
            getPixel x y = colours $ (arr u) ! (y `div` zoom, x `div` zoom)
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

    wolframExport :: Wolfram -> Int -> Int -> Word8 -> IO ()
    wolframExport u n zoom r = savePngImage (wolframPath r) img
        where
            img = pngFrom1D wolframPixels zoom (run (wolframRule r) u n)

    wolframExportAll :: Wolfram -> Int -> Int -> IO ()
    wolframExportAll u n zoom = sequence_ $ map (wolframExport u n zoom) [0..255]

--- Export Conway
    conwayPixels :: ColourMap Bool
    conwayPixels True  = PixelRGB8 0x00 0x00 0x00
    conwayPixels False = PixelRGB8 0xff 0xff 0xff

    conwayPath :: String -> String -> String
    conwayPath name ext = "img/conway/pattern_" ++ name ++ "." ++ ext

    conwayExportStep :: String -> GameOfLife -> Int -> Int -> IO ()
    conwayExportStep name u n zoom = savePngImage (conwayPath name "png") img
        where
            img = pngFrom2D conwayPixels zoom (run gameOfLifeRule u n !! (n - 1))

    conwayExport :: String -> GameOfLife -> Int -> Int -> Int -> IO ()
    conwayExport name u n zoom delay = either print id $ exportGifFrom2D (run gameOfLifeRule u n) conwayPixels zoom delay (conwayPath name "gif")

--- Export Greenberg-Hastings
    excitablePixels :: ColourMap ExcitableCell
    excitablePixels Spike = PixelRGB8 0x00 0xff 0x33
    excitablePixels Rest  = PixelRGB8 0xff 0xff 0x00
    excitablePixels Quiet = PixelRGB8 0xff 0xff 0xff

    excitablePath :: String -> String -> String
    excitablePath name ext = "img/excitable/pattern_" ++ name ++ "." ++ ext

    excitableExportStep :: String -> ExcitableMedium -> Int -> Int -> IO ()
    excitableExportStep name u n zoom = savePngImage (excitablePath name "png") img
        where
            img = pngFrom2D excitablePixels zoom (run greenbergHastingsRule u n !! (n - 1))

    excitableExport :: String -> ExcitableMedium -> Int -> Int -> Int -> IO ()
    excitableExport name u n zoom delay = either print id $ exportGifFrom2D (run greenbergHastingsRule u n) excitablePixels zoom delay (excitablePath name "gif")

--- Export Forest
    pngFrom2DRand :: ColourMap Tree -> Int -> Forest -> DynamicImage
    pngFrom2DRand colours zoom (EnvT _ u) = ImageRGB8 $ generateImage getPixel w h
        where
            getPixel x y = colours $ (arr u) ! (y `div` zoom, x `div` zoom)
            h = zoom * R.height u
            w = zoom * R.width  u
            arr (RandU _ _ a) = a

    exportGifFrom2DRand :: [Forest] -> ColourMap Tree -> Int -> Int -> String -> Either String (IO ())
    exportGifFrom2DRand us colours zoom delay path = writeGifAnimation path delay LoopingForever imgs
        where
            imgs = map (dynToImg . pngFrom2DRand colours zoom) us
            dynToImg (ImageRGB8 img) = img

    forestPixels :: ColourMap Tree
    forestPixels Fire = PixelRGB8 0xcc 0x33 0x00
    forestPixels Live = PixelRGB8 0x00 0xcc 0x33
    forestPixels Dead = PixelRGB8 0x00 0x00 0x00

    forestPath :: String -> String
    forestPath ext = "img/excitable/pattern_forest." ++ ext

    forestExport :: Forest -> Int -> Int -> Int -> IO ()
    forestExport u n zoom delay = either print id $ exportGifFrom2DRand (runRand forestFireRule u n) forestPixels zoom delay (forestPath "gif")
