module Hascell.Wolfram where

    import Hascell.Simulate
    import Hascell.Simulate1D

    import Control.Comonad
    import Data.Array
    import Data.Bits
    import Data.Bits.Bitwise (fromListBE)
    import Data.Word

--- WORLD
    type Wolfram = U Int Bool

    single1Wolfram :: Int -> Wolfram
    single1Wolfram d = U d $ listArray (0, 2 * d) $ false ++ [True] ++ false
        where
            false = take d $ repeat False

    patternWolfram :: [Bool] -> Wolfram
    patternWolfram cells = U d $ listArray (0, 2 * d) cells
        where
            d = length cells

--- RULES
    wolframRule :: Word8 -> Wolfram -> Bool
    wolframRule x u = testBit x $ fromListBE (fmap extract [left u, u, right u])

--- SHOW
    stringShow :: Wolfram -> Word8 -> Int -> IO ()
    stringShow u r n = mapM_ putStrLn strings
        where
            strings = map (concat . map showCell . list) us
            showCell True  = "██"
            showCell False = "  "
            list (U i a) = elems a
            us = run (wolframRule r) u n