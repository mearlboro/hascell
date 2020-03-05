module Hascell.Simulate1D.Wolfram where

    import Hascell.Simulate
    import Hascell.Simulate1D

    import Control.Comonad
    import Data.Array
    import Data.Bits
    import Data.Bits.Bitwise (fromListBE)
    import Data.Word

--- RULES
    type Wolfram = U Int Bool

    wolframWorld :: Int -> Wolfram
    wolframWorld d = U d $ listArray (0, 2 * d) $ (take d $ repeat False) ++ [True] ++ (take d $ repeat False)

    wolframRule :: Word8 -> Wolfram -> Bool
    wolframRule x u = testBit x $ fromListBE (fmap extract [left u, u, right u])

--- SHOW
    stringShow :: Word8 -> Int -> Int -> IO ()
    stringShow r n d = mapM_ putStrLn strings
        where
            strings = map (concat . map showCell . list) us
            showCell True  = "██"
            showCell False = "  "
            list (U i a) = elems a
            us = run (wolframRule r) (wolframWorld d) n
