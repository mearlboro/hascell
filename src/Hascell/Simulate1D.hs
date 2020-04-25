module Hascell.Simulate1D where

    import Hascell.Simulate

    import Data.Array

    left, right :: (Ix i, Num i) => U i a -> U i a
    left  (U i a)
        | i == 0    = U (snd . bounds $ a) a
        | otherwise = U (i - 1)            a
    right (U i a)
        | i == (snd . bounds $ a) = U 0       a
        | otherwise               = U (i + 1) a
