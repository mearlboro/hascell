{-# LANGUAGE DeriveFunctor #-}
module Hascell.Simulate2D where

    import Hascell.Simulate

    import Control.Comonad
    import Data.Array

    data Move = N | NE | E | SE | S | SW | W | NW deriving (Bounded, Enum, Eq, Show)

    height, width :: (Integral i, Ix i) => U (i, i) a -> i
    height (U _ a) = fst . snd . bounds $ a
    width  (U _ a) = snd . snd . bounds $ a

    neighbour :: (Integral i, Ix i) => U (i, i) a -> Move -> U (i, i) a
    neighbour u@(U (i, j) a) move = case move of
        N  -> U (i,               (j + 1) `mod` w) a
        NE -> U ((i + 1) `mod` h, (j + 1) `mod` w) a
        E  -> U ((i + 1) `mod` h,  j)              a
        SE -> U ((i + 1) `mod` h, (j - 1) `mod` w) a
        S  -> U (i,               (j - 1) `mod` w) a
        SW -> U ((i - 1) `mod` h, (j - 1) `mod` w) a
        W  -> U ((i - 1) `mod` h,  j)              a
        NW -> U ((i - 1) `mod` h, (j + 1) `mod` w) a
        where
            h = height u + 1
            w = width  u + 1


    numNeighbours :: (Eq a) => U (Int, Int) a -> a -> Int
    numNeighbours u a = length $ filter (==a) $ fmap extract $ map (neighbour u) [N ..]
