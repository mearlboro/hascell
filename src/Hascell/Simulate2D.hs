module Hascell.Simulate2D where

    import Hascell.Simulate

    import Control.Comonad
    import Data.Array

    data Move = N | NE | E | SE | S | SW | W | NW deriving (Bounded, Enum, Eq, Show)
    data Neighbourhood = Moore | VonNeumann | VonNeumannExt deriving (Bounded, Enum, Eq, Show)

    height, width :: (Integral i, Ix i) => U (i, i) a -> i
    height (U _ a) = fst . snd . bounds $ a
    width  (U _ a) = snd . snd . bounds $ a

    neighbour :: (Integral i, Ix i) => U (i, i) a -> i -> Move -> U (i, i) a
    neighbour u@(U (i, j) a) steps move = case move of
        N  -> U (i,                   (j + steps) `mod` w) a
        NE -> U ((i + steps) `mod` h, (j + steps) `mod` w) a
        E  -> U ((i + steps) `mod` h,  j)              a
        SE -> U ((i + steps) `mod` h, (j - steps) `mod` w) a
        S  -> U (i,                   (j - steps) `mod` w) a
        SW -> U ((i - steps) `mod` h, (j - steps) `mod` w) a
        W  -> U ((i - steps) `mod` h,  j)              a
        NW -> U ((i - steps) `mod` h, (j + steps) `mod` w) a
        where
            h = height u + 1
            w = width  u + 1

    neighboursMoore :: (Integral i, Ix i) => U (i, i) a -> [U (i, i) a]
    neighboursMoore u = map (neighbour u 1) [N ..]

    neighboursVonNeumann :: (Integral i, Ix i) => U (i, i) a -> [U (i, i) a]
    neighboursVonNeumann u = map (neighbour u 1) [N, E, S, W]

    neighboursVonNeumannExt :: (Integral i, Ix i) => U (i, i) a -> [U (i, i) a]
    neighboursVonNeumannExt u = map (neighbour u 2) [N, E, S, W]

    numNeighbours :: (Eq a) => Neighbourhood -> U (Int, Int) a -> a -> Int
    numNeighbours neigh u a = length $ filter (==a) $ fmap extract neighbourhood
        where
            neighbourhood = case neigh of
                Moore         -> neighboursMoore         u
                VonNeumann    -> neighboursVonNeumann    u
                VonNeumannExt -> neighboursVonNeumannExt u