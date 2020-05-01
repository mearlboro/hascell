module Hascell.SimulateRand2D where

    import Hascell.Simulate

    import Control.Comonad
    import Data.Array
    import System.Random

    data Move = N | NE | E | SE | S | SW | W | NW deriving (Bounded, Enum, Eq, Show)
    data Neighbourhood = Moore | VonNeumann | VonNeumannExt deriving (Bounded, Enum, Eq, Show)

    height, width :: (Integral i, Ix i) => RandU r (i, i) a -> i
    height (RandU _ _ a) = fst . snd . bounds $ a
    width  (RandU _ _ a) = snd . snd . bounds $ a

    neighbour :: (Integral i, Ix i, RandomGen r) => RandU r (i, i) a -> i -> Move -> RandU r (i, i) a
    neighbour u@(RandU r (i, j) a) steps move = case move of
        N  -> RandU r (i,                   (j + steps) `mod` w) a
        NE -> RandU r ((i + steps) `mod` h, (j + steps) `mod` w) a
        E  -> RandU r ((i + steps) `mod` h,  j)              a
        SE -> RandU r ((i + steps) `mod` h, (j - steps) `mod` w) a
        S  -> RandU r (i,                   (j - steps) `mod` w) a
        SW -> RandU r ((i - steps) `mod` h, (j - steps) `mod` w) a
        W  -> RandU r ((i - steps) `mod` h,  j)              a
        NW -> RandU r ((i - steps) `mod` h, (j + steps) `mod` w) a
        where
            h  = height u + 1
            w  = width  u + 1

    neighboursMoore :: (Integral i, Ix i, RandomGen r) => RandU r (i, i) a -> [RandU r (i, i) a]
    neighboursMoore u = map (neighbour u 1) [N ..]

    neighboursVonNeumann :: (Integral i, Ix i, RandomGen r) => RandU r (i, i) a -> [RandU r (i, i) a]
    neighboursVonNeumann u = map (neighbour u 1) [N, E, S, W]

    neighboursVonNeumannExt :: (Integral i, Ix i, RandomGen r) => RandU r (i, i) a -> [RandU r (i, i) a]
    neighboursVonNeumannExt u = map (neighbour u 2) [N, E, S, W]

    numNeighbours :: (Eq a, RandomGen r) => Neighbourhood -> RandU r (Int, Int) a -> a -> Int
    numNeighbours neigh u a = length $ filter (==a) $ fmap extract neighbourhood
        where
            neighbourhood = case neigh of
                Moore         -> neighboursMoore         u
                VonNeumann    -> neighboursVonNeumann    u
                VonNeumannExt -> neighboursVonNeumannExt u

