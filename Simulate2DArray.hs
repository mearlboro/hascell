{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Codec.Picture
import Control.Comonad
import Data.Array
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.Word
import System.Process (rawSystem)

--- Data Type
data U i a = U i (Array i a) deriving (Functor, Show)

instance Ix i => Comonad (U i) where
    extract (U i a)   = a ! i
    duplicate (U i a) = U i $ listArray (bounds a) (flip U a <$> (range $ bounds a))
    extend f u        = fmap f $ duplicate u


--- 2D world
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


--- Game of Life
numNeighbours :: U (Int, Int) Bool -> Int
numNeighbours u = length $ filter id $ fmap extract $ map (neighbour u) [N ..]

gameOfLife :: U (Int, Int) Bool -> Bool
gameOfLife w
    |      extract w  && (numNeighbours w < 2)          = False
    |      extract w  && (numNeighbours w `elem` [2,3]) = True
    |      extract w  && (numNeighbours w > 3)          = False
    | not (extract w) && (numNeighbours w == 3)         = True
    | otherwise                                         = extract w

world :: U (Int, Int) Bool
world =  U (0, 0) xs
  where
    ys = listArray ((0, 0), (4,4)) $ repeat False
    xs = ys // [ ((1, 3), True)
               , ((2, 2), True)
               , ((0, 1), True)
               , ((1, 1), True)
               , ((2, 1), True) ]

--- Show as string
stringShow :: U (Int, Int) Bool -> [String]
stringShow u@(U (i, j) a) = map showRow $ [ U (k, j) a | k <- [0 .. height u] ]
    where
        showCell True  = "██"
        showCell False = "  "
        showRow (U (i, j) a) = concatMap showCell [ extract $ U (i, k) a | k <- [0 .. width u] ]

runConway u = do
    getLine
    rawSystem "clear" []
    mapM_ putStrLn $ stringShow u
    runConway (extend gameOfLife u)
