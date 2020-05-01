{-# LANGUAGE DeriveFunctor #-}
module Hascell.Simulate where

    import Control.Comonad
    import Data.Array
    import System.Random

    data U i a = U i (Array i a) deriving (Functor, Show)

    instance Ix i => Comonad (U i) where
        extract   (U i a) = a ! i
        duplicate (U i a) = U i $ listArray (bounds a) (flip U a <$> (range $ bounds a))
        extend f u        = fmap f $ duplicate u

    arr :: Ix i => U i a -> Array i a
    arr (U _ a) = a

    run :: Ix i => (U i a -> a) -> U i a -> Int -> [U i a]
    run rule u n = take n $ iterate (extend rule) u

    data RandU r i a = RandU r i (Array i a) deriving (Functor, Show)

    instance (RandomGen r, Ix i) => Comonad (RandU r i) where
        extract    (RandU _ i a) = a ! i
        duplicate  (RandU r i a) = RandU r i $ listArray (bounds a) (flip (RandU r) a <$> (range $ bounds a))
        extend f u@(RandU r i a) = RandU r' i a'
            where
            (_, r') = next r
            (RandU _ _ a') = fmap f $ duplicate u
