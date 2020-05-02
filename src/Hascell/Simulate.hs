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

