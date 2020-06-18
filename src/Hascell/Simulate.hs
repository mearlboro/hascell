{-# LANGUAGE DeriveFunctor #-}
module Hascell.Simulate where

    import Control.Comonad
    import Data.Array
    import Data.Bifunctor(first)
    import Data.Graph.Inductive.Graph
    import Data.Graph.Inductive.PatriciaTree
    import Data.Maybe(fromJust)
        
    data U i a = U i (Array i a) deriving (Functor, Show)

    instance Ix i => Comonad (U i) where
        extract   (U i a) = a ! i
        duplicate (U i a) = U i $ listArray (bounds a) (flip U a <$> (range $ bounds a))
        extend f u        = fmap f $ duplicate u

    arr :: Ix i => U i a -> Array i a
    arr (U _ a) = a

    run :: Comonad w => (w a -> a) -> w a -> Int -> [w a]
    run rule u n = take n $ iterate (extend rule) u

    data G a = G Node (Gr a ())

    instance Functor G where
        fmap f (G n gr) = G n (first f gr)
             
    instance Comonad G where
        extract (G n gr)   = fromJust $ lab gr n
        duplicate (G n gr) = G n (labMap (\(n,_) -> (n, G n gr)) gr)

    labMap :: Graph gr => (LNode a -> LNode b) -> gr a c -> gr b c
    labMap f gr = mkGraph (map f . labNodes $ gr) (labEdges gr)
