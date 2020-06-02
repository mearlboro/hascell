module Hascell.Random where

    import Control.Comonad
    import Control.Comonad.Trans.Class
    import System.Random
    
    data RandT g w a = RandT {seed :: g, unRandT :: w a}

    instance (Functor w) => Functor (RandT g w) where
                      fmap f (RandT g w) = RandT g (fmap f w)

               
    instance (RandomGen g, Comonad w) => Comonad (RandT g w) where
                      extract (RandT g w) = extract w
                      duplicate (RandT g w) = let (g', g'') = split g
                                              in RandT g' (extend (RandT g'') w) 

    instance (RandomGen g) => ComonadTrans (RandT g) where
                      lower = unRandT
