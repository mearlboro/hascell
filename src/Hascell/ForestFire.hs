module Hascell.ForestFire where

    import Hascell.Simulate
    import Hascell.Simulate2D

    import Control.Comonad
    import Control.Comonad.Trans.Env
    import Data.Array
    import System.Process (rawSystem)
    import System.Random

--- WORLD
    data Tree   = Live | Fire | Dead deriving (Eq, Show)
    type Params = (StdGen, Float, Float)
    type Forest = EnvT Params (U (Int, Int)) Tree

    get :: Ix i => EnvT Params (U i) a -> U i a
    get (EnvT _ u) = u

    extendRand :: (Forest -> Tree) -> Forest -> Forest
    extendRand rule u@(EnvT (g, f, p) (U (i, j) a)) = EnvT (g'', f, p) r
        where
            (g', g'') = split g
            u' = EnvT (g', f, p) (U (i, j) a)
            EnvT _ r = extend rule u'

    runRand rule u n = take n $ iterate (extendRand rule) u

--- RULES
    forestFireRule :: Forest -> Tree
    forestFireRule (EnvT (g, f, p) u@(U (i,j) a)) = case tree of
        Fire -> Dead
        Dead -> if (rand < p) then Live else Dead
        Live -> if (numNeighbours Moore u Fire > 0)
                then Fire
                else if (rand < f) then Fire else Live
        where
            tree = extract u
            rs = take n $ randomRs (0.0, 1.0) g
            n = (height u + 1) * (width u + 1)
            rand = rs !! ( i * (width u + 1) + j )
            test val = rand < val

--- SHOW
    stringShowStep :: Forest -> [String]
    stringShowStep (EnvT _ u@(U (i, j) a))
      = map showRow $ [ U (k, j) a | k <- [0 .. height u] ]
        where
            showCell Live = "{}"
            showCell Fire = "██"
            showCell Dead = "  "
            showRow (U (i, j) a) = concatMap showCell [ extract $ U (i, k) a | k <- [0 .. width u] ]

    stringShowF u@(EnvT (g, f, p) (U (i, j) a)) = do
        getLine
        rawSystem "clear" []
        mapM_ putStrLn $ stringShowStep u
        let (g', g'') = split g
            u' = EnvT (g', f, p) (U (i, j) a)
            EnvT _ r = extend forestFireRule u'
        stringShowF (EnvT (g'', f, p) r)


    newForest f p = EnvT (g, f, p) (U (0, 0) cells)
        where
            g = mkStdGen $ round $ 16860353668.0 ** f / p
            empty = listArray ((0, 0), (20, 20)) $ repeat Live
            line2 = zipWith (,) (zipWith (,) [8,5,6,7,3,1,19,14,1,2] [16,14,14,4,8,1,4,12,8,10]) (take 10 (repeat Dead))
            line1 = zipWith (,) (zipWith (,) [1,3,5,7,12,3,8,11,18,2] [17,16,13,3,8,1,5,8,7,11]) (take 10 (repeat Fire))
            cells = empty // line1 // line2


