module Hascell.Conway where

    import Hascell.Simulate
    import Hascell.Simulate2D

    import Control.Comonad
    import Data.Array
    import System.Process (rawSystem)

--- RULE
    type GameOfLife = U (Int, Int) Bool

    gameOfLife :: GameOfLife -> Bool
    gameOfLife u
        |     cell && (numNeighbours u True < 2)          = False
        |     cell && (numNeighbours u True `elem` [2,3]) = True
        |     cell && (numNeighbours u True > 3)          = False
        | not cell && (numNeighbours u True == 3)         = True
        | otherwise                                       = cell
        where
            cell = extract u

--- SHOW
    stringShow :: GameOfLife -> [String]
    stringShow u@(U (i, j) a) = map showRow $ [ U (k, j) a | k <- [0 .. height u] ]
        where
            showCell True  = "██"
            showCell False = "  "
            showRow (U (i, j) a) = concatMap showCell [ extract $ U (i, k) a | k <- [0 .. width u] ]

    conwayRun u = do
        getLine
        rawSystem "clear" []
        mapM_ putStrLn $ stringShow u
        conwayRun (extend gameOfLife u)

--- PATTERNS
    glider :: GameOfLife
    glider =  U (0, 0) xs
        where
            ys = listArray ((0, 0), (30,30)) $ repeat False
            xs = ys // [ ((1, 3), True)
                       , ((2, 2), True)
                       , ((0, 1), True)
                       , ((1, 1), True)
                       , ((2, 1), True) ]

