module Hascell.Conway where

    import Hascell.Simulate
    import Hascell.Simulate2D

    import Control.Comonad
    import Data.Array
    import System.Process (rawSystem)

--- WORLD
    type GameOfLife = U (Int, Int) Bool

    patternGameOfLife :: [(Int, Int)] -> GameOfLife
    patternGameOfLife coords = U (0, 0) cells
        where
            empty = listArray ((0, 0), (w + 1, h + 1)) $ repeat False
            alive = zipWith (,) coords $ repeat True
            cells = empty // alive
            w = maximum $ map fst coords
            h = maximum $ map snd coords

--- RULES
    gameOfLifeRule :: GameOfLife -> Bool
    gameOfLifeRule u
        |     cell && (numNeighbours Moore u True < 2)          = False
        |     cell && (numNeighbours Moore u True `elem` [2,3]) = True
        |     cell && (numNeighbours Moore u True > 3)          = False
        | not cell && (numNeighbours Moore u True == 3)         = True
        | otherwise                                       = cell
        where
            cell = extract u

--- SHOW
    stringShowStep :: GameOfLife -> [String]
    stringShowStep u@(U (i, j) a) = map showRow $ [ U (k, j) a | k <- [0 .. height u] ]
        where
            showCell True  = "██"
            showCell False = "  "
            showRow (U (i, j) a) = concatMap showCell [ extract $ U (i, k) a | k <- [0 .. width u] ]

    stringShow u = do
        getLine
        rawSystem "clear" []
        mapM_ putStrLn $ stringShowStep u
        stringShow (extend gameOfLifeRule u)
