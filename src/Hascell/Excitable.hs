module Hascell.Excitable where

    import Hascell.Simulate
    import Hascell.Simulate2D

    import Control.Comonad
    import Data.Array
    import System.Process (rawSystem)

--- WORLD
    data ExcitableCell   = Quiet | Spike | Rest deriving (Bounded, Enum, Eq, Show)
    type ExcitableMedium = U (Int, Int) ExcitableCell

--- RULES
    greenbergHastingsRule :: ExcitableMedium -> ExcitableCell
    greenbergHastingsRule u
        | cell == Quiet && (numNeighbours VonNeumann u Spike >= 1) = Spike
        | cell == Spike                                            = Rest
        | cell == Rest                                             = Quiet
        | otherwise                                                = cell
        where
            cell = extract u

--- SHOW
    stringShowStep :: ExcitableMedium -> [String]
    stringShowStep u@(U (i, j) a) = map showRow $ [ U (k, j) a | k <- [0 .. height u] ]
        where
            showCell Spike = "██"
            showCell Quiet = "  "
            showCell Rest  = "##"
            showRow (U (i, j) a) = concatMap showCell [ extract $ U (i, k) a | k <- [0 .. width u] ]

    stringShow u = do
        getLine
        rawSystem "clear" []
        mapM_ putStrLn $ stringShowStep u
        stringShow (extend greenbergHastingsRule u)

--- MODELS
    spiral :: ExcitableMedium
    spiral = U (0, 0) cells
        where
            empty = listArray ((0, 0), (100, 100)) $ repeat Rest
            cells = empty // [ ((50, 50), Spike)
                             , ((50, 51), Quiet)
                             ]

    spiral2 :: ExcitableMedium
    spiral2 = U (0, 0) cells
        where
            empty = listArray ((0, 0), (100, 100)) $ repeat Rest
            line1 = zipWith (,) (zipWith (,)  (take 50 (repeat 50)) [50 .. 99]) (take 50 (repeat Spike))
            line2 = zipWith (,) (zipWith (,)  (take 50 (repeat 51)) [50 .. 99]) (take 50 (repeat Quiet))
            cells = empty // line1 // line2