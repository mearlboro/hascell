-- define zipper data type for generic 1D automaton as infinite list
-- the current cell is surrounded by two lists of all the other cells
data W a = W [a] a [a]

instance Functor W where
    fmap f (W as x bs) = W (fmap f as) (f x) (fmap f bs)

extract :: W a -> a
extract (W _ x _) = x

duplicate :: W a -> W (W a)
duplicate w = W (tail $ iterate left w) w (tail $ iterate right w)

-- navigation
left, right :: W a -> W a
left  (W (l:ls) x    rs ) = W    ls  l (x:rs)
right (W    ls  x (r:rs)) = W (x:ls) r rs

-- flatten to list
list :: W a -> [a]
list (W ls x rs) = reverse ls ++ [x] ++ rs

-- define how automaton changes state
evolve :: (W a -> a) -> W a -> W a
evolve rule w = fmap rule $ duplicate w

evolveN :: (W a -> a) -> W a -> Int -> [ W a ]
evolveN rule w n = take n $ iterate (evolve rule) w

-- since it's assumed infinite, we need to truncate it to display it
truncateD :: Int -> W a -> W a
truncateD d (W ls x rs) = W (take d ls) x (take d rs)

run :: (W a -> a) -> W a -> Int -> Int -> [[a]]
run rule w n d = map (list . (truncateD d)) $ evolveN rule w n

--------------------------------------------------------------------------------
-- for booleans, implement show function
showCell :: Bool -> String
showCell True  = "██"
showCell False = "  "

showGen :: [Bool] -> String
showGen cs = concat $ map showCell cs

--printRun :: (W a -> a) -> W a -> Int -> Int -> IO ()
printRun rule w n d = mapM_ putStrLn generations
    where
        generations = map showGen $ run rule w n d

--------------------------------------------------------------------------------
------------------------------ Wolfram's rule 30 -------------------------------
--
-- starts with a True cell in the middle and evolves by the following rule
-- [ left_cell XOR (central_cell OR right_cell ]

rule30start :: W Bool
rule30start = W (repeat False) True (repeat False)

rule30 :: W Bool -> Bool
rule30 w = lc /= (cc || rc)
    where lc = extract $ left  w
          rc = extract $ right w
          cc = extract w

--------------------------------------------------------------------------------
------------------------------ Wolfram's rule 30 -------------------------------
--
-- starts with a True cell in the middle and evolves by the following rule
-- [ left_cell XOR (central_cell OR right_cell ]
rule90 :: W Bool -> Bool
rule90 w = lc /=rc
    where lc = extract $ left  w
          rc = extract $ right w
