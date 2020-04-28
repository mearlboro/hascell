module Main where

import Hascell.Conway
import Hascell.Graphics
import System.IO  
import System.FilePath
      
parseInput :: String -> [(Int, Int)]
parseInput x = map pairs . map (map read) . map words . lines $ x
    where
        pairs (x:y:_) = (x,y)

stringRunGameOfLife :: IO ()
stringRunGameOfLife = do
    pattern <- getLine
    contents <- readFile ("meta/patterns/" ++ pattern) 
    u <- return . patternGameOfLife . parseInput $ contents
    stringShow u

imgRunGameOfLife :: IO ()
imgRunGameOfLife = do
    pattern <- getLine
    contents <- readFile ("meta/patterns/" ++ pattern) 
    u <- return . patternGameOfLife . parseInput $ contents
    conwayExport pattern u 100 5 10


main :: IO ()
main = putStrLn "Welcome to Hascell"