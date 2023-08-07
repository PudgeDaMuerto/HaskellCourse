module Golf () where
import Data.List (transpose)

-----------------------------Ex.1 Hopscotch-----------------------------

skips :: [a] -> [[a]]
skips [] = []
skips xs = [[x | (i, x) <- zip [1..] xs, i `mod` n == 0] | n <- [1..length xs]]

---------------------------Ex.2 Local maxima----------------------------

localMaxima :: [Integer] -> [Integer]
localMaxima [_, _] = []
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)

-----------------------------Ex.3 Histogram-----------------------------

histogram :: [Integer] -> String
histogram xs = unlines $ rotate bars ++ legend
    where
        count x = length . filter (==x)
        template = [count i xs | i <- [0..9]]
        rotate = reverse . transpose
        max = maximum template
        bars = map (\x -> take max $ replicate x '*' ++ repeat ' ') template
        legend = ["==========","0123456789"]