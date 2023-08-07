module Golf () where
import Data.List (group, transpose)

skips :: [a] -> [[a]]
skips [] = []
skips xs = [[x | (i, x) <- zip [1..] xs, i `mod` n == 0] | n <- [1..length xs]]

localMaxima :: [Integer] -> [Integer]
localMaxima [_, _] = []
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)

histogram :: [Integer] -> String
-- histogram = foldr (\x acc -> replicate (fromIntegral x) ' ' ++ "*\n" ++ acc) "==========\n0123456789\n"
histogram xs = unlines [replicate (fromIntegral x) ' ' ++ "*" | x <- xs] ++ legend 
    where
        rotate = reverse . transpose
        legend = "==========\n0123456789\n"

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

bars :: [[String]] -> [String]
bars xs = [x | x <- xs]

drawOne :: Integer -> String
drawOne x = replicate (fromIntegral x) ' ' ++ "\n"

draw :: [Integer] -> String
draw xs@(x:t)
    | length xs == 1 = drawOne x
    | otherwise =  