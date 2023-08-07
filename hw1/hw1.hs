module Homework1 where

----------------Validating Credit Card Number----------------

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev num = reverse (toDigits num)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [
        if odd y 
            then (reverse xs !! y) * 2 
            else reverse xs !! y 
        | y <- [0..(length xs - 1)]
    ]

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [ if x > 9 then sum (toDigits x) else x  | x <- xs]

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

---------------------The Towers of Hanoi---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dst tmp = fromSrcToTmp ++ fromSrcToDst ++ fromTmpToDst 
    where 
        fromSrcToTmp = hanoi (n-1) src tmp dst
        fromSrcToDst = [(src, dst)]
        fromTmpToDst = hanoi (n-1) tmp dst src 

hanoi4Pegs :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4Pegs 0 _ _ _ _ = [] 
hanoi4Pegs n src dst tmp1 tmp2 = fromSrcToTmpK ++ fromSrcToDst ++ fromTmpToDst
    where
        n' = fromIntegral n :: Double
        k = n - round (sqrt (2*n' + 1)) + 1
        fromSrcToTmpK = hanoi4Pegs k src tmp1 dst tmp2
        fromSrcToDst = hanoi (n - k) src dst tmp2
        fromTmpToDst = hanoi4Pegs k tmp1 dst src tmp2
