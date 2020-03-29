module Main where

import Lib

main :: IO ()
main = putStrLn "Hello World!"

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal."

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors  :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sumPairs :: [(Int, Int)] -> [Int]
sumPairs ls = [a + b | (a, b) <- ls]

firstLetter :: String -> String
firstLetter "" = ""
firstLetter word@(first:remaining) = "The first letter of " ++ word ++ " is " ++ [first]


ageGroup :: Double -> String
ageGroup age
    | age < 0 = error "Age must be between 0 and 200"
    | age > 200 = error "Age must be between 0 and 200" 
    | age <= 1 = "Baby"
    | age <= 3 = "Toddler"
    | age < 13 = "Child"
    | age < 18 = "Teen"
    | age < 60 = "Adult"
    | otherwise = "Elder"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a


head' :: [a] -> a
head' [] = error "List cannot be empty."
head' (first:_) = first
