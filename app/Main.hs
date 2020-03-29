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


ageGroupByYears :: Double -> String
ageGroupByYears years
    | years < 0 = error "Age must be between 0 and 200"
    | years > 200 = error "Age must be between 0 and 200" 
    | years <= 1 = "Baby"
    | years <= 3 = "Toddler"
    | years < 13 = "Child"
    | years < 18 = "Teen"
    | years < 60 = "Adult"
    | otherwise = "Elder"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a


head' :: [a] -> a
head' [] = error "List cannot be empty."
head' (first:_) = first

ageGroupByMonths :: Double -> String
ageGroupByMonths monthsOld = ageGroupByYears (monthsOld/12)

describeList :: [a] -> String
describeList ls = "The list is " ++ kind ls
  where kind [] = "empty."
        kind [x] = "a singleton list."
        kind xs = "a longer list."
