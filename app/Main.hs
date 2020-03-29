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

recurMax :: (Ord a) => [a] -> a
recurMax [] = error "Max of an empty list."
recurMax [x] = x
recurMax (x:xs) = max' x (recurMax xs)


replicate' :: Int -> a -> [a]
replicate' n item
    | n <= 0    = []
    | otherwise = item : replicate' (n - 1) item

range' :: Int -> Int -> [Int]
range' start end
    | start > end = []
    | otherwise = start : range' (start + 1) end


take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' item = item : repeat' item

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' item (x:xs) 
    | item == x = True
    | otherwise = item `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
        in quicksort smallerOrEqual ++ [x] ++ quicksort larger
