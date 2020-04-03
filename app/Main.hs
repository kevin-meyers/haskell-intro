module Main where
import Lib
import Data.List
import Data.Char
import qualified Data.Text as T
import Data.Text.Internal (Text)
import Data.Maybe (fromJust)

import Text.Numeral.Language.ENG as EN
import Text.Numeral.Grammar

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


isUpper :: Char -> Bool
isUpper = (`elem` ['A'..'Z'])


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


makeAdder :: Int -> Int -> Int
makeAdder a b = a + b


flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

isPrime :: Int -> Bool
isPrime k 
    | k > 1 = null [ x | x <- [2..(round . sqrt . fromIntegral $ k)], k `mod` x == 0] 
    | otherwise = False


isFactorOf :: Integral a => a -> a -> Bool
isFactorOf x n = n `mod` x == 0

factorList :: Int -> [Int]
factorList n = filter (flip isFactorOf n) [1 .. n `div` 2]

primes :: [Int]
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors :: Int -> [Int]
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps


problem_12 = find ((>500) . nDivisors) (scanl1 (+) [1..])
    where nDivisors n = product $ map ((+1) . length) (group (primeFactors n))

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show


collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)


toEnglish :: Int -> Text
toEnglish = fromJust . EN.us_cardinal defaultInflection

countLetters :: Text -> Int
countLetters = T.length . T.filter (>='a')

lengthFromNum :: Int -> Int
lengthFromNum num
	| num < 100 = countLetters . toEnglish $ num
	| num `mod` 100 == 0 = countLetters . toEnglish $ num
	| otherwise = (+3) . countLetters . toEnglish $ num

triangle = [
	[75],
	[95,64],
	[17,47,82],
	[18,35,87,10],
	[20,04,82,47,65],
	[19,01,23,75,03,34],
	[88,02,77,73,07,63,67],
	[99,65,04,28,06,16,70,92],
	[41,41,26,56,83,40,80,70,33],
	[41,48,72,33,47,32,37,16,94,29],
	[53,71,44,65,25,43,91,52,97,51,14],
	[70,11,33,28,77,73,17,78,39,68,17,57],
	[91,71,52,38,17,14,91,43,58,50,27,29,48],
	[63,66,04,68,89,53,67,30,73,16,69,87,40,31],
	[04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

sumFactors :: Int -> Int
sumFactors = sum . factorList

areAmicable :: Int -> Int -> Bool
areAmicable a b 
	| b > 10000 = False
	| a == b = False
	| otherwise =  a == sumFactors b

isAmicable :: Int -> Bool
isAmicable n = areAmicable n $ sumFactors n

isPerfect :: Int -> Bool
isPerfect n = n == sumFactors n

isSorted :: (Ord a) => [a] -> Bool
isSorted [a] = True
isSorted (last:remaining)
    | head remaining < last = False
    | otherwise = isSorted remaining


bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [x] = [x]
bubbleSort (x:y:xs)
    | x < y = x : bubbleSort (y:xs)
    | otherwise = y : bubbleSort (x:xs)

getMin :: (Ord a) => [a] -> a
getMin [m] = m
getMin (m:x:xs)
    | x > m = getMin $ m:xs
    | otherwise = getMin $ x:xs

removeItem :: (Ord a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem item (x:xs)
    | x == item = xs
    | otherwise = x : removeItem item xs 

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort l = minItem : selectionSort (removeItem minItem l)
    where minItem = getMin l

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey _ [] = Nothing
findKey key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKey key xs
