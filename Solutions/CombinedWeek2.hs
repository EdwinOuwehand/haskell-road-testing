
module Lab2 where

import Data.List
import Data.Char
import Data.Fixed
import System.Random
import Test.QuickCheck


infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- Exercise 1 
-- Time taken: 2h

-- Most elegant solution.
-- Jouke
-- Not using hardcoded quartiles
-- Average Deviation calculation, very nice feature!
-- Comments

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
        p <- getStdRandom random
        ps <- probs (n-1)
        return (p:ps)

-- https://stackoverflow.com/a/26372259
sofrequency :: Ord a => [a] -> [Int]
sofrequency = map length.group.sort

-- dist :: Int -> IO [Int]
dist n = do
        ps <- probs n
        return $ sofrequency $ map (\ n -> n - (mod' n 0.25)) ps

avgDeviation' :: Int -> IO [Int]
avgDeviation' 0 = return []
avgDeviation' i = do
        dst <- dist 100000
        dstx <- avgDeviation' (i-1)
        return (((flip div 4).sum $ map (abs.(subtract 25000)) dst):dstx)

-- Avg deviation over 100 runs
avgDeviation :: IO Int
avgDeviation = do
        r <- avgDeviation' 100
        return $ (sum r) `div` (length r)

-- While not giving a perfect 1/4 per quarter distribution,
-- the avgDeviation function observes deviations around 100-115
-- on a 25000 numbers per quarter basis. This equals percentages
-- of 0.40-0.46%
probsExample = dist 100000

-- A test would be to check if the diviation stays within 0.05%
-- This would look like the following:
probsTest = do
        r <- avgDeviation
        return $ r <= 125


-- Exercise 2
-- Time Taken: 1h
-- Solution from Edwin
-- Seperate functions for isTriangle, isRightangled etc.
-- Tests
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rightangled | Other deriving (Eq,Show)

-- Not a triangle   when the length of one side is longer than the others combined. (Any other cases?)
-- Equilateral      when all sides are equal.
-- Rightangled      when one corner is exactly 90 degrees.
-- Isosceles        when not equilateral and has two sides of equal length. 
-- other            when none of the above but still a triangle. 

isTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isTriangle x y z = x < (y + z)

isEquilateral :: Eq a => a -> a -> a -> Bool
isEquilateral x y z = x == y && y == z

-- We know that pythagoras holds only for rightangled triangles,
-- and since we know all three sides we can verify it this way. 
isRightangled :: (Eq a, Num a) => a -> a -> a -> Bool
isRightangled x y z = (y^2 + z^2) == x^2

isIsosceles :: Eq a => a -> a -> a -> Bool
isIsosceles x y z = not (isEquilateral x y z) && (x == y || y == z || x == z)

isOther :: (Num a, Ord a) => a -> a -> a -> Bool
isOther x y z = isTriangle x y z && not (isEquilateral x y z) &&  not (isRightangled x y z) && not (isIsosceles x y z)

shape :: (Num a, Ord a) => [a] -> Shape
shape [x, y, z] | not (isTriangle x y z) = NoTriangle
                | isEquilateral x y z    = Equilateral
                | isRightangled x y z    = Rightangled
                | isIsosceles x y z      = Isosceles
                | isOther x y z          = Other

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = shape (reverse (sort[x, y, z]))

testData :: [[Integer]]
testData = [ [x,y,z] | x <- [0..5], y <- [x..5], z <- [x..5]]

tester :: [[Integer]] -> Shape -> [[Integer]]
tester xs s = [ [x, y, z] | [x, y, z] <- xs, triangle x y z == s]

-- -- NoTriangles
-- tester testData NoTriangle
--
-- -- Equilateral
-- tester testData Equilateral
--
-- -- Rightangled
-- tester testData Rightangled
--
-- -- Isosceles
-- tester testData Isosceles
--
-- -- Other
-- tester testData Other


-- Exercise 3
-- Time taken : 1h
-- Jouke's solution
-- Extensive quicksort used for stronger/weaker lists
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

p1, p2, p3, p4 :: Int -> Bool
p1 = (\ x -> even x && x > 3)
p2 = (\ x -> even x || x > 3)
p3 = (\ x -> (even x && x > 3) || even x)
p4 = even

properties = [("p1", p1), ("p2", p2), ("p3", p3), ("p4", p4)]

getName (n, _) = n
getFunc (_, f) = f

-- From slides
quicksort' :: [a] -> [(t, a -> Bool)] -> [(t, a -> Bool)]
quicksort' _ [] = []  
quicksort' r (x:xs) = 
   quicksort' r [ a | a <- xs, stronger r (getFunc x) (getFunc a)]
   ++ [x]
   ++ quicksort' r [ a | a <- xs, not $ stronger r (getFunc x) (getFunc a)]

-- When using stronger and weaker, p4 will occur twice as it is as strong as p3,
-- given that the "&& x > 3" is useless with the "|| even x" in place

lstxx :: [[Char]]
lstxx = map getName $ quicksort' [-10..10] properties


-- Exercise 4
-- Solution: Steff
-- Time Taken: 1h
-- This solution has tests and is elegant
-- Different properties tested
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] xs  = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)

pEqual :: Eq a => [a] -> Bool
pEqual xs = isPermutation xs xs
-- quickCheck pEqual
-- +++ OK, passed 100 tests.

pReversed :: Eq a => [a] -> Bool
pReversed xs = (isPermutation xs (reverse xs))
-- quickCheck pReversed
-- +++ OK, passed 100 tests.

pSorted :: Ord a => [a] -> Bool
pSorted xs = isPermutation xs (sort xs)
-- quickCheck pSorted
-- +++ OK, passed 100 tests.


-- Exercise 5
-- Solution: Curry
-- Time Taken: 1h
-- Most elegant using curry/uncurry.
-- Tests for both true and false cases
teq :: Eq a => (a, a) -> Bool
teq = uncurry (==)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement x y = (isPermutation x y) && (not $ any teq $ zip x y)

deran :: (Enum a, Eq a, Num a) => a -> [[a]]
deran n = filter (isDerangement [0..n-1]) $ permutations [0..n-1]

testIsDerangementTrue = all (uncurry isDerangement) [([1,2,3,4], [4,3,2,1]), ([1,2,3,4], [2,3,4,1])]
testIsDerangementFalse = all (not.(uncurry isDerangement)) [([1,2,3,4], [5,6,7,8]), ([1,2,3,4], [1,2,3,4]), ([1,1,1,1], [1,1,1,1])]

testIsDerangement :: Bool
testIsDerangement = testIsDerangementTrue && testIsDerangementFalse


-- Exercise 6
-- Solution: Jovan
-- Time taken: 30 min
-- Have done this exercise before in pre-master
-- Tests for letters only
-- Tests
-- Comments

-- -- First, give a specification of ROT13.

-- ROT13 = A cipher by rotating the alphabet 13 characters. So 'a' = 'n', 'b' = 'o'
-- etc.

-- -- Next, give a simple implementation of ROT13.

move13lower, move13upper :: Char -> Char
move13lower c = chr ((((ord c - 97 + 13)) `mod` 26) + 97)
move13upper c = chr ((((ord c - 65 + 13)) `mod` 26) + 65)

transform :: String -> String
transform [] = []
transform (c:cs) | ord c >= 65 && ord c < 90   = move13upper c : transform cs
                 | ord c >= 97 && ord c < 122  = move13lower c : transform cs
                 | otherwise                   = c : transform cs

rot13 :: String -> String
rot13 s = transform s

-- Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.
-- Test inverse
test1 = rot13 (rot13 "HENK") == "HENK"
--quickCheck test1

-- -- Test appliance only on letters.
test2 = rot13 "!@#$%^&*()1234567890-=" == "!@#$%^&*()1234567890-="


-- Exercise 7
-- WARNING: I haven't checked the IBAN formats by country. This can be done by
-- comparing the IBAN country code and validate with the list at https://en.wikipedia.org/wiki/International_Bank_Account_Number

-- Time spent: 1 hour
-- Jovan's solution
-- Solid tests both invalid and valid tests

removeSpaces :: String -> String
removeSpaces s = filter (/= ' ') s

moveFirst4ToEnd :: String -> String
moveFirst4ToEnd s = drop 4 s ++ take 4 s

charToInt :: Char -> String
charToInt c | elem (toUpper c) ['A'..'Z'] = show ([10..35] !! (ord (toUpper c) `mod` 65))
            | otherwise                   = c : []

replaceChars :: String -> String
replaceChars []     = []
replaceChars (c:cs) = charToInt c ++ replaceChars cs

strTransform :: String -> String
strTransform = replaceChars . moveFirst4ToEnd . removeSpaces

iban :: String -> Bool
iban s = ((read (strTransform s) :: Integer) `mod` 97) == 1

ibans = ["AL47 2121 1009 0000 0002 3569 8741", "AD12 0001 2030 2003 5910 0100", "AT61 1904 3002 3457 3201", "AZ21 NABZ 0000 0000 1370 1000 1944", "BH67 BMAG 0000 1299 1234 56", "BE62 5100 0754 7061", "BA39 1290 0794 0102 8494", "BG80 BNBG 9661 1020 3456 78", "HR12 1001 0051 8630 0016 0", "CY17 0020 0128 0000 0012 0052 7600", "CZ65 0800 0000 1920 0014 5399", "DK50 0040 0440 1162 43", "EE38 2200 2210 2014 5685", "FO97 5432 0388 8999 44", "FI21 1234 5600 0007 85", "FR14 2004 1010 0505 0001 3M02 606", "GE29 NB00 0000 0101 9049 17", "DE89 3704 0044 0532 0130 00", "GI75 NWBK 0000 0000 7099 453", "GR16 0110 1250 0000 0001 2300 695", "GL56 0444 9876 5432 10", "HU42 1177 3016 1111 1018 0000 0000", "IS14 0159 2600 7654 5510 7303 39", "IE29 AIBK 9311 5212 3456 78", "IL62 0108 0000 0009 9999 999", "IT40 S054 2811 1010 0000 0123 456", "JO94 CBJO 0010 0000 0000 0131 0003 02", "KW81 CBKU 0000 0000 0000 1234 5601 01", "LV80 BANK 0000 4351 9500 1", "LB62 0999 0000 0001 0019 0122 9114", "LI21 0881 0000 2324 013A A", "LT12 1000 0111 0100 1000", "LU28 0019 4006 4475 0000", "MK072 5012 0000 0589 84", "MT84 MALT 0110 0001 2345 MTLC AST0 01S", "MU17 BOMM 0101 1010 3030 0200 000M UR", "MD24 AG00 0225 1000 1310 4168", "MC93 2005 2222 1001 1223 3M44 555", "ME25 5050 0001 2345 6789 51", "NL39 RABO 0300 0652 64", "NO93 8601 1117 947", "PK36 SCBL 0000 0011 2345 6702", "PL60 1020 1026 0000 0422 7020 1111", "PT50 0002 0123 1234 5678 9015 4", "QA58 DOHB 0000 1234 5678 90AB CDEF G", "RO49 AAAA 1B31 0075 9384 0000", "SM86 U032 2509 8000 0000 0270 100", "SA03 8000 0000 6080 1016 7519", "RS35 2600 0560 1001 6113 79", "SK31 1200 0000 1987 4263 7541", "SI56 1910 0000 0123 438", "ES80 2310 0001 1800 0001 2345", "SE35 5000 0000 0549 1000 0003", "CH93 0076 2011 6238 5295 7", "TN59 1000 6035 1835 9847 8831", "TR33 0006 1005 1978 6457 8413 26", "AE07 0331 2345 6789 0123 456", "GB29 NWBK 6016 1331 9268 19"]

wrongIbans = ["GB29 AAAA 6016 1331 9268 19", "AE07 AAAA 2345 6789 0123 456"]

testProperIbans = all (==True) (map iban ibans)
-- quickCheck testProperIbans

testImproperIbans = all (==False) (map iban wrongIbans)
-- quickCheck testImproperIbans
