
module Lab2 where

import Data.List
import Data.Char 
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)


-- Exercise 1 (1h30m)
splitQuartile :: Float -> Float -> [Float] -> [Float]
splitQuartile lo hi list = filter (\x -> x>=lo && x<=hi) list

calcFrequency :: IO [Int]
calcFrequency = do 
    xs <- probs 10000
    let q1 = splitQuartile 0 0.25 xs
        q2 = splitQuartile 0.25 0.50 xs
        q3 = splitQuartile 0.50 0.75 xs
        q4 = splitQuartile 0.75 1 xs  in 
        return [length q1, length q2, length q3, length q4]


-- Exercise 2 (1h)
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

triangle :: Int -> Int -> Int -> Shape
triangle x y z = shape (reverse (sort[x, y, z]))


-- Exercise 3 (1h)
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

domain = [(-10)..10]

eq1, eq2, eq3 :: Integer -> Bool
eq1 = (\ x -> even x && x > 3)
eq2 = (\ x -> even x || x > 3)
eq3 = (\ x -> (even x && x > 3) || even x)

strongerList = do {
        ; putStrLn (show (stronger domain eq1 even))
        ; putStrLn (show (stronger domain eq2 even))
        ; putStrLn (show (stronger domain eq3 even))
        ; putStrLn (show (stronger domain even eq3)) }


-- Exercise 4 (1h30m)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = elem xs (permutations ys)

-- > quickCheck (identity | order | reversable)
identity :: Eq a => [a] -> Bool 
identity xs = isPermutation xs xs

order :: Ord a => [a] -> Bool
order xs = isPermutation xs (sort xs) && isPermutation (sort xs) xs

reverseable :: Eq a => [a] -> Bool
reverseable xs = isPermutation xs (reverse xs) && isPermutation (reverse xs) xs

-- TODO use weaker & stronger


-- Exercise 5 (1h30m)
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs

deran :: Int -> [[Int]]
deran n = let perms = permutations [0..n-1]
              derms = (\ x -> isDerangement x [0..n-1]) in
              filter derms perms

irreflexive, symmertrical  :: Ord a => [a] -> [a] -> Bool
irreflexive xs ys = isDerangement xs ys --> 
           ((null xs && null ys) || not (xs == ys))

symmertrical xs ys = isDerangement xs ys --> isDerangement ys xs 

--subPerms 



-- Exercise 6
rot13 :: String -> String




-- Exercise 7 (3h)
iban :: String -> Bool
iban x =  mod (iban' x) 97 == 1

iban' :: String -> Integer
iban' x = read (convert (rearrange x) []) :: Integer

rearrange :: String -> String
rearrange xs = let (ys,zs) = (take 4 xs, drop 4 xs) in zs++ys

-- Replace all Chars with by ascii values recursively 
convert :: String -> String -> String
convert [] ys = ys
convert (x:xs) ys = convert xs (ys ++ (charToNumber x))

-- Replace char with ascii value
-- Source: https://stackoverflow.com/questions/1706154/replacing-characters-with-numbers-in-haskell
charToNumber :: Char -> String
charToNumber x | isDigit x = [x]
               | otherwise = show (ord x - 55)
