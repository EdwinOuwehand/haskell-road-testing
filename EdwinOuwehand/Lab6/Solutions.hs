
module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck  
import Lecture6 hiding (exM, composites)

-- n = 8x + 9y, n>=63
-- Exercise 1 (30m)
--x^y mod n
exM :: Integer -> Integer -> Integer -> Integer
exM x y n = let z = exM x (y `div` 2) n
                w = (z*z) `mod` n in 
                    if y == 0 then 1
                    else if even y then w
                    else (x*w) `mod` n 


-- Exercise 2 (20m)
{-|
    exM  324 42320320 7232
    expM 324 42320320 7232

    exM  3240 42320320 7232
    expM 3240 42320320 7232

    exM  32400 42320320 7232
    expM 32400 42320320 7232
    This calculation already amounts to a remarkable difference in time efficiency. 
-}


-- Exercise 3 (20m)
composites :: [Integer]
composites = filter (not . prime) [1..]

-- Src: Lecture 1
--prime :: Integer -> Bool
--prime n = n > 1 && all (\ x -> rem n x /= 0) xs
--  where xs = takeWhile (\ y -> y^2 <= n) primes
-- Another algorithm is already included in lecture 6.


-- Exercise 4 (1h30m)
findLeastComposite :: [Integer] -> Int -> IO ()
findLeastComposite (x:xs) k = do y <- primeTestsF k x
                                 if y then do print $ show x
                                 else findLeastComposite xs k 

{-|
    *Lab6> findLeastComposite composites 1
    "9"
    *Lab6> findLeastComposite composites 1
    "45"
    *Lab6> findLeastComposite composites 1
    "28"
    *Lab6> findLeastComposite composites 1
    "55"
    *Lab6> findLeastComposite composites 1
    "9"

    *Lab6> findLeastComposite composites 2
    "21"
    *Lab6> findLeastComposite composites 2
    "91"
    *Lab6> findLeastComposite composites 2
    "481"
    *Lab6> findLeastComposite composites 2
    "225"
    *Lab6> findLeastComposite composites 2
    "703"

    *Lab6> findLeastComposite composites 3
    "703"
    *Lab6> findLeastComposite composites 3
    "301"
    *Lab6> findLeastComposite composites 3
    "341"
    *Lab6> findLeastComposite composites 3
    "561"
    *Lab6> findLeastComposite composites 3
    "2821"
    *Lab6> 
-}


-- Exercise 5 (45m)
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

--"294409" immidiately or find "56052361" after a minute orso. 
-- Carmichael numbers appear to almost always fail the test immidiately. 


-- Exercise 6 (45m)
-- Copied from exercise 4
findLeastCompositeMR :: [Integer] -> Int -> IO ()
findLeastCompositeMR (x:xs) k = do y <- primeMR k x
                                   if y then do print $ show x
                                   else findLeastCompositeMR xs k 

{-|
    *Lab6> findLeastCompositeMR composites 1
    "15"
    *Lab6> findLeastCompositeMR composites 1
    "66"
    *Lab6> findLeastCompositeMR composites 1
    "15"
    *Lab6> findLeastCompositeMR composites 1
    "9"
    *Lab6> findLeastCompositeMR composites 1
    "51"
    *Lab6> findLeastCompositeMR composites 1
    "27"
    *Lab6> findLeastCompositeMR composites 2
    "973"
    *Lab6> findLeastCompositeMR composites 2
    "4187"
    *Lab6> findLeastCompositeMR composites 2
    "49"
    *Lab6> findLeastCompositeMR composites 2
    "3277"
    *Lab6> findLeastCompositeMR composites 2
    ^CInterrupted.
    *Lab6> findLeastCompositeMR composites 3
    ^CInterrupted.
-}

-- The Miller-Rabin primality check appears to be more accurate for larger numbers.
-- As long as it gets past the first few lower numbers, it rarely finds any false positives.


-- Exercise 7 (1h)
--mersennes :: [Integer]
--mersennes = map (\x -> 2^x-1) [1..]
findMersennePrimes :: Int -> IO ()
findMersennePrimes k = findMersennePrimes' primes k

findMersennePrimes' :: [Integer] -> Int -> IO ()
findMersennePrimes' (x:xs) k = do y <- primeMR k (2^x -1)
                                  if y then do print $ show (2^x -1) ++ " is a Mersenne Prime. "
                                               findMersennePrimes' xs k
                                  else do findMersennePrimes' xs k
