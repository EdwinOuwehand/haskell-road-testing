module CombinedWeek6 where

-- stack install extra monad-loops

import Data.List
import Data.Char
import Data.Bits
import System.Random
import Control.Monad.Loops
import Control.Monad.Extra
import Lecture6 hiding (exM, composites, primeMR)

-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 2h
-- Discussion: Jovan's solution was the one with a good explanation
-- #####################################################################################################################

{--
  When the exponent is even, then by means of modulo squaring, x^y `mod` n can
  be reduced to (x^(y `div` 2) * x^(y `div` 2)) `mod` n because (a^2 * a^3) = a^(2+3) = a^5.
  By reducing the exponentiation by means of divide and conquer, it results in
  a less memory intensive and easier expression. Therefore making the final modulo
  less intensive. https://en.wikipedia.org/wiki/Modular_exponentiation
--}
exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x y n | even y    = dup_exM `mod` n
          | otherwise = (x * dup_exM) `mod` n
        where
          exM' = exM x (y `div` 2) n
          dup_exM = exM' * exM'

-- #####################################################################################################################
-- Lab Assignment 2
-- Amount of time taken:  10 min
-- Discussion: It was easy to see that emM was much faster, everyone had a good answer here.
-- #####################################################################################################################

-- When running:
-- expM 5 5555555555 221
-- It takes more then 30 seconds to complete

-- When running
-- exM 5 5555555555 221
-- It takes less then 1 second to complete

-- This concludes that exM is much more efficient then expM

-- #####################################################################################################################
-- Lab Assignment 3
-- Amount of time taken: 20m
-- Discussion: A combination of exercises was used. It was noted that we can start the algorithm at 4
-- instead of at 2.
-- #####################################################################################################################

composites :: [Integer]
composites = filter (not . prime) [4..]

-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken:   2h
-- Discussion: Steff's solution is elegant and clean and includes feedback on the exercise
-- #####################################################################################################################

testF k n = test primeTestsF k 0 (take n composites)

test anonFunc var1 var2 [] = print  "Done"
test anonFunc var1 var2 (naturalNumber:naturalNumbers) =
  do bool  <- anonFunc var1 naturalNumber
     if bool
         then do
              print ("Found false positive on " ++ show naturalNumber)
              test anonFunc var1 (var2+1) naturalNumbers
         else test anonFunc var1 var2 naturalNumbers

-- testF 1 2000
-- k=1 The lowest number I could find is 9
-- k=2 The lowest number I could find is 15
-- k=3 The lowest number I could find is 15
-- k=4 The lowest number I could find is 15
-- k=5 The lowest number I could find is 561
-- k=6 The lowest number I could find is 561
-- k=7 The lowest number I could find is 561
-- k=8 The lowest number I could find is 561
-- k=8 The lowest number I could find is 561

-- When increasing k, it becomes harder and harder to find false positives
-- When k=15 out of the 50 runs I did, only one false positive came back

-- #####################################################################################################################
-- Lab Assignment 5
-- Amount of time taken: 1h
-- Discussion: Jouke's solution used filterM and mapM which seems like a clean way to do it
-- #####################################################################################################################

smallestM ll = do
    l <- ll
    return $ minimum $ concat l

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
      k <- [2..],
      prime (6*k+1),
      prime (12*k+1),
      prime (18*k+1) ]

testPrimeTestsF = mapM (\z -> (filterM (primeTestsF 3) $ take 1000 carmichael)) [1..100]

smallestFooled = smallestM testPrimeTestsF

-- #####################################################################################################################
-- Lab Assignment 6
-- Amount of time taken:  45 min
-- Discussion: This exercise was chosen because of the output that is included in the exercise
-- #####################################################################################################################

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

-- #####################################################################################################################
-- Lab Assignment 7
-- Amount of time taken: 1h
-- Discussion: This version was chosen because of the good explanation and correct implementation
-- #####################################################################################################################

-- The Miller-Rabin primality check appears to be more accurate for larger numbers.
-- As long as it gets past the first few lower numbers, it rarely finds any false positives.

-- Miller-Rabin is therefore a suitable primality check for finding large mersenne primes.

findMersennePrimes :: Int -> IO ()
findMersennePrimes k = findMersennePrimes' primes k

findMersennePrimes' :: [Integer] -> Int -> IO ()
findMersennePrimes' (x:xs) k = do y <- primeMR k (2^x -1)
                                  if y then do print $ show (2^x -1) ++ " is a Mersenne Prime. "
                                               findMersennePrimes' xs k
                                  else do findMersennePrimes' xs k

-- findMersennePrimes 1
-- "3 is a Mersenne Prime. "
-- "7 is a Mersenne Prime. "
-- "31 is a Mersenne Prime. "
-- "127 is a Mersenne Prime. "
-- "8191 is a Mersenne Prime. "
-- "131071 is a Mersenne Prime. "
-- "524287 is a Mersenne Prime. "


-- #####################################################################################################################
-- Lab Assignment 8
-- Amount of time taken: 2h
-- Discussion: This solution was chosen because it was the only one
-- #####################################################################################################################

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n

findPrime :: Int -> IO Integer
findPrime k = do
   p  <- getStdRandom (randomR (2^(k-1), 2^k - 1))
   ok <- primeMR 10 p
   if ok then return p else findPrime k


findPair :: Int -> IO (Integer, Integer)
findPair bits = do
                first <- findPrime bits
                second <- findPrime bits
                if first == second then findPair bits
                else return (first,second)

keyGenerator k = do
                 (first, second) <- findPair k
                 let private = rsaPrivate first second
                 let public = rsaPublic first second
                 return (private, public)

assg8 message = do
                    (privateKey, publicKey) <- keyGenerator 10
                    print $ "Message is: " ++ show message
                    let messageEncoded = rsaEncode publicKey message
                    let messageDecoded = rsaDecode privateKey messageEncoded
                    print $ "Encoded message:" ++ show messageEncoded
                    print $ "Decoded message:" ++ show messageDecoded

-- assg8
-- *CombinedWeek6> assg8 123456
-- "Message is: 123456"
-- "Encoded message:274718"
-- "Decoded message:123456"

-- *CombinedWeek6> assg8 98765
-- "Message is: 98765"
-- "Encoded message:462706"
-- "Decoded message:98765"
