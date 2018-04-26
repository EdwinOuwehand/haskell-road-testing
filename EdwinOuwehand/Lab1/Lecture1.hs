
module Lecture1 where

import Data.List
import Test.QuickCheck

sentence = "Sentences can go " ++ onAndOn
onAndOn  = "on and " ++ onAndOn

sentences = "Sentences can go on":
             map (++ " and on") sentences

threefold :: Integer -> Bool
threefold n = rem n 3 == 0

threefolds = filter threefold [0..]

nats = [0..]

query1 = all (\ n -> any (\ m -> n < m) nats) nats

query2 = any (\ n -> all (\ m -> n <= m) nats) nats

forall = flip all
exist  = flip any         

query1' = forall nats (\ n -> exist nats (\ m -> n < m))

query2' = exist nats (\ n -> forall nats (\ m -> n <= m))

myall :: (a -> Bool) -> [a] -> Bool
myall p [] = True
myall p (x:xs) = p x && myall p xs

list2p :: Eq a => [a] -> a -> Bool
list2p = flip elem

myallTest :: [Int] -> [Int] -> Bool
myallTest = \ ys xs -> let p = list2p ys in
  all p xs == myall p xs

myall' p = foldr (\ x b -> p x && b) True

myallTest' :: [Int] -> [Int] -> Bool
myallTest' = \ ys xs -> let p = list2p ys in
  all p xs == myall' p xs

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ d -> not (divide d n)) [2..n-1]

isPrime' :: Integer -> Bool
isPrime' n = all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) [2..]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)

eprimes = sieve [2..]

least :: (Integer -> Bool) -> Integer

least p = head (filter p nats)       

least1 p = lst p 0
     where lst p n = if p n then n else lst p (n+1)

dif2 :: [Integer] -> [(Integer,Integer)]
dif2 (p:q:rs) = if p + 2 == q then (p,q) : dif2 (q:rs)
                else dif2 (q:rs)

primePairs = dif2 primes

dif6 :: [Integer] -> [(Integer,Integer,Integer)]
dif6 (p:q:r:ss) = if p + 6 == r then (p,q,r) : dif6 (q:r:ss)
                  else dif6 (q:r:ss)

primeTriples = dif6 primes

sol = take 100 primeTriples

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)

mersenne :: [Integer]
mersenne = [ p | p <- primes, prime (2^p - 1) ]

pythTriples :: [(Integer,Integer,Integer)]
pythTriples = filter (\ (x,y,z) -> x^2 + y^2 == z^2)  
   [ (x,y,z) | z <- [1..], x <- [1..z], y <- [1..z], x < y  ]

seq1 k = map sum [ [1..n] | n <- [0..k] ]
seq2 k = map (\ n -> (n * (n+1)) `div` 2) [0..k]

seq1seq2Test = \ n -> seq1 n == seq2 n 

f1, f2 :: Int -> Int
f1 = \ n -> sum [0..n]
f2 = \ n -> (n*(n+1)) `div` 2

test1 = quickCheckResult (\n -> f1 n == f2 n)

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

test1' = quickCheckResult (\n -> n >= 0 --> f1 n == f2 n)

