 
module Lab1 where
import Data.List
import Test.QuickCheck 

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


-- Exercise 1 (1h30m)
-- Workshop 2
eq1 :: Int -> Int 
eq1 = \ n -> sum( map (^2)[0 .. n])

eq2 :: Int -> Int
eq2 = \ n -> (n * (n + 1) * (2*n + 1)) `div` 6

--Always true for neg integers, actual test is ignored.
test1 = quickCheck(\n -> n>=0 --> (eq1 n == eq2 n)) 
test1Ver = verboseCheckResult(\n -> n>=0 --> (eq1 n == eq2 n)) 

-- Workshop 3
eq3 :: Int -> Int
eq3 = \ n -> sum( map (^3)[0 .. n])

eq4 :: Int -> Int
eq4 = \n -> ((n * (n + 1)) `div` 2)^2

test2 = quickCheck(\n -> n>=0 --> (eq3 n == eq4 n)) 
test2Ver = verboseCheckResult(\n -> n>=0 --> (eq3 n == eq4 n)) 


-- Exercise 2 (1h)
prodLen :: Int -> Bool
prodLen n = let len = length(subsequences [1 .. n]) in
                len == (2^n)

test3 = quickCheck(\n -> n>=0 --> prodLen n)


-- Exercise 3 (1h30m)
fact n = product [1..n]

test4 = quickCheck (\n -> n>=0 --> length(permutations([1 .. n])) == fact n)


-- Exercise 4 (30m)
reversablePrimes = filter (\x -> prime (reversal x)) (takeWhile (<10000) primes)

-- Test fails, since e.g. reversal 30 returns 3, thus information is lost. 
-- The function is usable in this context however, since this only applies to multitudes of 10, 
-- which are therefore never prime. 
testReversal = verboseCheckResult (\n -> n>=0 --> n == (reversal (reversal n)))


-- Exercise 5 (1h30m)
primeSum from = let to = from + 101 in 
    sum (take (to - from) (drop from primes))

prime101 = head (filter(\x -> prime x) (map(\x -> primeSum x) [0..]))


-- Exercise 6 (2h)
primeSet from range = let to = from + range in -- Should start running at 0, thus primeSet 0 x
    take (to - from) (drop from primes)

counterSet = head(filter(\x -> not (prime (product x + 1))) (map(\x -> primeSet 0 x) [0..]))
counterResult = product counterSet +1


-- Exercise 7 (3h)
-- src: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- src: https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

oddEl :: [Integer] -> [Integer]
oddEl [x] = [x]
oddEl (x:y:[]) = [x]
oddEl (x:y:xs) = x:oddEl xs

evenEl :: [Integer] -> [Integer]
evenEl [x] = []
evenEl (x:y:[]) = [y]
evenEl (x:y:xs) = y:evenEl xs

accountSum nr = sum (merge(evenEl nr) (map(\x -> if (x>=9) then sum (digs x) else x) (map(*2)(oddEl (nr)))))

luhn :: Integer -> Bool
luhn nr = let aNr = tail (reverse (digs nr))
              check = last (digs nr) 
              total = (accountSum aNr) + check in
              mod total 10 == 0

--isAmericanExpress, isMaster, isVisa :: Integer -> Bool
--isAmericanExpress n = 
--   x == 3 && (y == 4 || y == 7) &&
--   length (digs n) == 15 &&
--   luhn n
--   where (x:y:_) = toDigits n

--isMaster n = 
--   x == 5 && elem y [1..5] &&
--   length (toDigits n) == 16 &&
--   luhn n
--   where (x:y:_) = toDigits n

--isVisa n =
--   head digits == 4 &&
--   (l == 13 || l == 16) &&
--   luhn n
--   where digits = toDigits n; l = length digits


-- Exercise 8 (1h30m)
--accuses :: Boy -> Boy -> Bool
--accuses Maththew thief = not(Carl == thief) && not(Maththew == thief)
--accuses Peter thief = (Maththew == thief) || (Jack == thief)
--acusses Jack thief = 

--accusers :: Boy -> [Boy]

--guilty, honest :: [Boy]
