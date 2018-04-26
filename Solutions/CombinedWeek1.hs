import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

prime' :: Int -> Bool
prime' n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes'  

primes :: [Integer]
primes = 2 : filter prime [3..] 

primes' :: [Int]
primes' = 2 : filter prime' [3..] 

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

-- Exercise 1
-- Workshop 2
ind1, ind2 :: Int -> Int
ind1 = \ n -> if n > 0 then n^2 + (ind1 $ n-1) else 0
ind2 = \ n -> (n*(n+1)*(2*n+1)) `div` 6

funcCompare :: (Int -> Int) -> (Int -> Int) -> Int -> Bool
funcCompare = \ p1 p2 x -> p1 x == p2 x

inductionTest1 :: [Int] -> Bool
inductionTest1 = \ xs -> all (funcCompare ind1 ind2) $ map abs xs

qinductionTest1 = quickCheck inductionTest1

-- Workshop 3
ind3, ind4 :: Int -> Int
ind3 = \ n -> if n > 0 then n^3 + (ind3 $ n-1) else 0
ind4 = \ n -> (n*(n+1) `div` 2)^2

inductionTest2 :: [Int] -> Bool
inductionTest2 = \ xs -> all (funcCompare ind3 ind4) $ map abs xs

qinductionTest2 = quickCheck inductionTest2 


-- Exercise 2
prodLen :: Int -> Bool
prodLen n = length(subsequences [1 .. n]) == (2^n)

qpodLen = quickCheck(\n -> prodLen (abs n))

-- Answer 1:
-- Its hard to test because the subsequences increases exponentially the more
-- combinations a subset contains. Therefore the first few lists can be easily
-- generated as their length is small. But as quickCheck increases the testing
-- lists, it gets tougher to generate the combinations and therefore the program
-- gets slower.

-- Answer 2:
-- In the testLab2 function above, we are actually testing if the length of subsets function is correct
-- We are not testing the actual implementation of the function to see if it returns the correct items


-- Exercise 3
qPerms = quickCheckResult(\ n -> length (permutations [1..n]) == product [1..n])

-- Again, this test takes a very long time because the possible permutations is exponentially
-- What we are testing with this function is not the actual content the permutations function is returning but
-- we are checking if the length of the return value of the function is correct.
-- When this is correct, we assume that the function is correct.


-- Exercise 4
reversablePrimes = filter (\x -> prime (reversal x)) (takeWhile (<10000) primes)

-- Test fails, since e.g. reversal 30 returns 3, thus information is lost. 
-- The function is usable in this context however, since this only applies to multitudes of 10, 
-- which are therefore never prime. 
testReversal = verboseCheckResult (\n -> n>=0 --> n == (reversal (reversal n)))


-- Exercise 5
findConsecPrimes :: Int -> [Int]
findConsecPrimes sumOf = filter (\n -> prime'(n)) (map (\n -> sum (takeSkip sumOf n primes')) [0..])

takeSkip :: Int -> Int -> [Int] -> [Int]
takeSkip takeNum skip xs = drop skip . take (takeNum+skip) $ xs

-- To answer the question how to test the correctness of this function, we first have to split it into the parts
-- we actually want to test. For instance, there is no point testing the sum, toInteger or isPrime function
-- However, we can test if the takeSkip function works as provided

-- Here, we test if the takeSkip function takes the right amount of items
testLab51 = quickCheckResult(\s -> length (takeSkip (abs s) 4 [0..]) == (abs s))

-- Here, we test if the takeSkip function drops the right amount of items
-- We can do this because we know the starting point of the lazy list
-- This is 0
testLab52 = quickCheckResult(\s -> head (takeSkip 4 (abs s) [0..]) == (abs s))


-- Exercise 6
findSmallest :: [Int] -- Head findSmallest
findSmallest = filter (\n -> (not (prime ((product (take n primes)) + 1)))) [1..]
 

-- Exercise 7
-- Used: https://www.codeproject.com/Tips/515367/Validate-credit-card-number-with-Mod-algorithm

-- | digits
-- The digits function converts a number, to a list
-- ie 123 to [1,2,3]
digits :: Integer -> [Integer]
digits n = map (\x -> read [x] :: Integer) (show n)

-- | luhn
-- @a the number that is to be converted to digits
luhn :: Integer -> Bool
luhn a = ((sumDoubledDigits (digits a)) + (sumSingledDigits (digits a))) `mod` 10 == 0

-- | sumDoubledDigits
-- @digitList the creditcard number in digits format
-- @return the sum of the doubled formula
sumDoubledDigits :: [Integer] -> Integer
sumDoubledDigits digitList = sum (map (\n -> if 2*n >= 10 then sum(digits (2*n)) else 2*n) (secondElement (reverse digitList)))

-- | sumSingledDigits
sumSingledDigits :: [Integer] -> Integer
sumSingledDigits digitList = sum (firstElement (reverse digitList))

-- | firstElement
firstElement :: [Integer] -> [Integer]
firstElement [x] = [x]
firstElement (x:y:[]) = [x]
firstElement (x:y:xs) = x:firstElement xs

-- | secondElement
secondElement :: [Integer] -> [Integer]
secondElement [x] = []
secondElement (x:y:[]) = [y]
secondElement (x:y:xs) = y:secondElement xs

-- | isAmex
isAmex :: Integer -> Bool
isAmex n = let d = (digits n)
               (x:y:_) = d in
           x == 3 && (y == 4 || y ==7) && length d == 15 && luhn n
-- | isMaster
isMaster :: Integer -> Bool
isMaster n = let d = (digits n)
                 (x:y:_) = d in
           x == 5 && y >= 1 && y <= 5 && length d == 16 && luhn n
-- | isVisa
isVisa :: Integer -> Bool
isVisa n = let d = (digits n) in
           head d == 4 && (length d == 13 || length d == 16) && luhn n

-- Used a custom generator to do the tests for the credit card
-- Credits to http://blog.nikosbaxevanis.com/2015/02/21/generators-and-the-choose-function/
takeFromList :: [a] -> Gen a
takeFromList xs =
    choose (0, length xs - 1) >>= \i -> return $ xs !! i

--Valid
testIsAmexDataProvider = [370641901055973,371147293582862,340705124058341,371963778807438]
testIsAmex = quickCheckResult(forAll (takeFromList testIsAmexDataProvider) isAmex)

testIsMasterDataProvider = [5175358392203733,5490914525114508,5291351292871369,5411131786792122]
testIsMaster = quickCheckResult(forAll (takeFromList testIsMasterDataProvider) isMaster)

testIsVisaDataProvider = [4730523804309979,4916298014443522,4556461911725558,4716887562297314]
testIsVisa = quickCheckResult(forAll (takeFromList testIsVisaDataProvider) isVisa)

testIsNotAmex = quickCheckResult(forAll (takeFromList testIsMasterDataProvider) (not.isAmex))
testIsNotMaster = quickCheckResult(forAll (takeFromList testIsVisaDataProvider) (not.isMaster))
testIsNotVisa = quickCheckResult(forAll (takeFromList testIsAmexDataProvider) (not.isVisa))


-- Exercise 8
accuses :: Boy -> Boy -> Bool

-- Matthew: Carl didn't do it, and neither did I.
accuses Matthew x = not (x == Matthew) && not (x == Carl)

-- Peter: It was Matthew or it was Jack.
accuses Peter x = (x == Matthew) || (x == Jack)

-- Jack: Matthew and Peter are both lying.
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)

-- Arnold: Matthew or Peter is speaking the truth, but not both.
accuses Arnold x = accuses Matthew x /= accuses Peter x

-- Carl: What Arnold says is not true.
accuses Carl x = not (accuses Arnold x)

-- | Get all the boys where b accuses x
accusers :: Boy -> [Boy]
accusers x = filter (\b -> accuses b x ) boys

-- | Get the guilty person who is accused by 3 people
guilty :: [Boy]
guilty = filter (\b -> length (accusers b) == 3) boys

-- | See who is honest
honest :: [Boy]
honest =  nub [ x | x <- boys, y <- guilty, accuses x y ]
