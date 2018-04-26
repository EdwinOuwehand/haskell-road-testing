
module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- Exercise 1 (1h)
-- This solution had most tests included. Definitions were all similar. 
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

---- | logical entailment 
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)
--entails (Impl p q) (Impl (Neg q) (Neg p))

---- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- Should return 
-- True
-- False
-- True
-- True
-- False
-- True
-- True
-- False
-- True
-- False
-- True
-- False
-- True
-- True
-- True

checkDefinitions = do {
    ; print $ contradiction (Cnj [p, (Neg p)])
    ; print $ contradiction (Cnj [p, (Neg q)])
    ; print $ contradiction (Cnj [(Dsj [p, q]), (Cnj [(Neg p), (Neg q)])])
    ; print $ tautology (Dsj [p, (Neg p)])
    ; print $ tautology (Dsj [p, (Neg q)])
    ; print $ tautology (Cnj [(Dsj [p, (Neg p)]), (Dsj [q, (Neg q)])])
    ; print $ entails (p) (p)
    ; print $ entails (p) (Neg p)
    ; print $ entails (Cnj [p, q]) (Dsj [p, q])
    ; print $ entails (Impl p q) (Impl q p)
    ; print $ equiv (p) (p)
    ; print $ equiv (p) (q)
    ; print $ equiv (Dsj [p, q]) (Dsj [q, p])
    ; print $ equiv (Cnj [p, q]) (Cnj [q, p])
    ; print $ equiv (Impl p q) (Dsj [(Neg p), q]) }


-- Exercise 2 (1h)
-- Only solutions actually using random forms. 
testParser' :: Form -> Bool
testParser' x = (== x).head.parse $ show x

-- Test a variety of logic constructions
testParser :: Bool
testParser = all testParser' [x | let q = [Prop i | i <- [1..5]],
                                  z <- [Cnj q, Dsj q, Prop 0],
                                  y <- [Impl z z, Equiv z z],
                                  x <- [y, Neg y]]

-- In addition to this, given the generator written for exercise 4,
-- just "quickCheck testParser'" now works too!
qcTestParser = quickCheck testParser'

instance Arbitrary (Form) where
    arbitrary = sized $ sizedArbitraryForm

sizedArbitraryForm :: Int -> Gen Form
sizedArbitraryForm 0 = do
        n <- choose (0, 100)
        return $ Prop n

sizedArbitraryForm n = do
        n <- choose (0, n)
        r <- choose (0, 100)
        -- divide by two to prevent gigantic formulas from being generated
        ts1 <- sizedArbitraryForm (n `div` 2)
        ts2 <- sizedArbitraryForm (n `div` 2)
        t <- elements [Prop r, Neg ts1, Cnj [ts1, ts2], Dsj [ts1, ts2], Impl ts1 ts2, Equiv ts1 ts2]
        return t


-- Exercise 3 (4h30)
-- Most compact solution, though we are not certain the double Neg checks are really necessary.
cnf :: Form -> Form 
cnf x = cnf' $ nnf $ arrowfree x

cnf' :: Form -> Form 
cnf' (Prop x) = Prop x
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' (Neg (Neg f)) = cnf' f
cnf' (Dsj [x, (Cnj [y, z])]) = Cnj [(Dsj [(cnf' x), y]), (Dsj [(cnf' x), z])]
cnf' (Dsj [(Cnj y), x]) = cnf' (Dsj [x, (Cnj y)])
cnf' (Dsj [x, y]) = Dsj [(cnf' x), (cnf' y)]
cnf' (Cnj [x, y]) = Cnj [(cnf' x), (cnf' y)]


-- Exercise 4 (1h)
cnfTest :: Form -> Bool
cnfTest f = equiv f (cnf f)

-- quickCheck cnfTest
-- Reusing the arbitrary Form defintion from exercise 2
