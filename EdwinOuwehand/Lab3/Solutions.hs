
module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- Exercise 1 (1h)
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


-- Exercise 2 (45m)
-- Tried to parse different operators in a couple of combinations, the output should be equal to the input. 
checkParser = do { 
    ; putStrLn (show $ parse "(1==>2)")
    ; putStrLn (show $ parse "+(1 2)")
    ; putStrLn (show $ parse "*(1 2)")
    ; putStrLn (show $ parse "+(-1 -2)")
    ; putStrLn (show $ parse "(1<=>2)")
    ; putStrLn (show $ parse "+((1<=>2) (-3==>4))") }


-- Exercise 3 (4h30m)
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

-- Some forms for testing:
-- equiv myForm (cnf myForm)
myForm  = (Impl (Dsj [p, q]) p)
myForm2 = (Cnj [r, (Dsj [q, p])])


-- Exercise 4 (3h)
-- Test: quickCheck prop_equiv
prop_equiv :: [Int] -> Bool
prop_equiv x = checkForm $ genForm (filter (<5) (map abs x))

checkForm :: Form -> Bool
checkForm f = equiv f (cnf f)

genForm :: [Int] -> Form
genForm x = head $ parse $ genForm' x

genForm' :: [Int] -> String
genForm' [] =  show 0
genForm' [x] = show x
genForm' (x:xs)
           | x == 0 = "-" ++(genForm' xs) 
           | x == 1 = "+("++(genForm' xs)++" "   ++(show $ head xs)++")"
           | x == 2 = "(" ++(genForm' xs)++"==>" ++(show $ head xs)++")"
           | x == 3 = "*("++(genForm' xs)++" "   ++(show $ head xs)++")"
           | x == 4 = "(" ++(genForm' xs)++"<=>" ++(show $ head xs)++")" 
           | otherwise = ""
