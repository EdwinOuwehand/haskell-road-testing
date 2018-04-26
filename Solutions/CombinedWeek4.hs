module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd
import Data.Tuple


-- #####################################################################################################################
-- Lab Assignment 1
-- Amount of time taken: 2 hours
-- #####################################################################################################################

-- We could use some extra explanation on paradoxes.
-- There were some troubles understanding the theory when discussed with the group


-- #####################################################################################################################
-- Lab Assignment 2
-- Amount of time taken: 30m hours
-- This solution was a union of two solutions: One containing the generators and the other containing the right tests
-- The QuickCheck generator was the same on the other solutions
-- #####################################################################################################################

-- While this does not convert a list to a Set directly,
-- it does transform it to a set-like list
toset :: (Ord a) => [a] -> [a]
toset = sort.nub


-- Sracth Version
-- More or less does the same as below, but for Int
-- lists only
--instance Arbitrary (Set Int) where
--    arbitrary = do
--        x <- genIntList
--        return $ Set x

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        x <- arbitrary
        return (Set $ toset $ x)


-- Inspired by: https://ilearn.ps.informatik.uni-kiel.de/public/assets/42576?style=original&1468318866
prop_isEmpty_empty :: Bool
prop_isEmpty_empty = isEmpty emptySet

prop_member_empty :: Int -> Bool
prop_member_empty x = not (inSet x emptySet)

prop_isEmpty_insert :: Int -> Set Int -> Bool
prop_isEmpty_insert x s = not (isEmpty (insertSet x s))

prop_member_delete :: Int -> Set Int -> Bool
prop_member_delete x s = not (inSet x (deleteSet x s))

prop_union_set :: Set Int -> Set Int -> Bool
prop_union_set setOne setTwo = let setUnified = (unionSet setOne setTwo) in
                        (subSet setOne setUnified)  && (subSet setTwo setUnified)

-- Alternate approach to getting deciding intersect
prop_intersect_set :: Set Int -> Set Int -> Bool
prop_intersect_set s1 s2 = let  intersect1 = (sIntersect s1 s2)
                                intersect2 = unionSet (s1 `sDifference` s2) (s2 `sDifference` s1) in
                        not $ subSet intersect1 intersect2 && not (isEmpty intersect1)


main :: IO ()
main = do
        putStr "prop_isEmpty_empty : "
        quickCheck prop_isEmpty_empty
        putStr "prop_member_empty : "
        quickCheck prop_member_empty
        putStr "prop_isEmpty_insert : "
        quickCheck prop_isEmpty_insert
        putStr "prop_member_delete : "
        quickCheck prop_member_delete
        putStr "prop_union_set : "
        quickCheck prop_union_set
        putStr "prop_intersect_set : "
        quickCheck prop_intersect_set
        
-- prop_isEmpty_empty : +++ OK, passed 1 tests.
-- prop_member_empty : +++ OK, passed 100 tests.
-- prop_isEmpty_insert : +++ OK, passed 100 tests.
-- prop_member_delete : +++ OK, passed 100 tests.
-- prop_union_set : +++ OK, passed 100 tests.
-- prop_intersect_set : +++ OK, passed 100 tests.       

-- #####################################################################################################################
-- Lab Assignment 3
-- Amount of time taken: 45m
-- This solution was picked because it is short, readable and correct.
-- We also have tests added to test some properties
-- #####################################################################################################################

sUnion :: Ord a => Set a -> Set a -> Set a
sUnion (Set xs) (Set ys) = Set $ toset $ xs ++ ys

sIntersect :: Ord a => Set a -> Set a -> Set a
sIntersect (Set xs) (Set ys) = Set $ toset $ xs `intersect` ys

sDifference :: Ord a => Set a -> Set a -> Set a
sDifference (Set xs) (Set ys) = Set $ toset $ xs \\ ys

-- xor function
-- Src: https://annevankesteren.nl/2007/02/haskell-xor
xor :: Bool -> Bool -> Bool
xor x y | x == True && y == False = True
        | x == False && y == True = True
        | otherwise = False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

xor'' :: Bool -> Bool -> Bool
xor'' True a = not a
xor'' False a = a

set2List :: Set a -> [a]
set2List (Set xs) = xs

prop_union_subset :: Set Int -> Set Int -> Bool
prop_union_subset x y = subSet x z && subSet y z
    where z = (sUnion x y)

prop_difference_inset :: Set Int -> Set Int -> Bool
prop_difference_inset x y = not (any (\ z -> inSet z (sDifference x y)) (set2List y))

prop_intersection_inset :: Set Int -> Set Int -> Bool
prop_intersection_inset x y = not (any (\ a -> (xor (inSet a x) (inSet a y))) z)
    where z = set2List (sIntersect x y)

-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken: 2 hours
-- #####################################################################################################################

-- Partitions are not clear

-- Exercise 5.115 is difficult to understand

-- #####################################################################################################################
-- Lab Assignment 5
-- Amount of time taken: 30 minutes
-- This solutions was picked because using the swap method makes it very readable and easy to understand
-- #####################################################################################################################

type Rel a = [(a,a)]

-- By using concatMap, I can return more items then are in the present list
-- I have included the swap method from Data.Tuple which swaps the tuple around
symClos :: Ord a => Rel a -> Rel a
symClos xs = concatMap (\n -> [n, swap n]) xs

-- symClos [(1,2),(2,3),(3,4)]
-- [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]

-- #####################################################################################################################
-- Lab Assignment 6
-- Amount of time taken: 3 hours
-- The solution has been chosen because the it uses the FixedPoint function and the at operator.
-- It is also the shortest we have seen so far.
-- #####################################################################################################################

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos xs = sort $ fp (\n -> xs ++ (n @@ n)) xs

-- trClos [(1,2),(2,3),(3,4)]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- #####################################################################################################################
-- Lab Assignment 7
-- Amount of time taken: 1 h

-- #####################################################################################################################

-- One of the first things that came to mind for symClos is testing list size,
-- after all, it a list should be twice the size after applying the relation.
-- Unfortunately, it is not. The inverse of a relation could already exist,
-- and a relation with itself does not duplicate. Therefore, it is not a
-- property that is usefull to test.

-- sCElemsTest checks if for every relation, its inverse relation also exists.
sCElemsTest :: Rel Int -> Bool
sCElemsTest xs = all (\ r -> r `elem` sc && (swap r) `elem` sc) xs where sc = symClos xs

-- Unfortunately, it appears to be impossible to create a functioning test for trClos.
-- This is because quickCheck generates looping relations (e.g. (a,b) -> (b,c) -> (c,a)),
-- which cannot be filtered out in trClos without altering the provided @@ operator.
-- This in contrary to self referencing relations (a,a), those are added to the list of
-- possible relations and then removed from the algorithm.
-- To show how looping relations could be handled without getting stuck, tC2' uses
-- filter (not.(flip elem cx)), thus removing already iterated over relations.

-- Should one want to test this without quickCheck, i.e. manually, either tC2 can be used
-- to compare the result to a different implementation of the trClos algorithm, or the
-- solutions should be predefined. One of these manual examples is mentioned in the exercise
-- on blackboard.

tC2' [] o __ = []
tC2' rs o cx = rxs ++ (tC2' rxs o (cx ++ rxs))
    where rxs = filter (not.(flip elem cx)) $ concatMap (\t -> map (\z -> (fst t, snd z)) $ filter (\z -> fst z == (snd t)) o) rs

tC2 rs = rs ++ (tC2' rs' rs' []) where rs' = filter (not.(uncurry (==))) rs

-- This would be how a possible test would look like:
tCTest :: Rel Int -> Bool
tCTest rs = (trClos rs) == (sort.nub $ tC2 rs)


-- #####################################################################################################################
-- Lab Assignment 8
-- Amount of time taken: 30 minutes
-- This solution was picked because it was the only one
-- #####################################################################################################################

assg8 = (trClos $ symClos [(1,2),(2,3),(3,4)]) == (symClos $ trClos [(1,2),(2,3),(3,4)])
-- The above function returns false.

-- So to answer the question if there is a difference between
-- the symmetric closure of the transitive closure and
-- the transitive closure of the symmetric closure
-- The answer is YES, there is a difference

-- The example is provided here:
-- trClos $ symClos [(1,2),(2,3),(3,4)]
-- [(1,1),(1,2),(1,2),(1,3),(1,4),(2,1),(2,1),(2,2),(2,3),(2,3),(2,4),(3,1),(3,2),(3,2),(3,3),(3,4),(3,4),(4,1),(4,2),(4,3),(4,3),(4,4)]

-- symClos $ trClos [(1,2),(2,3),(3,4)]
-- [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]

-- These two are different, proven by counterexample

