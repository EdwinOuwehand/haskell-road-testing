-- Exercise 3 (3h)

module Lecture5Sol3

where 

import Data.List
import System.Random
import Lecture5

-- > quickCheck randMinimalTest

randMinimalTest :: IO Bool
randMinimalTest = do { ; sud <- genRandomSudoku
					   ; constr <- randomize $ filledPositions (fst sud)
                       ; prob <- genProblem $ minimalize sud constr
                       ; return (uniqueSol prob) } 

testMinimalNTimes :: (Eq a, Num a) => a -> IO ()
testMinimalNTimes 0 = return ()
testMinimalNTimes n = do { ; t <- randMinimalTest
                           ; if t then putStrLn "Success" else putStrLn "Failed!"
                           ; testMinimalNTimes (n-1) }

-- > testMinimalNTimes 10
