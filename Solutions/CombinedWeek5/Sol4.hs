module Ex4 where

import Data.List
import System.Random
import Lecture5
import Data.Tuple


-- #####################################################################################################################
-- Lab Assignment 4
-- Amount of time taken: 2 hours
-- Discussion:     We have chosen this solution because it produces good examples and has good comments.
--                 Next to that, it also utilizes the 'minimalize' function that tries to minimalize the sudoku with 
--                 empty blocks
-- #####################################################################################################################

getPossForBlock i i2 = [
                           (i, i2),
                           (i, i2 + 1),
                           (i, i2 + 2),
                           (i + 1, i2),
                           (i + 1, i2 + 1),
                           (i + 1, i2 + 2),
                           (i + 2, i2),
                           (i + 2, i2 + 1),
                           (i + 2, i2 + 2)
                       ]

getRandomBlockPositions :: IO [(Int, Int)]
getRandomBlockPositions = do
                            random <- randomize [(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)]
                            let xs = take 3 random
                            let y = concatMap (\(a,b) -> getPossForBlock a b) xs
                            return y

genProblemBlock :: Node -> IO Node
genProblemBlock n = do ys <- randomize xs
                       pos1 <- getRandomBlockPositions
                       return (minimalize (emptyPositions n (pos1)) ys)
                where xs = filledPositions (fst n)

emptyPositions :: Node -> [(Row,Column)] -> Node
emptyPositions n [] = n
emptyPositions n (h:t) = emptyPositions (eraseN n h) t

main :: IO ()

main = do r <- genRandomSudoku
          putStr "\nOriginal Solution\n"
          showNode r
          s  <- genProblemBlock r
          putStr "\nProblem\n"
          showNode s
          putStr "\nSolved\n"
          showNode $ head $ solveNs [s]

-- It is easy to remove blocks from the solution but this makes it easier for the puzzle solver since
-- there are more correct solutions to the puzzle.
-- So when increasing the amount of empty block you are also increasing the amount of solutions
--
-- I am also seeing some different behaviour on the 'minimalize' function, which now
-- seems to have a harder time to minimilze the solution.
-- As you can see below I have included an output where 3 empty blocks were removed but the minimalize
-- function was not able to empty out any other numbers in the other non-empty blocks

-- Output:
-- Original Solution
-- +-------+-------+-------+
-- | 9 8 6 | 5 7 2 | 1 3 4 |
-- | 4 5 1 | 9 3 6 | 7 8 2 |
-- | 7 2 3 | 1 8 4 | 6 5 9 |
-- +-------+-------+-------+
-- | 2 9 5 | 4 6 3 | 8 7 1 |
-- | 3 6 8 | 7 1 9 | 4 2 5 |
-- | 1 4 7 | 8 2 5 | 3 9 6 |
-- +-------+-------+-------+
-- | 6 7 9 | 2 4 8 | 5 1 3 |
-- | 5 1 4 | 3 9 7 | 2 6 8 |
-- | 8 3 2 | 6 5 1 | 9 4 7 |
-- +-------+-------+-------+
--
-- Problem
-- +-------+-------+-------+
-- | 9   6 | 5 7   | 1   4 |
-- |   5   | 9     | 7     |
-- |     3 |     4 | 6     |
-- +-------+-------+-------+
-- |       |     3 |       |
-- |       |   1   |       |
-- |       | 8 2 5 |       |
-- +-------+-------+-------+
-- | 6   9 |       | 5     |
-- |     4 |       | 2 6 8 |
-- |     2 |       |     7 |
-- +-------+-------+-------+
--
-- Solved
-- +-------+-------+-------+
-- | 9 8 6 | 5 7 2 | 1 3 4 |
-- | 4 5 1 | 9 3 6 | 7 8 2 |
-- | 7 2 3 | 1 8 4 | 6 5 9 |
-- +-------+-------+-------+
-- | 2 9 5 | 4 6 3 | 8 7 1 |
-- | 3 6 8 | 7 1 9 | 4 2 5 |
-- | 1 4 7 | 8 2 5 | 3 9 6 |
-- +-------+-------+-------+
-- | 6 7 9 | 2 4 8 | 5 1 3 |
-- | 5 1 4 | 3 9 7 | 2 6 8 |
-- | 8 3 2 | 6 5 1 | 9 4 7 |
-- +-------+-------+-------+


-- Try two
-- Original Solution
-- +-------+-------+-------+
-- | 3 5 6 | 2 1 8 | 4 7 9 |
-- | 2 9 4 | 5 7 6 | 3 8 1 |
-- | 1 7 8 | 9 4 3 | 2 6 5 |
-- +-------+-------+-------+
-- | 9 6 5 | 1 8 4 | 7 3 2 |
-- | 8 1 2 | 7 3 5 | 6 9 4 |
-- | 7 4 3 | 6 9 2 | 1 5 8 |
-- +-------+-------+-------+
-- | 4 3 9 | 8 2 7 | 5 1 6 |
-- | 5 2 1 | 3 6 9 | 8 4 7 |
-- | 6 8 7 | 4 5 1 | 9 2 3 |
-- +-------+-------+-------+
--
-- Problem
-- +-------+-------+-------+
-- |   5 6 |       |       |
-- |   9   |   7   |       |
-- | 1 7   | 9   3 |       |
-- +-------+-------+-------+
-- |       | 1 8   |       |
-- |     2 |     5 |       |
-- | 7 4 3 |       |       |
-- +-------+-------+-------+
-- |       | 8     | 5 1   |
-- |       |   6 9 | 8   7 |
-- |       | 4     | 9   3 |
-- +-------+-------+-------+
--
-- Solved
-- +-------+-------+-------+
-- | 3 5 6 | 2 1 8 | 4 7 9 |
-- | 2 9 4 | 5 7 6 | 3 8 1 |
-- | 1 7 8 | 9 4 3 | 2 6 5 |
-- +-------+-------+-------+
-- | 9 6 5 | 1 8 4 | 7 3 2 |
-- | 8 1 2 | 7 3 5 | 6 9 4 |
-- | 7 4 3 | 6 9 2 | 1 5 8 |
-- +-------+-------+-------+
-- | 4 3 9 | 8 2 7 | 5 1 6 |
-- | 5 2 1 | 3 6 9 | 8 4 7 |
-- | 6 8 7 | 4 5 1 | 9 2 3 |
-- +-------+-------+-------+

-- Try 3
-- Original Solution
-- +-------+-------+-------+
-- | 6 9 8 | 3 2 4 | 7 1 5 |
-- | 5 4 1 | 8 7 9 | 2 3 6 |
-- | 2 3 7 | 1 5 6 | 9 4 8 |
-- +-------+-------+-------+
-- | 1 7 4 | 2 3 8 | 5 6 9 |
-- | 3 6 5 | 9 1 7 | 8 2 4 |
-- | 8 2 9 | 4 6 5 | 1 7 3 |
-- +-------+-------+-------+
-- | 4 1 6 | 5 9 2 | 3 8 7 |
-- | 9 8 3 | 7 4 1 | 6 5 2 |
-- | 7 5 2 | 6 8 3 | 4 9 1 |
-- +-------+-------+-------+
--
-- Problem
-- +-------+-------+-------+
-- |       |       | 7 1 5 |
-- |       |       | 2 3 6 |
-- |       |       | 9 4 8 |
-- +-------+-------+-------+
-- |       | 2 3 8 | 5 6 9 |
-- |       | 9 1 7 | 8 2 4 |
-- |       | 4 6 5 | 1 7 3 |
-- +-------+-------+-------+
-- | 4 1 6 | 5 9 2 | 3 8 7 |
-- | 9 8 3 | 7 4 1 | 6 5 2 |
-- | 7 5 2 | 6 8 3 | 4 9 1 |
-- +-------+-------+-------+
--
-- Solved
-- +-------+-------+-------+
-- | 6 4 8 | 3 2 9 | 7 1 5 |
-- | 5 9 1 | 8 7 4 | 2 3 6 |
-- | 2 3 7 | 1 5 6 | 9 4 8 |
-- +-------+-------+-------+
-- | 1 7 4 | 2 3 8 | 5 6 9 |
-- | 3 6 5 | 9 1 7 | 8 2 4 |
-- | 8 2 9 | 4 6 5 | 1 7 3 |
-- +-------+-------+-------+
-- | 4 1 6 | 5 9 2 | 3 8 7 |
-- | 9 8 3 | 7 4 1 | 6 5 2 |
-- | 7 5 2 | 6 8 3 | 4 9 1 |
-- +-------+-------+-------+


