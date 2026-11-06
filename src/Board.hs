module Board where

import Cell (Cell(..))
import Player (Player(..))

import Data.List (transpose)

data Board = Board Int [Cell] deriving (Eq, Show)

-- | Returns an empty board with sides of the given length
emptyBoard :: Int -> Board
emptyBoard n = Board n (replicate (n * n) Empty)

-- | Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (Board n cs) = rows' cs
  where rows' [] = []
        rows' cs = r : rows' rs
          where (r, rs) = splitAt n cs

-- | Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols = transpose . rows

-- | Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (Board n cs) = [ [cs !! (k * (n + 1)) | k <- [0 .. n - 1]]
                     , [cs !! (k * (n - 1)) | k <- [1 .. n]]
                     ]

-------------------------------------------------------------------
testBoard1, testBoard2, testBoard3 :: Board
testBoard1 = Board 4 [ Taken O, Taken X, Empty,   Taken O
                     , Taken O, Empty,   Taken X, Taken X
                     , Taken O, Empty,   Empty,   Taken X
                     , Taken O, Taken X, Empty,   Empty
                     ]
testBoard2 = Board 2 [ Taken X, Empty
                     , Empty,   Empty
                     ]
testBoard3 = Board 5 [ Taken O, Taken X, Empty,   Taken O, Taken X
                     , Taken O, Empty,   Taken X, Taken X, Empty
                     , Empty,   Empty,   Taken X, Taken O, Taken O
                     , Taken O, Taken X, Empty,   Empty,   Taken X
                     , Taken X, Empty,   Taken O, Empty,   Empty
                     ]
