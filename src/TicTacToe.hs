module TicTacToe (gameOver, parsePosition, tryMove) where

import Data.List (nub)
import Text.Read (readMaybe)

import Board
import Cell
import Player
import Helpers

type Position = (Int, Int)

gameOver :: Board -> Bool
gameOver board = any winningLine (rows board ++ cols board ++ diags board)
  where
    winningLine :: [Cell] -> Bool
    winningLine xs = nub xs == [Taken X] || nub xs == [Taken O]

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition position = parseHelper (words position)
  where
    parseMaybe (Just x) (Just y) = Just (x, y)
    parseMaybe _ _ = Nothing
    parseHelper [x, y] = parseMaybe (readMaybe x) (readMaybe y)
    parseHelper _ = Nothing

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (i, j) (Board n cells) 
  | i >= n || j >= n || i < 0 || j < 0 = Nothing
  | otherwise = tryMoveHelper isEmptyResult
  where
    posWanted = i * n + j
    isEmptyResult = isEmpty (cells !! (posWanted)) 

    tryMoveHelper True = 
        Just (Board n (replace posWanted (Taken player) cells))
    tryMoveHelper False = Nothing