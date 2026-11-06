module Main where

import TicTacToe
import Board
import Cell
import Player
import Helpers

import Text.Read (readMaybe)
import Data.List (intersperse)

import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))

prettyPrint :: Board -> IO ()
prettyPrint board = mapM_ printRow (rows board)
  where
    printRow row = putStrLn (unwords (map showCell row))

    showCell Empty = "-"
    showCell (Taken X) = "X"
    showCell (Taken O) = "O"

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- You might like to use one of the following signatures, as per the spec.
-- but this is up to you.
--
-- doParseAction :: (String -> Maybe a) -> IO a
-- doParseAction :: String -> (String -> Maybe a) -> IO a

prompt :: String -> (String -> Maybe a) -> IO a
prompt message parse = do
  putStrLn message
  input <- getLine
  check (parse input)
  where
    check (Just x) = pure x
    check Nothing = do
      putStrLn "Invalid input, try again."
      prompt message parse

-- | Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board player = prompt message attempt
  where
    message = "Player " ++ show player ++ ", make your move (row col):"

    attempt input = do
      position <- parsePosition input
      tryMove player position board

-- | Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board player = do
  prettyPrint board
  newBoard <- takeTurn board player
  continue newBoard
  where
    continue newBoard
      | gameOver newBoard = do
          prettyPrint newBoard
          putStrLn ("Player " ++ show player ++ " has won!")
      | boardFull newBoard = do
          prettyPrint newBoard
          putStrLn "It's a draw!"
      | otherwise = playGame newBoard (swap player)

    boardFull (Board _ cells) = all (not . isEmpty) cells

-- | Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = do
  disableBuffering -- don't remove!
  putStrLn "Welcome to tic tac toe on an N x N board"
  n <- prompt "Enter the board size (N):" parseSize
  playGame (emptyBoard n) startingPlayer
  putStrLn "Thank you for playing"
  where
    parseSize :: String -> Maybe Int
    parseSize input
      | Just n <- readMaybe input, n >= 1 = Just n
      | otherwise = Nothing

{-|
When ran via `cabal run`, Haskell will "buffer" the input and output streams
for performance. This is annoying, since it means that some printing can happen
out-of-order -- the strings are only written when a newline is entered!

There are a few ways of getting around this, like using `hFlush stdout` after
each print. However, in this case, it is simplest to just disable the buffering
altogether. This function *must* be called at the start of your `main` function.
-}
disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
