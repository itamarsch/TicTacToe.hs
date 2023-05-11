module GameLogic (Board (..), Coordinates, winner, Player (..), nextTurn, playAtSpot) where

import Control.Applicative ((<|>))
import Control.Monad (guard, (<=<))
import Data.List (foldl1')
import Data.Map.Strict (Map, insert, (!?))

type Coordinates = (Int, Int)

data Player = Cross | Circle deriving (Eq, Show)

newtype Board = Board (Map Coordinates Player)

nextTurn :: Player -> Player
nextTurn Cross = Circle
nextTurn Circle = Cross

playAtSpot :: Coordinates -> Player -> Board -> Maybe Board
playAtSpot coord spot (Board board) = case board !? coord of
  Nothing -> Just $ Board $ insert coord spot board
  Just _ -> Nothing

unrepeat :: Eq a => [a] -> Maybe a
unrepeat (x : xs) = guard (all (== x) xs) >> pure x
unrepeat _ = Nothing

winner :: Board -> Coordinates -> Maybe Player
winner (Board board) (x, y) =
  foldl1' (<|>) $ isWin <$> winningChecks
  where
    winningChecks
      -- Center
      | x == 1 && y == 1 = [row, column, diag2, diag1]
      -- True if coordinates aren't part of diagonal
      | (x == 1 || y == 1) && (x /= y) = [row, column]
      -- True for diag 1
      | x == y = [row, column, diag1]
      -- True for diag 2
      | otherwise = [row, column, diag2]

    isWin = unrepeat <=< traverse (board !?)

    diag1 = [(n, n) | n <- [0 .. 2]]
    diag2 = [(2 - n, n) | n <- [0 .. 2]]
    row = [(x', y) | x' <- [0 .. 2]]
    column = [(x, y') | y' <- [0 .. 2]]
