-- My lsp for some reason prefers this when formatting
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (guard, join, (<=<))
import Data.Either.Extra (maybeToEither)
import Data.Foldable (foldl')
import Data.List (foldl1')
import Data.Map (Map, alter, empty, (!?))
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), clearScreen, setCursorPosition, setSGR)
import Text.Read (readMaybe)

type Coordinates = (Int, Int)

data Player = Cross | Circle deriving (Eq, Show)

showPlayerT :: Player -> Text.Text
showPlayerT = Text.pack . show

newtype Spot = Spot (Maybe Player)

instance Show Spot where
  show = Text.unpack . showSpotT

showSpotT :: Spot -> Text.Text
showSpotT (Spot (Just Cross)) = "x"
showSpotT (Spot (Just Circle)) = "0"
showSpotT (Spot Nothing) = " "

newtype Board = Board (Map Coordinates Player)

instance Show Board where
  show = Text.unpack . showBoard

showBoard :: Board -> Text.Text
showBoard (Board board) = foldl' replaceExclamation unfilledBoard $ Spot . (board !?) <$> coordinates
  where
    -- Not Sure what I think about this implementations but it works and is pretty clean
    coordinates :: [Coordinates]
    coordinates = [(x, y) | y <- [0 .. 2], x <- [0 .. 2]]
    replaceExclamation :: Text.Text -> Spot -> Text.Text
    replaceExclamation boardText spot = replaceOne "!" (showSpotT spot) boardText

    unfilledBoard =
      "  0   1   2 \n\
      \0 ! | ! | ! \n\
      \ ---+---+---\n\
      \1 ! | ! | ! \n\
      \ ---+---+---\n\
      \2 ! | ! | ! "

nextTurn :: Player -> Player
nextTurn Cross = Circle
nextTurn Circle = Cross

replaceOne :: Text.Text -> Text.Text -> Text.Text -> Text.Text
replaceOne pattern substitution text = Text.concat [front, substitution, Text.drop (Text.length pattern) back]
  where
    (front, back) = Text.breakOn pattern text

playAtSpot :: Coordinates -> Player -> Board -> Maybe Board
playAtSpot coord spot (Board board) = case board !? coord of
  Nothing -> Just $ Board $ alter (const $ Just spot) coord board
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

getCoordsAndPlayTurn :: Board -> Player -> IO (Board, Coordinates)
getCoordsAndPlayTurn board turn = do
  TextIO.putStrLn $ showPlayerT turn <> " turn"
  coord <- getInputAndValidate parseIntTuple "Please Enter space seperated coordinates: x y"
  case playAtSpot coord turn board of
    Nothing -> do
      setSGR [SetColor Foreground Dull Red]
      TextIO.putStrLn "Invalid placement"
      setSGR [Reset]
      getCoordsAndPlayTurn board turn
    Just newBoard -> pure (newBoard, coord)

type Parser a = (Text.Text -> Either Text.Text a)

getInputAndValidate :: Parser a -> Text.Text -> IO a
getInputAndValidate validator message = do
  TextIO.putStrLn message
  input <- TextIO.getLine
  case validator input of
    Right value -> pure value
    Left errorMessage -> do
      setSGR [SetColor Foreground Dull Red]
      TextIO.putStrLn errorMessage
      setSGR [Reset]
      getInputAndValidate validator message

parseNumber :: Parser Int
parseNumber = inBounds <=< maybeToEither "Input has to be a number" . readMaybe . Text.unpack
  where
    inBounds :: Int -> Either Text.Text Int
    inBounds x
      | x < 3 && x >= 0 = Right x
      | otherwise = Left "Coordinate out of bounds"

parseIntTuple :: Parser Coordinates
parseIntTuple s =
  case Text.words s of
    [x, y] -> liftA2 (,) (parseNumber x) (parseNumber y)
    _ -> Left "Input has to be two numbers"

printBoard :: Board -> IO ()
printBoard board = do
  setCursorPosition 0 0
  clearScreen
  TextIO.putStrLn $ showBoard board

game :: Board -> Player -> Int -> IO ()
game board turn turnCounter = do
  (newBoard, coords) <- getCoordsAndPlayTurn board turn
  printBoard newBoard
  case winner newBoard coords of
    Just win ->
      TextIO.putStrLn $ showPlayerT win <> " Won!"
    Nothing ->
      if turnCounter == 9
        then TextIO.putStrLn "Tie!"
        else game newBoard (nextTurn turn) (turnCounter + 1)

main :: IO ()
main = do
  let initialBoard = Board empty
  printBoard initialBoard
  game initialBoard Cross 1
