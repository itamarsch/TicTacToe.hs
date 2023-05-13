{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Terminal (terminalGame) where

import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Data.Either.Extra (maybeToEither)
import Data.Function (on)
import Data.Map.Strict (empty, findWithDefault)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import GameLogic (Board (..), Coordinates, Player (..), nextTurn, playAtSpot, squareSize, winner)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), clearScreen, setCursorPosition, setSGR)
import Text.Read (readMaybe)

showPlayerT :: Player -> Text.Text
showPlayerT = Text.pack . show

showBoard :: Board -> Text.Text
showBoard (Board board) =
  let rows = rowString <$> zip [0 ..] (replicate squareSize [0 .. (squareSize - 1)])

      rowString :: (Int, [Int]) -> Text.Text
      rowString (colIndex, row) =
        (Text.pack . show) colIndex
          <> " "
          <> Text.intercalate " | " ((\t -> findWithDefault " " (t, colIndex) textBoard) <$> row)

      textBoard = playerToSpotText <$> board

      playerToSpotText :: Player -> Text.Text
      playerToSpotText Circle = "0"
      playerToSpotText Cross = "x"

      prefix = "  " <> Text.intercalate "   " (Text.pack . show <$> [0 .. (squareSize - 1)]) <> "\n"
      rowDivider = "\n " <> Text.intercalate "+" (replicate squareSize "---") <> "\n"
   in prefix <> Text.intercalate rowDivider rows

getCoordsAndPlayTurn :: Board -> Player -> IO (Board, Coordinates)
getCoordsAndPlayTurn board turn = do
  TextIO.putStrLn $ showPlayerT turn <> " turn"
  coord <- getInputAndValidate parseCoorinate "Please Enter space seperated coordinates: x y"
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
parseNumber =
  let inBounds :: Int -> Either Text.Text Int
      inBounds x
        | x < squareSize && x >= 0 = Right x
        | otherwise = Left "Coordinate out of bounds"
   in inBounds <=< maybeToEither "Input has to be a number" . readMaybe . Text.unpack

parseCoorinate :: Parser Coordinates
parseCoorinate s =
  case Text.words s of
    [x, y] -> (liftA2 (,) `on` parseNumber) x y
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
      if turnCounter == (squareSize * squareSize)
        then TextIO.putStrLn "Tie!"
        else game newBoard (nextTurn turn) (turnCounter + 1)

terminalGame :: IO ()
terminalGame = do
  let initialBoard = Board empty
  printBoard initialBoard
  game initialBoard Cross 1
