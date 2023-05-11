{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Terminal (terminalGame) where

import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Data.Either.Extra (maybeToEither)
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict (empty, findWithDefault)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import GameLogic (Board (..), Coordinates, Player (..), nextTurn, playAtSpot, winner)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), clearScreen, setCursorPosition, setSGR)
import Text.Read (readMaybe)

showPlayerT :: Player -> Text.Text
showPlayerT = Text.pack . show

showBoard :: Board -> Text.Text
showBoard (Board board) = foldl' replaceExclamation unfilledBoard $ flip (findWithDefault " ") textBoard <$> coordinates
  where
    playerToSpotText :: Player -> Text.Text
    playerToSpotText Circle = "0"
    playerToSpotText Cross = "x"

    textBoard = playerToSpotText <$> board
    -- Not Sure what I think about this implementations but it works and is pretty clean
    coordinates :: [Coordinates]
    coordinates = [(x, y) | y <- [0 .. 2], x <- [0 .. 2]]

    replaceExclamation :: Text.Text -> Text.Text -> Text.Text
    replaceExclamation boardText spotText = replaceOne "!" spotText boardText

    unfilledBoard =
      "  0   1   2 \n\
      \0 ! | ! | ! \n\
      \ ---+---+---\n\
      \1 ! | ! | ! \n\
      \ ---+---+---\n\
      \2 ! | ! | ! "

replaceOne :: Text.Text -> Text.Text -> Text.Text -> Text.Text
replaceOne pattern substitution text = Text.concat [front, substitution, Text.drop (Text.length pattern) back]
  where
    (front, back) = Text.breakOn pattern text

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

terminalGame :: IO ()
terminalGame = do
  let initialBoard = Board empty
  printBoard initialBoard
  game initialBoard Cross 1
