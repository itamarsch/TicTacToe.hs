import Control.Applicative (liftA2, (<|>))
import Control.Monad (join)
import Data.Function (on)
import Data.List (intercalate, transpose)
import Data.Maybe (isJust)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), clearScreen, setCursorPosition, setSGR)
import Text.Read (readMaybe)

type Board = [BoardRow]

type BoardRow = [Maybe BoardSpot]

data BoardSpot = Cross | Circle deriving (Eq, Show)

nextTurn :: BoardSpot -> BoardSpot
nextTurn Cross = Circle
nextTurn Circle = Cross

showBoard :: Board -> String
showBoard board =
  "  0   1   2 \n"
    ++ intercalate divider (zipWith showRow [0 ..] board)
  where
    divider = " ---+---+---\n"

spotText :: Maybe BoardSpot -> String
spotText (Just Cross) = "x"
spotText (Just Circle) = "0"
spotText Nothing = " "

showRow :: Int -> BoardRow -> String
showRow index xs = show index ++ " " ++ intercalate " | " (map spotText xs) ++ " \n"

initialBoard :: Board
initialBoard = [[Nothing | _ <- [1 .. 3]] | _ <- [1 .. 3]]

modifyIndex :: Int -> (a -> a) -> [a] -> [a]
modifyIndex 0 f (x : xs) = f x : xs
modifyIndex index f (x : xs) = x : modifyIndex (index - 1) f xs
modifyIndex _ _ [] = error "Out of bounds"

playAtSpot :: (Int, Int) -> BoardSpot -> Board -> Maybe Board
playAtSpot (x, y) spot board = case getAtSpot (x, y) board of
  Nothing -> Just $ modifyIndex y (modifyIndex x $ const $ Just spot) board
  Just _ -> Nothing

getAtSpot :: (Int, Int) -> Board -> Maybe BoardSpot
getAtSpot (x, y) board = board !! y !! x

allEqMaybe :: Eq a => [a] -> Maybe a
allEqMaybe [] = Nothing
allEqMaybe xs | all (== head xs) xs = Just $ head xs
allEqMaybe _ = Nothing

winner :: Board -> Maybe BoardSpot
winner rows@[row1, row2, row3] =
  foldl1 (<|>) $
    isWin
      <$> [ row1,
            row2,
            row3,
            head columns,
            columns !! 1,
            columns !! 2,
            diag1,
            diag2
          ]
  where
    isWin = join . allEqMaybe
    columns = transpose rows
    diag1 = (\(i, v) -> v !! i) <$> zip [0 ..] rows
    diag2 = (\(i, v) -> v !! (2 - i)) <$> zip [0 ..] rows
winner _ = error "Board has more than 3 columns"

isTie :: Board -> Bool
isTie = all (all isJust)

getChordsAndPlayTurn :: Board -> BoardSpot -> IO Board
getChordsAndPlayTurn board turn = do
  putStrLn $ show turn ++ " turn"
  (x, y) <- getInputAndValidate parseIntTuple "Please Enter space seperated coordinates: x y"
  case playAtSpot (x, y) turn board of
    Nothing -> do
      setSGR [SetColor Foreground Dull Red]
      putStrLn "Invalid placement"
      setSGR [Reset]
      getChordsAndPlayTurn board turn
    Just newBoard -> return newBoard

type Validator a = (String -> Either String a)

getInputAndValidate :: Validator a -> String -> IO a
getInputAndValidate validator message = do
  putStrLn message
  input <- getLine
  case validator input of
    Right value -> return value
    Left errorMessage -> do
      setSGR [SetColor Foreground Dull Red]
      putStrLn errorMessage
      setSGR [Reset]
      getInputAndValidate validator message

parseNumber :: Validator Int
parseNumber s = case readMaybe s of
  Nothing -> Left "Input has to be number"
  Just a | a > 2 || a < 0 -> Left "Input has to be between 0-2"
  Just a -> Right a

parseIntTuple :: Validator (Int, Int)
parseIntTuple s =
  case words s of
    [x, y] -> liftA2 (,) (parseNumber x) (parseNumber y)
    _ -> Left "Input has to be two numbers"

printBoard :: Board -> IO ()
printBoard board = do
  setCursorPosition 0 0
  clearScreen
  putStrLn $ showBoard board

game :: Board -> BoardSpot -> IO ()
game board turn = do
  newBoard <- getChordsAndPlayTurn board turn
  printBoard newBoard
  case winner newBoard of
    Just win -> do
      putStrLn $ show win ++ " Won!"
    Nothing ->
      if isTie newBoard
        then putStrLn "Tie!"
        else game newBoard $ nextTurn turn

main :: IO ()
main = do
  printBoard initialBoard
  game initialBoard Cross
