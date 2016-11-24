module P096 where

import Data.List (intersperse)
import System.Random (getStdGen)
import Data.Maybe (fromJust)

import Sudoku.Backtracking (solve)
import Sudoku.Parser (parseBoard)
import Sudoku.Board (Board, prettyPrint, isSolved, getCell)

run :: IO ()
run = do
  boards <- readBoards
  rng <- getStdGen
  print . sum . map (topLeftNum . fromJust . solve rng) $ boards

topLeftNum :: Board -> Integer
topLeftNum board = read . concatMap show $ cells
  where
    cells = map (head . getCell board) [(0,0), (0,1), (0,2)]

readBoards :: IO [Board]
readBoards = do
  text <- readFile "resources/p096_sudoku.txt"
  let numericLines = filter (('G' /=) . head) . lines $ text
  return . map extract . boards . map fixLine $ numericLines
  where
    fixLine = intersperse ' ' . map (\c -> if c == '0' then '_' else c)
    boards [] = []
    boards allLines = (parseBoard . ("3 3\n"++) . unlines . take 9) allLines : boards (drop 9 allLines)
    extract (Left err) = error (show err)
    extract (Right board) = board
