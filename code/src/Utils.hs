{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Utils
  ( getPercent,
    getRandom,
    getLen,
    isIn,
    isInEdge,
    elementAt,
    getNeighbors,
    randomCell,
    randomSet,
    tupleToDir,
    dirToTuple,
    getNewPos,
    printEnvironment,
    removeElement,
    getAllElements
  )
where

import System.Random
import Types

-- Porciento de las casillas vacias limpias
getPercent :: [[Cell]] -> Float
getPercent board = 100 - percent part total
  where
    (n, m) = getLen board
    total = length [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)], elementAt (x, y) board == Empty || elementAt (x, y) board == Dirt]
    part = length [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)], elementAt (x, y) board == Dirt]

percent :: Int -> Int -> Float
percent x y = 100 * (a / b)
  where
    a = fromIntegral x :: Float
    b = fromIntegral y :: Float

-- Imprimir el Ambiente
printEnvironment :: [[Cell]] -> Int -> IO ()
printEnvironment board (-1) = putStrLn ""
printEnvironment board n = do
  print (printRow (board !! (length board - n - 1)))
  printEnvironment board (n -1)

printRow :: [Cell] -> [String]
printRow = map cellToString

-- Random entre a y b
getRandom :: StdGen -> Int -> Int -> (Int, StdGen)
getRandom gen a b = randomR (a, b) gen :: (Int, StdGen)

-- Remover elemento de un array
removeElement :: [Cell] -> Cell -> [Cell]
removeElement cells cell = [c | c <- cells, c /= cell]

tupleToDir :: (Int, Int) -> Direction
tupleToDir dir
  | dir == (-1, 0) = North
  | dir == (1, 0) = South
  | dir == (0, 1) = East
  | dir == (0, -1) = West

dirToTuple :: Direction -> (Int, Int)
dirToTuple dir
  | dir == North = (-1, 0)
  | dir == South = (1, 0)
  | dir == East = (0, 1)
  | dir == West = (0, -1)

-- Dada la direccion y una casilla devuelve la siguinete casilla en dicha direccion
getNewPos :: Direction -> (Int, Int) -> (Int, Int)
getNewPos dir coorO = (fst _dir + fst coorO, snd _dir + snd coorO)
  where
    _dir = dirToTuple dir

-- Dimensiones del Ambiente
getLen :: [[Cell]] -> (Int, Int)
getLen board = (length board, length (head board))

-- Una casilla esta dentro del Ambiente
isIn :: (Int, Int) -> (Int, Int) -> Bool
isIn (x, y) (n, m)
  | x < 0 || x >= n || y < 0 || y >= m = False
  | otherwise = True

-- Una casilla esta en el borde del Ambiente
isInEdge :: Direction -> (Int, Int) -> (Int, Int) -> Bool
isInEdge dir (x, y) (n, m)
  | dir == North = x == 0
  | dir == South = x == (n -1)
  | dir == East = y == (m -1)
  | dir == West = y == 0

-- Elemento de un indixe en el array
elementAt :: (Int, Int) -> [[Cell]] -> Cell
elementAt coor board =
  let element = (board !! fst coor) !! snd coor
   in element

getAllElements :: Cell -> [[Cell]] -> [(Int,Int)]
getAllElements cell board = elements
  where
    (n,m) = getLen board
    elements = filter isType [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)]]
    isType (x,y) = elementAt (x,y) board == cell

-- all == true -> Las 8 casillas al redeodr de una casilla.
-- all == false -> Las 6 casillas adyacentes a una casilla.
getNeighbors :: Bool -> (Int, Int) -> [[Cell]] -> [(Int, Int, Cell)]
getNeighbors all coor board = [(r + fst d, c + snd d, elementAt (r + fst d, c + snd d) board) | d <- zip d1 d2, isIn (r + fst d, c + snd d) (n, m)]
  where
    (d1, d2)
      | all = ([-1, -1, -1, 1, 1, 1, 0, 0], [-1, 1, 0, -1, 1, 0, -1, 1])
      | otherwise = ([1, -1, 0, 0], [0, 0, 1, -1])
    (n, m) = getLen board
    (r, c) = coor

-- Devuelve una casilla vacia aleatoria del Ambiente
randomCell :: StdGen -> [[Cell]] -> ((Int, Int), StdGen)
randomCell gen board
  | element == Empty = ((f, s), gen2)
  | otherwise = randomCell gen2 board
  where
    (n, m) = getLen board
    (f, gen1) = getRandom gen 0 (n -1)
    (s, gen2) = getRandom gen1 0 (m -1)
    element = elementAt (f, s) board

-- Devielve un conjunto de casillas vacias de tamaño k
randomSet :: StdGen -> Int -> [[Cell]] -> [(Int, Int)] -> ([(Int, Int)], StdGen)
randomSet gen 0 board set = (set, gen)
randomSet gen k board set
  | t `elem` set = randomSet gen1 k board set
  | otherwise = randomSet gen1 (k -1) board (t : set)
  where
    (t, gen1) = randomCell gen board