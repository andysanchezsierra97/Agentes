module Childs
  ( simulateChilds,
  )
where

import Environment
import System.Random
import Types
import Utils

-- devuelve la posicion del ultimo obtaculo de origin hacia la direcion dir
lastObstacle :: [[Cell]] -> (Int, Int) -> Direction -> (Int, Int)
lastObstacle board origin dir
  | isin && element == Obstacle = lastObstacle board newCoor dir
  | otherwise = origin
  where
    _dir = dirToTuple dir
    newCoor = (fst _dir + fst origin, snd _dir + snd origin)
    element = elementAt newCoor board
    isin = isIn newCoor (n, m)
    (n, m) = getLen board

-- Umpuja una serie de obstaculos consecutivos en la dirccion dir
push :: [[Cell]] -> (Int, Int) -> (Int, Int) -> Direction -> [[Cell]]
push board origin last dir = b2
  where
    _dir = dirToTuple dir
    (n, m) = getLen board
    (newLast, current) = ((fst last + fst _dir, snd last + snd _dir), (fst origin + fst _dir, snd origin + snd _dir))
    isValid
      | isInEdge dir last (n, m) = False
      | elementAt newLast board /= Empty = False
      | otherwise = True
    b1
      | isValid = insert Obstacle newLast board
      | otherwise = board
    b2
      | isValid = insert Child current (remove origin b1)
      | otherwise = board

-- Mueve el niño de la casilla origin hacia la direccion dir
move :: [[Cell]] -> (Int, Int) -> Direction -> [[Cell]]
move board origin dir
  | element == Empty = insert Child newPos (remove origin board)
  | element == Obstacle = push board origin last dir
  | otherwise = board
  where
    (n, m) = getLen board
    last = lastObstacle board newPos dir
    (newPos, element) = (getNewPos dir origin, elementAt newPos board)

-- El niño de la casilla coor ensucia o no el Ambiente
dirty :: StdGen -> [[Cell]] -> (Int, Int) -> ([[Cell]], StdGen)
dirty gen board coor
  | null empty = (board, gen)
  | otherwise =
    let len = length empty
        (r, gen1) = getRandom gen 0 (len -1)
        (x, y) = empty !! r

        (r1, gen2) = getRandom gen1 0 4
        put
          | r1 == 1 = insert Dirt (x, y) board
          | otherwise = board
     in (put, gen2)
  where
    neighbors = getNeighbors True coor board
    empty
      | null neighbors = []
      | otherwise = [(x, y) | (x, y, t) <- neighbors, t == Empty]

-- Simula un movimiento aleatorio de un niño
simulateChild :: StdGen -> [[Cell]] -> (Int, Int) -> ([[Cell]], StdGen)
simulateChild gen board coor = b
  where
    neighbors = [(x, y) | (x, y, cell) <- getNeighbors False coor board, cell == Empty || cell == Obstacle]
    b
      | null neighbors = (board, gen)
      | otherwise = dirty gen2 _move (x, y)
      where
        len = length neighbors
        (r, gen1) = getRandom gen 0 (len -1)
        (x, y) = neighbors !! r
        tmp = (x - fst coor, y - snd coor)
        (_move, gen2) = (move board coor (tupleToDir tmp), gen1)

-- Simula un movimiento aleatorio de todos los niño
simulateChilds :: StdGen -> [[Cell]] -> [(Int, Int)] -> ([[Cell]], StdGen)
simulateChilds gen board [] = (board, gen)
simulateChilds gen board (x : xs) = simulateChilds gen1 b1 xs
  where
    (b1, gen1) = simulateChild gen board x
