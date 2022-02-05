module BFS
  ( bfs,
    getNearestElement,
    getBestPos,
  )
where

import Types
import Utils

bfs :: [Cell] -> [(Int, Int)] -> [[Cell]] -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> [((Int, Int), Int)] -> ([((Int, Int), (Int, Int))], [((Int, Int), Int)])
bfs _ [] _ _ pi ds = (pi, ds)
bfs filters (u : q) board mark pi ds = bfs filters q' board mark' pi' ds'
  where
    neighbors = getNeighbors False u board
    neighbors1 = [(x, y, value) | (x, y, value) <- neighbors, (x, y) `notElem` mark, elementAt (x, y) board `elem` filters]
    d' = head [v | ((x, y), v) <- ds, (x, y) == u]
    ds' = ds ++ [((x, y), d' + 1) | (x, y, _) <- neighbors1]
    mark' = mark ++ [(x, y) | (x, y, _) <- neighbors1]
    q' = q ++ [(x, y) | (x, y, _) <- neighbors1]
    pi' = pi ++ [(u, (x, y)) | (x, y, _) <- neighbors1]

-- Busca el elemento mas cercano de tipo eType a la casilla coor
getNearestElement :: String -> [Cell] -> [[Cell]] -> (Int, Int) -> ((Int, Int), [((Int, Int), (Int, Int))])
getNearestElement eType filters board coor = (nearest, pi)
  where
    (pi, ds) = bfs filters [coor] board [coor] [] [(coor, 0)]
    tmp = [c | (c, v) <- ds, cellType (elementAt c board) == eType]
    nearest
      | null tmp = coor
      | otherwise = head tmp

getBestPos :: [((Int, Int), (Int, Int))] -> (Int, Int) -> (Int, Int) -> (Int, Int)
getBestPos pi coorO coorD
  | coorO == coorD = coorO
  | otherwise = pos
  where
    father = head [x | (x, y) <- pi, y == coorD]
    pos
      | father == coorO = coorD
      | otherwise = getBestPos pi coorO father