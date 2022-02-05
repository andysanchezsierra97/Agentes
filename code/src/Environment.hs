module Environment
  ( inserts,
    initEnvironment,
    changeEnvironment,
    remove,
    insert,
  )
where

import System.Random
import Types
import Utils

-- Inicia el Ambiente
initEnvironment :: Int -> Int -> ([[Cell]], StdGen)
initEnvironment n m = fillBoard (mkStdGen 10) [[Empty | y <- [0 .. m - 1]] | x <- [0 .. n - 1]]

--Pone aleatoriamente basura en casillas vacias del Ambiente
changeEnvironment :: StdGen -> [[Cell]] -> ([[Cell]], StdGen)
changeEnvironment gen board = putDirt gen [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)], elementAt (x, y) board == Empty] board
  where
    (n, m) = getLen board

-- Rellena el Ambiente de forma aleatoria
fillBoard :: StdGen -> [[Cell]] -> ([[Cell]], StdGen)
fillBoard gen board = (b_o, g_o)
  where
    (n, m) = getLen board
    _min = min n m
    (childs, gen1) = getRandom gen 1 _min
    (bots, gen2) = getRandom gen1 1 _min
    (dirts, gen3) = getRandom gen2 1 _min
    (obstacles, gen4) = getRandom gen3 1 _min
    (b_p, g_p) = createPlaypen gen4 board childs childs (0, 0)
    (b_c, g_c) = generateRandomCells g_p Child b_p childs
    (b_b, g_b) = generateRandomCells g_c Bot {cells = []} b_c bots
    (b_d, g_d) = generateRandomCells g_b Dirt b_b dirts
    (b_o, g_o) = generateRandomCells g_d Obstacle b_d obstacles

-- Crea el corral
createPlaypen :: StdGen -> [[Cell]] -> Int -> Int -> (Int, Int) -> ([[Cell]], StdGen)
createPlaypen gen board k count coor
  | k == count = createPlaypen gen (insert Playpen {cells = []} coor board) k (count -1) coor
  | count == 0 = (board, gen)
  | otherwise = createPlaypen gen1 b k (count -1) t
  where
    (b, t, gen1) = insertPlaypent gen board coor

-- Aumenta el tamaño del corral
insertPlaypent :: StdGen -> [[Cell]] -> (Int, Int) -> ([[Cell]], (Int, Int), StdGen)
insertPlaypent gen board coor = (insert Playpen {cells = []} t board, t, gen1)
  where
    neighbors = [(x, y) | (x, y, cell) <- getNeighbors False coor board, cell == Empty]
    len = length neighbors
    (r, gen1) = getRandom gen 0 (len -1)
    t = neighbors !! r

-- Ubica una lista de Elementos en el Ambiente
inserts :: Cell -> [(Int, Int)] -> [[Cell]] -> [[Cell]]
inserts _ [] board = board
inserts cell (t : xs) board = inserts cell xs (insert cell t board)

-- Ubica en el Ambiente una lista de elementos random
generateRandomCells :: StdGen -> Cell -> [[Cell]] -> Int -> ([[Cell]], StdGen)
generateRandomCells gen cell board k = (inserts cell ts board, gen1)
  where
    (ts, gen1) = randomSet gen k board []

-- Elimina el elemento de la casilla coor del Ambinte
remove :: (Int, Int) -> [[Cell]] -> [[Cell]]
remove coor board = [[if fst coor == x && snd coor == y then Empty else elementAt (x, y) board | y <- [0 .. m -1]] | x <- [0 .. n -1]]
  where
    (n, m) = getLen board

-- Inserta el elemento cell en la casilla coor del Ambinte
insert :: Cell -> (Int, Int) -> [[Cell]] -> [[Cell]]
insert cell coor board = [[if fst coor == x && snd coor == y then cell else elementAt (x, y) board | y <- [0 .. m -1]] | x <- [0 .. n -1]]
  where
    (n, m) = getLen board

--Dado una lista de casillas pone o no pone basura en ellas
putDirt :: StdGen -> [(Int, Int)] -> [[Cell]] -> ([[Cell]], StdGen)
putDirt gen [] board = (board, gen)
putDirt gen (x : xs) board = putDirt gen1 xs board'
  where
    (r, gen1) = getRandom gen 0 10
    board'
      | r == 1 = insert Dirt x board
      | otherwise = board