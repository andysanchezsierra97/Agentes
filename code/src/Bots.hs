{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Bots
  ( simulateRobots,
    getAllBot,
  )
where

import BFS
import Childs
import Environment
import Types
import Utils

--Suelta al niño que tiene cargad en la casilla del corral donde esta parado
dropChild :: (Int, Int) -> [[Cell]] -> [[Cell]]
dropChild = insert Playpen {cells = [Bot {cells = []}, Child]}

--Se mueve el robot de la casilla origin hacia la direccion dir
move :: Cell -> [[Cell]] -> (Int, Int) -> Direction -> [[Cell]]
move bot board origin dir
  -- Robot en el corral y se va a mover para otra casilla del corral
  | isInPlaypent && isPlaypent =
    let isChild = Child `elem` cells (elementAt origin board)
        newCells
          | element == Playpen {cells = []} = _cells
          | otherwise = Child : _cells
        newCell
          | isChild = Playpen {cells = [Child]}
          | otherwise = Playpen {cells = []}
        b1 = insert newCell origin board
     in insert Playpen {cells = [Bot {cells = newCells}]} coorD b1
  -- Robot en el corral y se va a mover hacia afuera del corral
  | isInPlaypent =
    let isChild = Child `elem` cells (elementAt origin board)
        newCells
          | element == Empty = _cells
          | otherwise = element : _cells
        newCell
          | isChild = Playpen {cells = [Child]}
          | otherwise = Playpen {cells = []}
        b1 = insert newCell origin board
     in insert Bot {cells = newCells} coorD b1
  -- Robot afuera del corral y se va mover hacia adentro del corral
  | isPlaypent =
    let dirt = Dirt `elem` _cells
        removedDirt = removeElement _cells Dirt
        newBot
          | dirt = Bot {cells = removedDirt}
          | otherwise = Bot {cells = _cells}
        newCell
          | element == Playpen {cells = []} = newBot
          | otherwise = if dirt then Bot {cells = Child : removedDirt} else Bot {cells = Child : _cells}
        b1
          | dirt = insert Dirt origin board
          | otherwise = insert Empty origin board
     in insert Playpen {cells = [newCell]} coorD b1
  -- Robot fuera del corral en una casilla que no hay basura (no tine que dejar la basura)
  | Dirt `notElem` _cells =
    let newCells
          | element == Empty = _cells
          | otherwise = element : _cells
     in insert Bot {cells = newCells} coorD (remove origin board)
  | otherwise =
    let removedDirt = removeElement _cells Dirt
        newCells
          | element == Empty = removedDirt
          | otherwise = element : removedDirt
     in insert Bot {cells = newCells} coorD (insert Dirt origin (remove origin board))
  where
    isInPlaypent = bot `elem` cells (elementAt origin board)
    _cells = cells bot
    coorD = getNewPos dir origin
    element = elementAt coorD board
    isPlaypent = cellType element == "Playpent"

--Limpia la casilla coor
clean :: [[Cell]] -> (Int, Int) -> [[Cell]]
clean board coor = insert bot {cells = newCells} coor board
  where
    bot = elementAt coor board
    newCells = removeElement (cells bot) Dirt

--Devuelve todos los roboses que estan en el Ambiente
getAllBot :: [[Cell]] -> [(Int, Int)]
getAllBot board = bots ++ botP
  where
    (n, m) = getLen board
    all = [(x, y) | x <- [0 .. (n -1)], y <- [0 .. (m -1)]]
    bots = filter isBot all
      where
        -- Todos los robot que no esten en el corral
        isBot (x, y) = cellType (elementAt (x, y) board) == "Bot"

    playpents = filter isPlaypent all
      where
        -- Todos las casillas del corral
        isPlaypent (x, y) = cellType (elementAt (x, y) board) == "Playpent"

    botP = filter isBotInPlayment playpents
      where
        -- Todos los robot que esten en el corral
        isBotInPlayment (x, y)
          | null (cells p) = False
          | otherwise = cellType (head (cells p)) == "Bot"
          where
            p = elementAt (x, y) board

--Robot que esta en la casilla coor
getBot :: [[Cell]] -> (Int, Int) -> Cell
getBot board coor
  | cellType element == "Bot" = element
  | otherwise = head (cells element)
  where
    element = elementAt coor board

--Devuelve todos los movimientos que puede realizar el robot en el momento
possiblesMoves :: [[Cell]] -> (Int, Int) -> [(Int, Int)]
possiblesMoves board coor = neighbors
  where
    bot = getBot board coor

    types1 = [Empty, Dirt, Playpen {cells = []}]
    types2 = [Empty, Dirt, Child, Playpen {cells = []}, Playpen {cells = [Child]}]

    neighbors
      | Child `elem` cells bot = [(x, y) | (x, y, cell) <- getNeighbors False coor board, cell `elem` types1]
      | otherwise = [(x, y) | (x, y, cell) <- getNeighbors False coor board, cell `elem` types2]

--Devuelve todas las acciones que puede realizar el robot en el momento
possiblesAccions :: [[Cell]] -> (Int, Int) -> [Accion]
possiblesAccions board coor = accions
  where
    bot = getBot board coor
    moves = possiblesMoves board coor
    cType = cellType (elementAt coor board)

    accions
      --No tiene niño cargado, no esta en una casilla con basura y tiene posibles movimientos -> Se tiene que mover
      | Child `notElem` cells bot && Dirt `notElem` cells bot && not (null moves) = [Move]
      --No tiene niño cargado, esta en una casilla con basura y tienes posibles movimientos -> Se puede mover o limpiar la basura
      | Child `notElem` cells bot && Dirt `elem` cells bot && not (null moves) = [Move, Clean]
      --No tiene niño cargado, esta en una casilla con basura y no tiene posibles movientos -> Tiene que limpiar la basura
      | Child `notElem` cells bot && Dirt `elem` cells bot = [Clean]
      --Tiene niño cargado, esta en una casilla del corral y tiene posibles movientos -> Puede soltar al niño o moverse
      | Child `elem` cells bot && cType == "Playpent" && not (null moves) = [Move, Drop]
      --Tiene niño cargado, esta en una casilla del corral y no tiene posibles movientos -> tiene que soltar al niño
      | Child `elem` cells bot && cType == "Playpent" = [Drop]
      --Tiene niño cargado, no esta en una casilla del corral y tiene posibles movientos -> Se tiene que mover
      | Child `elem` cells bot && not (null moves) = [Move]
      -- En otro caso no hace nada
      | otherwise = [Pass]

--Segun el tipo de robot, devuelve el mejor movimiento que puede realizar el robot en el momento
bestMove :: Int -> [[Cell]] -> (Int, Int) -> (Int, Int)
bestMove 0 board coor = getBestPos pi coor n
  where
    filters = [Dirt, Empty, Playpen {cells = []}]
    -- Buscar la basura mas cercana
    (n, pi) = getNearestElement "Dirt" filters board coor
bestMove 1 board coor = getBestPos pi coor n
  where
    bot = getBot board coor
    filters
      | Child `elem` cells bot = [Empty, Dirt, Playpen {cells = []}]
      | otherwise = [Empty, Dirt, Child, Playpen {cells = []}]
    childs = getAllElements Child board
    eType
      | null childs = "Dirt"
      | otherwise = "Child"

    (n, pi)
      -- Tiene niño cargado, se mueve para la casilla mas cercana del corral
      | Child `elem` cells bot = getNearestElement "Playpent" filters board coor
      -- No quedan niños fuera del coora, se mueve para la basura mas cercana
      | null childs = getNearestElement "Dirt" filters board coor
      -- Se mueve para el niño mas cercano
      | otherwise = getNearestElement "Child" filters board coor

--Segun el tipo de robot, devuelve la mejor accion a realizar en el momento
bestAccion :: Int -> [[Cell]] -> (Int, Int) -> Accion
bestAccion 0 board coor = accion
  where
    accions = possiblesAccions board coor
    dirts = getAllElements Dirt board
    accion
      -- Esta encima de una basura, La limpia
      | Clean `elem` accions = Clean
      -- No quedan basuras por limpiar, No hace nada
      | null dirts = Pass
      -- Se mueve hacia la basura mas cercana
      | Move `elem` accions = Move
      -- No hace nada
      | otherwise = Pass
bestAccion 1 board coor = accion
  where
    accions = possiblesAccions board coor
    childs = getAllElements Child board
    dirts = getAllElements Dirt board
    accion
      --Esta parado en una casilla vacia del corral con un niño cargado, Lo suelta
      | Drop `elem` accions = Drop
      --No quedan niños fuera del corral y esta parado en una casilla con basura, La limpia
      | null childs && Clean `elem` accions = Clean
      --No quedan niños fuera del corral, Se mueve hacia la basura mas cercana
      | null childs && not (null dirts) = Move
      --No quedan niños fuera del corral ni basuras, No hace nada
      | null dirts = Pass
      --Se mueve hacia el niño mas cercano
      | Move `elem` accions = Move
      | otherwise = Pass

--Simula ana accion realizada por el robot
simulateAccion :: Int -> [[Cell]] -> Accion -> (Int, Int) -> [[Cell]]
simulateAccion botType board accion coor = b
  where
    bot = getBot board coor
    b
      | accion == Pass = board
      | accion == Drop = dropChild coor board
      | accion == Clean = clean board coor
      | accion == Move =
        let best = bestMove botType board coor
            tmp = (fst best - fst coor, snd best - snd coor)
            b1
              | coor == best = board
              | otherwise = move bot board coor (tupleToDir tmp)
         in b1

--Simula el movimiento de un robot
simulateRobot :: Int -> [[Cell]] -> (Int, Int) -> [[Cell]]
simulateRobot botType board coor = simulateAccion botType board (bestAccion botType board coor) coor

--Simula el movimiento de todos los robots
simulateRobots :: Int -> [[Cell]] -> [(Int, Int)] -> [[Cell]]
simulateRobots botType board [] = board
simulateRobots botType board (x : xs) = simulateRobots botType (simulateRobot botType board x) xs
