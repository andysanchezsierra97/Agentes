{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types
  ( Accion (..),
    Cell (..),
    Direction (..),
    cellToString,
    cellType,
  )
where

data Accion = Move | Drop | Clean | Pass deriving (Eq, Show)

data Cell
  = Obstacle
  | Child
  | Dirt
  | Playpen {cells :: [Cell]}
  | Bot {cells :: [Cell]}
  | Empty
  deriving (Show, Eq)

data Direction = North | South | East | West deriving (Show, Eq)

cellToString :: Cell -> String
cellToString Empty = "_"
cellToString Child = "C"
cellToString Obstacle = "O"
cellToString Dirt = "D"
cellToString Bot {cells = []} = "B"
cellToString Bot {cells = [Child]} = "B[C]"
cellToString Bot {cells = [Dirt]} = "B[D]"
cellToString Bot {cells = [_, _]} = "B[CD]"
cellToString Playpen {cells = [Child]} = "P[C]"
cellToString Playpen {cells = []} = "P"
cellToString Playpen {cells = [Bot {cells = []}]} = "P[B]"
cellToString Playpen {cells = [Bot {cells = [Child]}]} = "P[B[C]]"
cellToString Playpen {cells = [Bot {cells = []}, Child]} = "P[B,C]"

cellType :: Cell -> String
cellType Empty = "Empty"
cellType Child = "Child"
cellType Obstacle = "Obstacle"
cellType Dirt = "Dirt"
cellType Bot {cells = _} = "Bot"
cellType Playpen {cells = _} = "Playpent"