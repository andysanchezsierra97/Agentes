module Simulacion
  ( simulation,
  )
where

import Bots
import Childs
import Environment
import System.IO.Unsafe
import System.Random
import Types
import Utils

simulate :: StdGen -> Int -> [[Cell]] -> Int -> Int -> ([[Cell]], StdGen)
simulate gen botType board t 0 = (board, gen)
simulate gen botType board t k = simulate gc botType bb t (k -1)
  where
    m = mod k t
    --Cada t, Cambia el Ambiente
    (b, g) = if m == 0 then changeEnvironment gen board else (board, gen)
    --Simula los moviminetos de los niños
    (bc, gc) = simulateChilds g b (getAllElements Child board)
    --Simula las acciones del robot
    bb = simulateRobots botType bc (getAllBot board)


simulation :: IO ()
simulation =
  let 
      n = 20
      m = 20
      t = 10
      s = 500

      (b, g) = initEnvironment n m
      (b1, g1) = simulate g 0 b t s
      (b2, g2) = simulate g 1 b t s
      
      p1 = getPercent b1
      p2 = getPercent b2

   in do
        printEnvironment b (n-1)
        printEnvironment b1 (n-1)
        printEnvironment b2 (n-1)
        print p1
        print p2
