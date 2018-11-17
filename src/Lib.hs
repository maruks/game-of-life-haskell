module Lib
    ( Grid(..), nextGrid, numberOfNeighbours
    ) where

import qualified Data.Map.Strict as Map

data Grid = Grid{width :: Int,
                  height :: Int,
                  cells :: Map.Map (Int, Int) Bool} deriving (Show, Eq)

isAlive :: Bool -> Int -> Bool
isAlive i x
  | x < 2 = False
  | x > 3 = False
  | x == 3 && not i = True
  | otherwise = i

nextGrid:: Grid -> Grid
nextGrid (Grid w h cells) = Grid w h newCells
  where newCells = Map.fromList $ map (\p -> (p,True))  [ (x,y) | x <- [0..w], y <- [0..h], isAlive (Map.member (x,y) cells) $ numberOfNeighbours (Grid w h cells) x y]

numberOfNeighbours :: Grid -> Int -> Int -> Int
numberOfNeighbours (Grid _ _ cells) xp yp =
  let ps = [ (x,y) | x <- [-1..1], y <- [-1..1], Map.member (xp+x,yp+y) cells, not (x==0 && y==0)] in length ps
