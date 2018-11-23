module Lib
  ( Grid(..)
  , nextGrid
  , numberOfNeighbours
  , allEmptyNeighbours
  ) where

import qualified Data.Map.Strict as Map
import Data.List (sort,nub)

type Point = (Int, Int)

data Grid = Grid
  { width :: Int
  , height :: Int
  , cells :: Map.Map Point Int
  } deriving (Show, Eq)

-- Any live cell with fewer than two live neighbors dies, as if by underpopulation.
-- Any live cell with two live neighbors lives on to the next generation.
-- Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
-- Any live cell with more than three live neighbors dies, as if by overpopulation.

isAlive :: Bool -> Int -> Bool
isAlive current neighbours
  | neighbours == 2 = current
  | otherwise = neighbours == 3

nextGrid :: Grid -> Grid
nextGrid grid@(Grid w h cells) = Grid w h newCells
  where
    newCells =
      Map.fromList $
      map
        (\p -> (p, 1))
        [ (x, y)
        | x <- [0 .. w]
        , y <- [0 .. h]
        , isAlive (Map.member (x, y) cells) $ numberOfNeighbours cells (x,y) ]

neighbours :: Map.Map Point Int -> Point -> [Point]
neighbours cells (xp, yp) =
  [ (xp + x, yp + y)
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , Map.member (xp + x, yp + y) cells
  , not (x == 0 && y == 0) ]

emptyNeighbours :: Grid -> Point -> [Point]
emptyNeighbours grid@(Grid w h cells) (xp, yp) =
  [ (xp + x, yp + y)
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , not $ Map.member (xp + x, yp + y) cells
  , x > -1 && y > -1 && x < w && y < h ]

allEmptyNeighbours :: Grid -> [Point]
allEmptyNeighbours grid@(Grid _ _ cells) = nub . sort $ concatMap (emptyNeighbours grid) $ Map.keys cells

numberOfNeighbours :: Map.Map Point Int -> Point -> Int
numberOfNeighbours cells = length . neighbours cells
