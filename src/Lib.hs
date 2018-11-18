module Lib
  ( Grid(..)
  , nextGrid
  , numberOfNeighbours
  ) where

import qualified Data.Map.Strict as Map

type Point = (Int, Int)

data Grid = Grid
  { width :: Int
  , height :: Int
  , cells :: Map.Map Point Int
  } deriving (Show, Eq)

isAlive :: Bool -> Int -> Bool
isAlive current neighbours
  | neighbours < 2 = False
  | neighbours > 3 = False
  | neighbours == 3 && not current = True
  | otherwise = current

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
        , isAlive (Map.member (x, y) cells) $ numberOfNeighbours grid x y ]

neighbours :: Grid -> Int -> Int -> [Point]
neighbours (Grid _ _ cells) xp yp =
  [ (x, y)
  | x <- [-1 .. 1]
  , y <- [-1 .. 1]
  , Map.member (xp + x, yp + y) cells
  , not (x == 0 && y == 0) ]

numberOfNeighbours :: Grid -> Int -> Int -> Int
numberOfNeighbours g x y = length $ neighbours g x y
