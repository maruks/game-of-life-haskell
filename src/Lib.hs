module Lib
    ( Grid(..), nextGrid
    ) where

import qualified Data.Map.Strict as Map

data Grid = Grid {width :: Int,
                  height :: Int,
                  cells :: Map.Map (Int, Int) Bool} deriving Show


nextGrid:: Grid -> Grid
nextGrid = id
