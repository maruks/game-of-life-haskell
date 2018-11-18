import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck

import Lib

import qualified Data.Map.Strict as Map

main :: IO ()
main =
  hspecWith
    defaultConfig
    { configFastFail = True
    }
    specs

specs :: Spec
specs =
  describe "game of life" $
  do it "next grid returns grid" $
       do let g = Grid 2 2 Map.empty
          nextGrid g `shouldBe` g
     it "returns the number of neighbours" $
       do let grid = Grid 2 2 $ Map.fromList [((0, 0), 1), ((0, 1), 1), ((1, 1), 1)]
          numberOfNeighbours grid 0 0 `shouldBe` 2
     it "should cause a cell to die if it has less than 2 neighbours" $
       do let grid = Grid 2 2 $ Map.fromList [((0, 1), 1), ((1, 1), 1)]
          cells (nextGrid grid) `shouldBe` Map.empty
