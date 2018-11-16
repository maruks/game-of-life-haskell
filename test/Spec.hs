import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "game of life" $ do
          it "bla bla" $ do
            2 `shouldBe` 2
