import Test.Tasty
import Test.Tasty.Runners.Html
import qualified Test.Tasty.QuickCheck as QC

import Regensburg

main = defaultMainWithIngredients (htmlRunner:defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests"
  [ localOption (QC.QuickCheckMaxSize 10) $
      QC.testProperty "append für Listen ist assoziativ"
      (prop_valid_semigroup :: [Int] -> [Int] -> [Int] -> Bool)
  ]
