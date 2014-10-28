import Test.Tasty
import Test.Tasty.Runners.Html

main = defaultMainWithIngredients (htmlRunner:defaultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" []
