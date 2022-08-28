module Main where

import Pizza.Lib.Data.Vector.Generic.SizedTests
import Test.Tasty
import Test.Tasty.Ingredients
import qualified Test.Tasty.Ingredients.ConsoleReporter as ConsoleReporter
import qualified Test.Tasty.Runners.Reporter as Reporter

main :: IO ()
main = defaultMainWithIngredients [composeReporters Reporter.ingredient ConsoleReporter.consoleTestReporter] tests

tests :: TestTree
tests =
  testGroup
    "pizza-vector-sized"
    [ testGroup
        "Pizza.Lib.Data.Vector.Generic"
        [sizedTests]
    ]
