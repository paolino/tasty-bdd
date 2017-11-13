module Main where

import Control.Concurrent
import Test.Tasty
import Test.Tasty.Bdd
import Test.Tasty.Ingredients          (Ingredient)
import Test.Tasty.Ingredients.FailFast
import Test.Tasty.Ingredients.Basic

t1 :: TestTree
t1 = testBdd "Test sequence" (
      Given (print "Some effect")
    . Given (print "Another effect")
    . GivenAndAfter (print "Aquiring resource" >> return "Resource 1")
                    (print . ("Release "++))
    . GivenAndAfter (print "Aquiring resource" >> return "Resource 2")
                    (print . ("Release "++))
    . When (print "Action returning" >> return ([1..10]++[100..106]) :: IO [Int])
    . Then (@?= ([1..10]++[700..706]))
    )

t2 :: TestTree
t2 = testBdd "A long running test" (
      GivenAndAfter (return 1000000) threadDelay
    . When (return "Effect" :: IO String)
    . Then (@?= "Effect")
    )

main :: IO ()
main = defaultMainWithIngredients ingredients $ testGroup "All the tests" [t1, t2]

ingredients :: [Ingredient]
ingredients = [listingTests, failFast consoleTestReporter]

