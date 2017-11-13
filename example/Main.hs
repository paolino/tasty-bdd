module Main where

import Control.Concurrent              (threadDelay)
import Test.Tasty                      (TestTree, defaultMainWithIngredients,
                                        testGroup)
import Test.Tasty.Bdd
import Test.Tasty.Ingredients          (Ingredient)
import Test.Tasty.Ingredients.Basic    (consoleTestReporter, listingTests)
import Test.Tasty.Ingredients.FailFast (failFast)

t1 :: TestTree
t1 = testBdd "Test sequence" $
      Given (putStrLn "\nFirst effect")
    . Given (putStrLn "Another effect")
    . GivenAndAfter (putStrLn "Aquiring resource" >> return "Resource 1")
                    (putStrLn . ("Release "++))
    . GivenAndAfter (putStrLn "Aquiring resource" >> return "Resource 2")
                    (putStrLn . ("Release "++))
    . When (putStrLn "Action returning" >> return ([1..10]++[100..106]) :: IO [Int])
    . Then (@?= ([1..10]++[700..706]))

t2 :: TestTree
t2 = testBdd "Exceedingly long running test" $
      GivenAndAfter (return 100000) threadDelay
    . When (return "Effect" :: IO String)
    . Then (\w -> length w @?= 6 >> threadDelay 1000000)
    . Then (\w -> tail w @?= "ffect" >> threadDelay 1000000)
    . Then (\w -> init w @?= "Effec" >> threadDelay 1000000)

main :: IO ()
main = defaultMainWithIngredients ingredients $ testGroup "All the tests" [t1, t2]

ingredients :: [Ingredient]
ingredients = [listingTests, failFast consoleTestReporter]

