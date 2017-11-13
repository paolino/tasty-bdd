{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Concurrent
import Test.Tasty
import Test.Tasty.Bdd
import Test.Tasty.Ingredients.FailFast
import Test.Tasty.Ingredients.Basic

t1' = (
      Given (print "Given 1")
    . Given (print "Given 2")
    . GivenAndAfter (print "Given before After 3" >> return "After After 3") (print)
    . GivenAndAfter (print "Given before After 4" >> return "After After 4") (print)
    . When (print "when ptek" >> return ([1..5]++[500..506]) :: IO [Int])
    . Then (@?= ([1..5]++[600..606]))
    . Then (\r -> length r @?= 4)
    . Then (\_ -> print "Then 5")
    )

t2 :: TestTree
t2 = testBdd "name is Paolino" (
      GivenAndAfter (print "G 01") (const (threadDelay 1000000 >> print "A 01"))
    . When (print "when Paolino" >> return "Paolino" :: IO String)
    )

t1 = testBdd "name is ptek" t1'

main = do
  defaultMainWithIngredients ingredients $ testGroup "All the tests" [t1, t2]

ingredients = [listingTests, failFast $ consoleTestReporter]

