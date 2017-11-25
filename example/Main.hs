{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Concurrent              (threadDelay)
import Data.Aeson
import Data.Aeson.QQ
import Test.Tasty                      (TestTree, defaultMainWithIngredients,
                                        testGroup)
import Test.Tasty.Bdd
import Test.Tasty.Ingredients          (Ingredient)
import Test.Tasty.Ingredients.Basic    (consoleTestReporter, listingTests)
import Test.Tasty.Ingredients.FailFast (failFast)


t1 :: TestTree
t1 = testBehavior "Test sequence"
    $ Given (putStrLn "\nFirst effect")
    $ Given (putStrLn "Another effect")
    $ GivenAndAfter (putStrLn "Aquiring resource" >> return "Resource 1")
                    (putStrLn . ("Release "++))
    $ GivenAndAfter (putStrLn "Aquiring resource" >> return "Resource 2")
                    (putStrLn . ("Release "++))
    $ When (putStrLn "Action returning" >> return ([1..10]++[100..106]) :: IO [Int])
    $ Then (@?= ([1..10]++[700..706]))
    $ End


t2 :: TestTree
t2 = testBehavior "Exceedingly long running test"
    $ GivenAndAfter (return 100000) threadDelay
    $ When (return "Effect" :: IO String)
    $ Then (\w -> length w @?= 6 >> threadDelay 1000000)
    $ Then (\w -> tail w @?= "ffect" >> threadDelay 1000000)
    $ Then (\w -> init w @?= "Effec" >> threadDelay 1000000)
    $ End


val :: Value
val = object [
      "boolean" .= True,
        "numbers" .= [1,2,3::Int] ]


val2 :: Value
val2 = object [
      "boolean" .= True,
        "numbers" .= [1,4,3::Int] ]


t3 :: TestTree
t3 = testBehavior "json small"
    $ When (return val :: IO Value)
    $ Then (\w -> w @?= val)
    $ Then (\w -> w @?= val2)
    $ End


valbig :: Value
valbig = [aesonQQ|
                 [
                     {
                         "type":"OrderOveruseRow",
                         "customer_no":220011,
                         "order_no":"10/10",
                         "units":"GB",
                         "line_numbers":"LN55",
                         "order_start":1448924400,
                         "order_end":1451602799,
                         "billing_method":"Volume Outgoing",
                         "ordered_amount":100,
                         "actual_usage":"1.00",
                         "overused_amount":"0.00",
                         "customer_name":"CustomerNameGmbh",
                         "position_text":""
                    }
                    ]
            |]


valbig2 :: Value
valbig2 = [aesonQQ|
                 [
                     {
                         "type":"OrderOveruseRow",
                         "customer_no":220011,
                         "order_no":"10/10",
                         "units":"GB",
                         "line_numbers":"LN55",
                         "order_start":1448924400,
                         "order_end":1451602799,
                         "billing_method":"Volume Outgoing",
                         "ordered_amount":100,
                         "actual_usage":"1.20",
                         "overused_amount":"0.00",
                         "customer_name":"CustomerTameGmbh",
                         "position_text":""
                    }
                    ]
            |]


t4 :: TestTree
t4 = testBehavior "json big value"
    $ When (return valbig :: IO Value)
    $ Then (\w -> w @?= valbig)
    $ Then (\w -> w @?= valbig2)
    $ End


main :: IO ()
main = defaultMainWithIngredients ingredients
    $ testGroup "All the tests" [t1, t2,t3,t4]


ingredients :: [Ingredient]
ingredients = [listingTests, failFast consoleTestReporter]

