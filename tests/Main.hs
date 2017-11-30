{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.QQ
import           Data.Char
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Test.HUnit                      as H
import           Test.Tasty
import           Test.Tasty.Bdd
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients          (Ingredient)
import           Test.Tasty.Ingredients.Basic    (consoleTestReporter,
                                                  listingTests)
import           Test.Tasty.Ingredients.FailFast (failFast)
import           Test.Tasty.Providers
import           Text.InterpolatedString.QM

import           CaptureStdout

ingredients :: [Ingredient]
ingredients = [listingTests, failFast consoleTestReporter]

testTest :: (Show a, Eq a) => ((a -> IO ()) -> IO ()) -> [a] -> IO ()
testTest t r' = do
    l <- newMVar []
    let app x = modifyMVar_ l (return . (x :))
    handle (\(_ :: SomeException) -> return "")
        $ captureStdout "tasty-bdd-test-suite"
        $ t app
    r <- readMVar l
    r H.@?= reverse r'

t1 :: TestTree
t1 = testCase "givens and givenandafter order is respected" $ do
    let t app = defaultMain
            $ testBehavior "Test sequence"
            $ Given (app "First effect")
            $ Given (app "Another effect")
            $ GivenAndAfter (app "Aquiring resource" >> return "Resource 1")
                            (app . ("Release "++))
            $ GivenAndAfter (app "Aquiring resource" >> return "Resource 2")
                            (app . ("Release "++))
            $ When (app "Action returning" >> return ())
            $ Then (\_ -> return ())
            $ End
    testTest t $ [
                "First effect",
                "Another effect",
                "Aquiring resource",
                "Aquiring resource",
                "Action returning",
                "Release Resource 2",
                "Release Resource 1"
                ]

t2 :: TestTree
t2 = testCase "recursive before decorations are honored" $ do
    let t app = defaultMain
            $ beforeEach (app 0)
            $ testGroup "g1"
                [ testCase "t1" $ app 1
                , testCase "t2" $ app 2
                , testGroup "g2"
                    [ testCase "t3" $ app 3]
                , testCase "t4" $ app 4
                ]
    testTest t [0,1,0,2,0,3,0,4]


t3 :: TestTree
t3 = testCase "recursive after decorations are honored" $ do
    let t app = defaultMain
            $ afterEach (app 0)
            $ testGroup "g1"
                [ testCase "t1" $ app 1
                , testCase "t2" $ app 2
                , testGroup "g2"
                    [ testCase "t3" $ app 3]
                , testCase "t4" $ app 4
                ]
    testTest t [1,0,2,0,3,0,4,0]

main = defaultMain $ testGroup "All" [t1, t2, t3]









{-
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




-}
