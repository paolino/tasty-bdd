{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.MVar
import           Control.Exception
import qualified Test.HUnit              as H
import           Test.Tasty
import           Test.Tasty.Bdd
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All"
    [
    testCase "givens and givenandafter order is respected" $ do
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
        testTest t [ "First effect"
                   , "Another effect"
                   , "Aquiring resource"
                   , "Aquiring resource"
                   , "Action returning"
                   , "Release Resource 2"
                   , "Release Resource 1"
                   ]

    , testCase "recursive before decorations are honored" $ do
        let t app = defaultMain
                $ beforeEach (app 0)
                $ testGroup "g1"
                    [ testCase "t1" $ app 1
                    , testCase "t2" $ app 2
                    , testGroup "g2" [ testCase "t3" $ app 3]
                    , testCase "t4" $ app 4
                    ]
        testTest t ([0,1,0,2,0,3,0,4] :: [Int])

    , testCase "recursive after decorations are honored" $ do
        let t app = defaultMain
                $ afterEach (app 0)
                $ testGroup "g1"
                    [ testCase "t1" $ app 1
                    , testCase "t2" $ app 2
                    , testGroup "g2" [ testCase "t3" $ app 3]
                    , testCase "t4" $ app 4
                    ]
        testTest t ([1,0,2,0,3,0,4,0] :: [Int])
    ]


testTest :: (Show a, Eq a) => ((a -> IO ()) -> IO ()) -> [a] -> IO ()
testTest t r' = do
    l <- newMVar []
    let app x = modifyMVar_ l (return . (x :))
    _ <- handle (\(_ :: SomeException) -> return "")
        $ captureStdout "tasty-bdd-test-suite"
        $ t app
    r <- readMVar l
    r H.@?= reverse r'
