{-# LANGUAGE GADTs             #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import Test.BDD.Language



t1 :: BDD IO String 
t1  = Given  (return ())
    $ GivenAndAfter (return ()) (return ())
    $ When "test ptek is THE name" (return "ptek")
    $ Then "name is correct" (@?= "ptek")
    $ Then "name is 4 char long" (\z -> length z @?= 5)
    $ End


main = defaultMain $ testTreeBDD t1

