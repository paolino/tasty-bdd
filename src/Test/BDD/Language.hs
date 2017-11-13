{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.BDD.Language where
import           Control.Concurrent
-- import           Control.Monad.Reader
-- import           Control.Monad.Writer
import           Control.Arrow
import           Control.Monad        hiding (when)
import           Data.Tagged
import           Data.Typeable
import           Lens.Micro
import           Lens.Micro.TH
import qualified Test.HUnit.Lang      as H
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Providers

data Phase = Preparing | Testing

data Language m t q a where

    Given   :: m ()
            -> Language m t q Preparing
            -> Language m t q Preparing
    GivenAndAfter   ::  m ()
                    ->  m ()
                    -> Language m t q Preparing
                    -> Language m t q Preparing
    When    :: String
            -> m t
            -> Language m t q Testing
            -> Language m t q Preparing
    Then   :: String
            -> (t -> m q)
            -> Language m t q Testing
            -> Language m t q Testing

    End    :: Language m t q Testing



data BDDTest m t = BDDTest
            { _gname    :: String
            , _tests    :: [(String,t -> m ())]
            , _teardown :: m ()
            , _rigup    :: m ()
            , _when     :: m t
            }

makeLenses ''BDDTest

type BDD m t = Language m t () Preparing

makeBDD :: Monad m => BDD m t -> BDDTest m t
makeBDD (Given fa p)
        = over rigup (fa >>) $ makeBDD p
makeBDD (GivenAndAfter fa fb  p)
        = over rigup (fa >>) . over teardown (>> fb) $ makeBDD p
makeBDD (When s fa p)
        =  set when fa $ set gname s $ bddT p

bddT :: Monad m => Language m t () Testing  -> BDDTest m t
bddT (Then s ca p) = over tests ((:) (s,ca)) $ bddT p
bddT End           = BDDTest "" [] (return ()) (return ()) (return undefined)

instance Typeable t => IsTest (BDDTest IO t) where
    run _ (BDDTest name ts td rup w) f = do
            rup
            t <- w
            let g [] = return $ testPassed "OK"
                g ((n,a):ts) = do
                    f $ Progress n 0
                    r <- H.performTestCase $ a t
                    case r of
                            H.Success     -> g ts
                            H.Failure _ s -> return $ testFailed s
                            H.Error _ s   -> return $ testFailed s
            r <- g ts
            td
            return r
    testOptions = Tagged []

testTreeBDD :: (Monad m, IsTest (BDDTest m t)) => BDD m t  -> TestTree
testTreeBDD t = singleTest (at ^. gname) at where at = makeBDD t


