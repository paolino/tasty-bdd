{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}


module Test.BDD.Language where

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

data GivenWithTeardown m = forall r. GivenWithTeardown (m r) (r -> m ())

data Language m t q a where

    Given   :: m ()
            -> Language m t q Preparing
            -> Language m t q Preparing
    GivenAndAfter   :: m r
                    -> (r -> m ())
                    -> Language m t q Preparing
                    -> Language m t q Preparing
    When    :: m t
            -> Language m t q Testing
            -> Language m t q Preparing
    Then   :: (t -> m q)
           -> Language m t q Testing
           -> Language m t q Testing

    End    :: Language m t q Testing

data BDDTest m t r = BDDTest
            { _tests :: [t -> m ()]
            , _rigup  :: [GivenWithTeardown m]
            , _when   :: m t
            }

makeLenses ''BDDTest

type BDD m t = Language m t () Preparing

makeBDD :: Monad m => r -> BDD m t -> BDDTest m t r
makeBDD res0 (Given fa p)
        =  makeBDD res0 $ GivenAndAfter fa (const $ return ()) p
makeBDD res0 (GivenAndAfter given after p)
        = over rigup (\oldRig -> (GivenWithTeardown given after) : oldRig)
        $ makeBDD res0 p
makeBDD res0 (When fa p)
        =  set when fa $ bddT res0 p

bddT :: Monad m => r -> Language m t () Testing  -> BDDTest m t r
bddT r (Then ca p) = over tests ((:) ca) $ bddT r p
bddT res0 End         = BDDTest [] [] (return undefined)
