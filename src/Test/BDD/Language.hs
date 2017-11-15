{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}


module Test.BDD.Language (
    Language(..)
    , BDDPreparing
    , BDDTesting
    , BDDTest(..)
    , TestContext(..)
    , context
    , tests
    , when
    , Interpret (..)
    , Phase (..)
    ) where

import           Lens.Micro
import           Lens.Micro.TH

-- | Separating the 2 phases by type
data Phase = Preparing | Testing

-- | Recording given actions and type related teardowns
data TestContext m = forall r. TestContext (m r) (r -> m ())

-- | Bare hoare language
data Language m t q a where
    -- ^ action to prepare the test
    Given           :: m ()
                    -> Language m t q 'Preparing
                    -> Language m t q 'Preparing

    -- ^ action to prepare the test, and related teardown action
    GivenAndAfter   :: m r
                    -> (r -> m ())
                    -> Language m t q 'Preparing
                    -> Language m t q 'Preparing

    -- ^ core logic of the test (last preparing action)
    When            :: m t
                    -> Language m t q 'Testing
                    -> Language m t q 'Preparing

    -- ^ action producing a test
    Then            :: (t -> m q)
                    -> Language m t q 'Testing
                    -> Language m t q 'Testing

    -- ^ final placeholder
    End             :: Language m t q 'Testing


-- | Result of this module interpreter
data BDDTest m t q = BDDTest
            { _tests   :: [t -> m q] -- ^ tests from 't'
            , _context :: [TestContext m] -- ^ test context
            , _when    :: m t -- ^ when action to compute 't'
            }

makeLenses ''BDDTest

-- | Preparing language types
type BDDPreparing m t q = Language m t q 'Preparing

-- | Testing language types
type BDDTesting m t q = Language m t q 'Testing

-- | An interpreter collecting the actions
class Interpret m t q a where
    interpret :: Monad m => Language m t q a -> BDDTest m t q

instance Interpret m t q Preparing where
    interpret  (Given given p)
            =  interpret $ GivenAndAfter given (const $ return ()) p
    interpret (GivenAndAfter given after p)
            = over context ((:) $ TestContext given after)
            $ interpret p
    interpret (When fa p)
            = set when fa $ interpret p


instance Interpret m t q Testing where
    interpret (Then ca p) = over tests ((:) ca) $ interpret p
    interpret End         = BDDTest [] []
        $ error "End on its own does not make sense as a test"
