-------------------------------------------------------------------------------
-- | Free monads to introduce do notation in ''Language''
--
-- Module    :
-- Copyright :  (c) Paolo Veronelli 2017
-- License   :  All rights reserved
-- Maintainer:  paolo.veronelli@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--
--
-------------------------------------------------------------------------------

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Test.BDD.LanguageFree
    (
        given
        , givenAndAfter_
        , givenAndAfter
        , then_
        , when_
        , GivenFree
        , ThenFree
        , mkFreeBDD
        , FreeBDD (..)
        , JumpOut (..)
--        , testBehaviorFree
--
--
    )
    where

import Control.Monad.Free
-- import Control.Exception
import Control.Monad.Catch
import Data.Typeable
import Test.Tasty.Providers


-- | Separating the 2 phases by type
data Phase = Preparing | Testing



-- | Bare hoare language
data Language m t a where
    -- | action to prepare the test
    Given           :: m a
                    -> (a -> Language m t 'Preparing)
                    -> Language m t 'Preparing
    -- | action to prepare the test, and related teardown action
    GivenAndAfter   :: m (a,r)
                    -> (r -> m ())
                    -> (a -> Language m t 'Preparing)
                    -> Language m t 'Preparing
    -- | core logic of the test (last preparing action)
    When            :: m t
                    -> Language m t 'Testing
                    -> Language m t 'Preparing
    -- | action producing a test
    Then            :: (t -> m ())
                    -> Language m t 'Testing
                    -> Language m t 'Testing
    -- | final placeholder
    End             :: Language m t 'Testing




data JumpOut m = JumpOut SomeException (m ())

instance Show (JumpOut m) where
    show (JumpOut e _) ="JumpOut: " <> show e
instance (Typeable m) => Exception (JumpOut m)

rethrow :: (MonadCatch m1, Typeable m2) => m2 () -> m1 a -> m1 a
rethrow td f = f `catch` (\e -> throwM $ JumpOut e td)

-- | An interpreter collecting the actions
interpret :: (Monad m, MonadCatch m, Typeable m)
          => m ()
          -> Language m t 'Preparing
          -> m (m ())
interpret td (Given g p) = do
    x <- rethrow td  g
    interpret td $ p x
interpret td (GivenAndAfter g after p) = do
    (x, r) <- rethrow td  g
    interpret (after r >> td) $ p x
interpret td (When fa p) = do
    x <- rethrow td fa
    interpretT td x p

interpretT :: (Monad m, MonadCatch m, Typeable m)
           => m ()
           -> t
           -> Language m t 'Testing
           -> m (m ())
interpretT td _ End        = return td
interpretT td x (Then f p) = do
    rethrow td $ f x
    interpretT td x p



data GivenFree m t a where
    GivenFree :: m b -> (b -> a) -> GivenFree m t a
    GivenAndAfterFree :: m (b,r) -> (r -> m ()) -> (b -> a) -> GivenFree m t a
    WhenFree :: m t -> Free (ThenFree m t) c -> a -> GivenFree m t a

data ThenFree m t a
    = ThenFree (t -> m ()) a
    deriving Functor

instance Functor (GivenFree m t) where
    fmap f (GivenFree m x)             = GivenFree m $ f <$> x
    fmap f (GivenAndAfterFree mr rm x) = GivenAndAfterFree mr rm $ f <$> x
    fmap f (WhenFree mt ft x)          = WhenFree mt ft $ f x

given :: m a -> Free (GivenFree m t) a
given m = liftF $ GivenFree m id

givenAndAfter :: m (b,r) -> (r -> m ()) -> Free (GivenFree m t) b
givenAndAfter g td = liftF $ GivenAndAfterFree g td id

givenAndAfter_ :: Functor m =>  m r -> (r -> m ()) -> Free (GivenFree m t) ()
givenAndAfter_ g td = liftF $ GivenAndAfterFree (((),) <$>  g) td id

when_ :: m t -> Free (ThenFree m t) b -> Free (GivenFree m t) ()
when_ mt ts = liftF $ WhenFree mt ts ()

thens :: Free (ThenFree m t) a -> Language m t 'Testing
thens (Free (ThenFree m f)) = Then m $ thens f
thens (Pure _)              = End

bddFree :: Free (GivenFree m t) x -> Language m t 'Preparing
bddFree (Free (GivenFree m f)) = Given m $ bddFree <$> f
bddFree (Free (GivenAndAfterFree mr rm f)) =
    GivenAndAfter mr rm $ bddFree <$> f
bddFree (Free (WhenFree mt ts _)) = When mt $ thens ts
bddFree (Pure _                 ) = error "empty tests not allowed"


then_ :: (t -> m ()) -> Free (ThenFree m t) ()
then_ m = liftF $ ThenFree m ()

mkFreeBDD :: (IsTest (FreeBDD m), Monad m, Typeable m, MonadCatch m)
          => String
          -> Free (GivenFree m t) x
          -> TestTree
mkFreeBDD s = singleTest s . FreeBDD . interpret (return ()) . bddFree

newtype FreeBDD m = FreeBDD (m (m ()))
