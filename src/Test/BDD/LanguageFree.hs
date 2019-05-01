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
        , then__
        , when_
        , GivenFree
        , ThenFree
        , FreeBDD
        , testFreeBDD
        , BDDResult (..)
--        , testBehaviorFree
--
--
    )
    where

import Control.Monad.Free
-- import Control.Exception
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Reader


-- | Separating the 2 phases by type
data Phase t = Preparing | Testing t



-- | Bare hoare language
data Language m a where
    -- | action to prepare the test
    Given           :: m a
                    -> (a -> Language m 'Preparing)
                    -> Language m  'Preparing
    -- | action to prepare the test, and related teardown action
    GivenAndAfter   :: m (a,r)
                    -> (r -> m ())
                    -> (a -> Language m  'Preparing)
                    -> Language m 'Preparing
    -- | core logic of the test (last preparing action)
    When            :: m t
                    -> Language m  ('Testing t)
                    -> Language m  'Preparing
    -- | action producing a test
    Then            :: (t -> m ())
                    -> Language m ('Testing t)
                    -> Language m ('Testing t)
    -- | final placeholder
    End             :: Language m ('Testing t)




data BDDResult m = Failed SomeException (m ()) | Succeded (m ())



type CJR m = ReaderT (m ()) m (BDDResult m)

stepIn :: MonadCatch m => m a -> (a -> CJR m ) -> CJR m
stepIn g q = catch (lift g >>= q) $ asks . Failed

interpret :: forall m . MonadCatch m => Language m  'Preparing -> m (BDDResult m)
interpret  y = runReaderT (interpret' y) (return  ()) where
    interpret' :: Language m 'Preparing -> CJR m
    interpret' (Given g p) = stepIn g $ interpret' . p
    interpret' (GivenAndAfter g z p) =
                     stepIn g $ \(x,r) -> local (z r >>) $ interpret' $ p x
    interpret' (When fa p) =
                     stepIn fa $ \x -> interpretT'  x p
    interpretT' :: t -> Language m ('Testing t) -> CJR m
    interpretT' _ End = asks Succeded
    interpretT' x (Then f p) =
                     stepIn (f x) $ \() -> interpretT'  x p


data GivenFree m  a where
    GivenFree :: m b -> (b -> a) -> GivenFree m  a
    GivenAndAfterFree :: m (b,r) -> (r -> m ()) -> (b -> a) -> GivenFree m a
    WhenFree :: m t -> Free (ThenFree m t) c -> a -> GivenFree m  a

data ThenFree m t a
    = ThenFree (t -> m ()) a
    deriving Functor

instance Functor (GivenFree m) where
    fmap f (GivenFree m x)             = GivenFree m $ f <$> x
    fmap f (GivenAndAfterFree mr rm x) = GivenAndAfterFree mr rm $ f <$> x
    fmap f (WhenFree mt ft x)          = WhenFree mt ft $ f x

type FreeBDD m x = Free (GivenFree m) x

given :: m a -> Free (GivenFree m) a
given m = liftF $ GivenFree m id

givenAndAfter :: m (b,r) -> (r -> m ()) -> Free (GivenFree m) b
givenAndAfter g td = liftF $ GivenAndAfterFree g td id

givenAndAfter_ :: Functor m =>  m r -> (r -> m ()) -> Free (GivenFree m) ()
givenAndAfter_ g td = liftF $ GivenAndAfterFree (((),) <$>  g) td id

when_ :: m t -> Free (ThenFree m t) b -> Free (GivenFree m) ()
when_ mt ts = liftF $ WhenFree mt ts ()

thens :: Free (ThenFree m t) a -> Language m ('Testing t)
thens (Free (ThenFree m f)) = Then m $ thens f
thens (Pure _)              = End

bddFree :: Free (GivenFree m) x -> Language m  'Preparing
bddFree (Free (GivenFree m f)) = Given m $ bddFree <$> f
bddFree (Free (GivenAndAfterFree mr rm f)) =
    GivenAndAfter mr rm $ bddFree <$> f
bddFree (Free (WhenFree mt ts _)) = When mt $ thens ts
bddFree (Pure _                 ) = error "empty tests not allowed"


then_ :: (t -> m ()) -> Free (ThenFree m t) ()
then_ m = liftF $ ThenFree m ()

then__ :: m () -> Free (ThenFree m t) ()
then__  = then_ . const

testFreeBDD :: (MonadCatch m)
          => Free (GivenFree m) x
          -> m (BDDResult m)
testFreeBDD = interpret . bddFree

