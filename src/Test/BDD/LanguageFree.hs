{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Test.BDD.LanguageFree (
        given_
        , givenAndAfter_
        , then_
        , when_
        , GivenFree
        , ThenFree
        , BddFree
        , bddFree
    ) where

import Control.Monad.Free
import Test.BDD.Language  (BDDPreparing, BDDTesting,
                           Language(End, Given, GivenAndAfter, Then, When))
import Test.Tasty
import Test.Tasty.Bdd
data GivenFree m t q a where
    GivenFree :: m () -> a -> GivenFree m t q a
    GivenAndAfterFree :: m r -> (r -> m ()) -> a -> GivenFree m t q a
    WhenFree :: m t -> Free (ThenFree m t q) b -> a -> GivenFree m t q a

instance Functor (GivenFree m t q) where
    fmap f (GivenFree m x)             = GivenFree m $ f x
    fmap f (GivenAndAfterFree mr rm x) = GivenAndAfterFree mr rm $ f x
    fmap f (WhenFree mt ft x)          = WhenFree mt ft $ f x

given_ :: m () -> Free (GivenFree m t q) ()
given_ m = liftF $ GivenFree m ()

givenAndAfter_ :: m r -> (r -> m ()) -> Free (GivenFree m t q) ()
givenAndAfter_ g td = liftF $ GivenAndAfterFree g td ()

when_ :: m t -> Free (ThenFree m t q) b -> Free (GivenFree m t q) ()
when_ mt ts = liftF $ WhenFree mt ts ()

type BddFree m t q = Free (GivenFree m t q)

bddFree :: Free (GivenFree m t q) x -> BDDPreparing m t q

bddFree (Free (GivenFree m f))             = Given m $ bddFree f
bddFree (Free (GivenAndAfterFree mr rm f)) = GivenAndAfter mr rm $ bddFree f
bddFree (Free (WhenFree mt ts _))          = When mt $ thens ts
bddFree (Pure _)                           = error "empty tests not allowed"

data ThenFree m t q a
    = ThenFree (t -> m q) a
    deriving Functor

then_ :: (t -> m q) -> Free (ThenFree m t q) ()
then_ m = liftF $ ThenFree m ()

-- then_ set digestor
thens :: Free (ThenFree m t q) a -> BDDTesting m t q
thens (Free (ThenFree m f)) = Then m $ thens f
thens (Pure _)              = End


-- | Bdd to TestTree tasty test
testBddFree ::
    (MonadIO m , TestableMonad m, Typeable t)
    => String -- ^ test name
    -> BddFree m t () () -- ^ bdd test definition
    -> TestTree  -- ^ resulting tasty test
testBddFree s = singleTest s . interpret . bddFree
{-
-- | the when action in disguise
hoare :: Free (GivenFree m t q) a
         -> m t
         -> Free (ThenFree m t q) a
         -> BDDTesting m t q
         -> BDDPreparing m t q
hoare g at t = givens g . When at . thens t-}

{-
example = let
    gs = do
                given_ $ print 1
                given_ $ print 2
    ts = do
                then_ $ const $ return ()
                then_ $ const $ return ()
    in gs `when_` (return 'a')  ts-}


