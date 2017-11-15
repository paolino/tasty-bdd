{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.BDD.LanguageFree (
        given_
        , givenAndAfter_
        , hoare
        , then_
        , GivenFree
        , ThenFree
    ) where

import           Control.Monad.Free
import           Test.BDD.Language

data GivenFree m a where
    GivenFree :: m () -> a -> GivenFree m a
    GivenAndAfterFree :: m r -> (r -> m ()) -> a -> GivenFree m a

instance Functor (GivenFree m) where
    fmap f (GivenFree m x)             = GivenFree m $ f x
    fmap f (GivenAndAfterFree mr rm x) = GivenAndAfterFree mr rm $ f x

given_ :: m () -> Free (GivenFree m) ()
given_ m = liftF $ GivenFree m ()

givenAndAfter_ :: m r -> (r -> m ()) -> Free (GivenFree m) ()
givenAndAfter_ g td = liftF $ GivenAndAfterFree g td ()


givens (Free (GivenFree m f))             = Given m . givens f
givens (Free (GivenAndAfterFree mr rm f)) = GivenAndAfter mr rm . givens f
givens (Pure _)                           = id

data ThenFree m t q a
    = ThenFree (t -> m q) a
    deriving Functor

then_ :: (t -> m q) -> Free (ThenFree m t q) ()
then_ m = liftF $ ThenFree m ()

thens (Free (ThenFree m f)) = Then m . thens f
thens (Pure _)              = id


hoare :: Free (GivenFree m) a
         -> m t
         -> Free (ThenFree m t q) a
         -> Language m t q Testing
         -> Language m t q Preparing
hoare g at t = givens g . When at . thens t

{-
example = let
    gs = do
                given_ $ print 1
                given_ $ print 2
    ts = do
                then_ $ const $ return ()
                then_ $ const $ return ()
    in gs `when_` (return 'a')  ts-}


