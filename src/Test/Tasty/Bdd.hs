{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Rank2Types        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Bdd (
    (@?=)
  , Language (..)
  , BDD
  , testBdd
  ) where

import Control.Arrow                   ((***))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Tagged
import Data.Typeable
import Test.BDD.Language
import Test.Tasty.Ingredients.FailFast
import Test.Tasty.Options
import Test.Tasty.Providers
import Text.Printf
import Text.Show.Pretty

class TestableMonad m where
    runCase :: m Result -> IO Result

instance TestableMonad IO where
    runCase = id

instance (Typeable t, TestableMonad m, MonadIO m, MonadCatch m, Typeable m) => IsTest (BDDTest m t Result) where
    run os (BDDTest ts rup w) f = runCase $ do
        teardowns <- sequence_ . reverse <$> mapM (\(GivenWithTeardown g a) -> a <$> g) rup
        resultOfWhen <- w
        let loop [] = return Nothing
            loop (then':xs) =  do
                    liftIO $ f (Progress "" (fromIntegral (length xs) / fromIntegral (length ts)))
                    (then' resultOfWhen >> loop xs)
                        `catch`
                        (\(EqualityDoesntHold e) ->
                             return (Just e))
        resultOfThen <- loop ts
        case resultOfThen of
            Just reason -> do
                case lookupOption os of
                    FailFast False -> teardowns
                    _              -> return ()
                return $ testFailed reason
            Nothing -> teardowns >> return (testPassed "")
    testOptions = Tagged [Option (Proxy :: Proxy FailFast)]

prettyDifferences :: (Show a) => a -> a -> String
prettyDifferences a1 a2 =
  let pa1 = lines $ ppShow a1
      pa2 = lines $ ppShow a2
      (equals, (diff1, diff2)) = (map fst *** unzip) . break (uncurry (/=)) $ zip pa1 pa2
  in printf "== MATCHING ==\n%s== LEFT ==\n%s== RIGHT==\n%s\n" (unlines equals) (unlines diff1) (unlines diff2)

newtype EqualityDoesntHold = EqualityDoesntHold String deriving (Show, Typeable)

instance Exception EqualityDoesntHold

infixl 4 @?=
(@?=) :: (Show a, Eq a, Typeable a, MonadThrow m) => a -> a -> m ()
a1 @?= a2 =
    if a1 == a2
    then return ()
    else throwM (EqualityDoesntHold (prettyDifferences a1 a2))

testBdd ::
    (Monad m, IsTest (BDDTest m t Result))
    => String
    -> (Language m t () 'Testing -> BDD m t)
    -> TestTree
testBdd s = singleTest s . makeBDD (testPassed "No test run") . ($ End)
