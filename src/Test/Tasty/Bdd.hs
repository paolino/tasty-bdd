{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE Rank2Types        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Tasty.Bdd (
    (@?=)
    , (@?/=)
    , (^?=)
    , (^?/=)
    , Language (..)
    , testBdd
    , BDDTesting
    , BDDPreparing
    , TestableMonad (..)
    , failFastIngredients
    , failFastTester
    , prettyDifferences
    , beforeEach, afterEach, beforeAll, afterAll

) where

import Control.Arrow                   ((***))
import Control.Monad.Catch             (Exception(..), MonadCatch(..),
                                        MonadThrow(..))
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Data.Tagged                     (Tagged(..))
import Data.TreeDiff
import Data.Typeable                   (Proxy(..), Typeable)
import Test.BDD.Language
import Test.Tasty                      (defaultMainWithIngredients,
                                        withResource)
import Test.Tasty.Ingredients          (Ingredient)
import Test.Tasty.Ingredients.Basic    (consoleTestReporter, listingTests)
import Test.Tasty.Ingredients.FailFast (FailFast(..), failFast)
import Test.Tasty.Options              (OptionDescription(..), lookupOption)
import Test.Tasty.Providers            (IsTest(..), Progress(..), Result,
                                        TestTree, singleTest, testFailed,
                                        testPassed)
import Text.PrettyPrint                (render)
import Text.Printf                     (printf)
import Text.Show.Pretty                (ppShow)

-- | testable monads can map to IO a Tasty Result
class (MonadCatch m, MonadIO m, Monad m, Typeable m) => TestableMonad m where
    runCase :: m Result -> IO Result


instance TestableMonad IO where
    runCase = id

-- | any testable monad can make a BDDTest a tasty test
instance (Typeable t, TestableMonad m)
        => IsTest (BDDTest m t ()) where
    run os (BDDTest ts rup w) f = runCase $ do
        teardowns <- sequence_ . reverse <$> mapM (\(TestContext g a) -> a <$> g) rup
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

-- | show a coloured difference of 2 values
prettyDifferences :: (ToExpr a) => a -> a -> String
prettyDifferences a1 a2 =
    show $ ansiWlEditExpr $ exprDiff (toExpr a1) (toExpr a2)

-- internal exception to trigger visual inspection on output
newtype EqualityDoesntHold
    = EqualityDoesntHold String
    deriving (Show, Typeable)

instance Exception EqualityDoesntHold

infixl 4 @?=
-- | equality test which show pretty differences on fail
(@?=) :: (ToExpr a, Eq a, Typeable a, MonadThrow m) => a -> a -> m ()
a1 @?= a2 =
    if a1 == a2
    then return ()
    else throwM
        $ EqualityDoesntHold
        $ printf "Expected equality:\n%s"
        $ prettyDifferences a1 a2

-- | equality test which show pretty differences on fail
(@?/=) :: (ToExpr a, Eq a, Typeable a, MonadThrow m) => a -> a -> m ()
a1 @?/= a2 =
    if a1 /= a2
    then return ()
    else throwM
        $ EqualityDoesntHold
        $ printf "Expected inequality:\n%s"
        $ prettyDifferences a1 a2

(^?=) :: (ToExpr a, Eq a, Typeable a, MonadThrow m) => m a -> a -> b -> m ()
f ^?= t = const $ f >>= (@?= t)

(^?/=) :: (ToExpr a, Eq a, Typeable a, MonadThrow m) => m a -> a -> b -> m ()
f ^?/= t = const $ f >>= (@?/= t)


-- | Bdd to TestTree tasty test
testBdd s = singleTest s . interpret
testBdd ::
    (MonadIO m , TestableMonad m, Typeable t)
    => String -- ^ test name
    -> BDDPreparing m t () -- ^ bdd test definition
    -> TestTree  -- ^ resulting tasty test

beforeAll :: IO () -> TestTree  -> TestTree
beforeAll f = withResource f (const $ return ()) . const

afterAll :: IO () -> TestTree  -> TestTree
afterAll f = withResource (return ()) (const f) . const

beforeEach :: IO () -> [TestTree] -> [TestTree]
beforeEach = map . beforeAll

afterEach :: IO () -> [TestTree] -> [TestTree]
afterEach = map . afterAll

acquire :: IO a -> (IO a -> TestTree) -> TestTree
acquire f = withResource f (const $ return ())

-- | default test runner fail-fast aware
failFastTester :: TestTree -> IO ()
failFastTester = defaultMainWithIngredients failFastIngredients

-- | basic ingredients fail-fast aware
failFastIngredients :: [Ingredient]
failFastIngredients = [listingTests, failFast consoleTestReporter]


instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d, ToExpr e, ToExpr f)
        => ToExpr (a, b, c, d, e, f) where
        toExpr (a, b, c, d, e,f)
                = App "_×_×_×_×_x_"
                [toExpr a, toExpr b, toExpr c, toExpr d, toExpr e, toExpr f]


