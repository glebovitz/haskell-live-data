module Spec where

import           ClassyPrelude.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Scientific           (Scientific, fromFloatDigits)
import           StockTicker
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen       (choose)

main :: IO ()
main = hspec $ do
    describe "StockTick" $ do
        prop "tickToBS/sinkBS" $ \ticks -> do
            ticks' <- mapM_ (yield . tickToBS) ticks
                   $$ CL.sequence sinkTick
                   =$ sinkList
            ticks' `shouldBe` ticks

instance Arbitrary StockTick where
    arbitrary = StockTick
        <$> (do
                len <- choose (1, 80)
                pack <$> replicateM len (choose ('A', 'Z')))
        <*> (fromGregorian <$> choose (2010, 2015) <*> choose (1, 12) <*> choose (1, 13))
        <*> ((fromFloatDigits :: Double -> Scientific) <$> arbitrary)
