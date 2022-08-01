{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quasar.Wayland.WlFixedSpec (spec) where

import Foreign.C.Types
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Protocol (WlFixed(..), fixedToDouble, doubleToFixed)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Test.Hspec
import Test.QuickCheck


deriving newtype instance Arbitrary WlFixed

C.include "<stdint.h>"

-- Libwayland implementation wl_fixed_to_double
-- https://github.com/wayland-project/wayland/blob/5e4253ed50cfc9a132b5231f68a7084e0c3dc417/src/wayland-util.h#L596-L644
libwaylandFixedToDouble :: WlFixed -> Double
libwaylandFixedToDouble (WlFixed value) = unsafeDupablePerformIO do
  CDouble result <- [CU.block|
    double {
      union {
        double d;
        int64_t i;
      } u;

      u.i = ((1023LL + 44LL) << 52) + (1LL << 51) + $(int32_t value);

      return u.d - (3LL << 43);
    }
  |]
  pure result

-- Libwayland implementation wl_fixed_from_double
-- https://github.com/wayland-project/wayland/blob/5e4253ed50cfc9a132b5231f68a7084e0c3dc417/src/wayland-util.h#L596-L644
libwaylandDoubleToFixed :: Double -> WlFixed
libwaylandDoubleToFixed (CDouble -> value) = WlFixed $ unsafeDupablePerformIO
  [CU.block|
    int32_t {
      union {
        double d;
        int64_t i;
      } u;

      u.d = $(double value) + (3LL << (51 - 8));

      return (int32_t)u.i;
    }
  |]

spec :: Spec
spec = do
  describe "fixedToDouble and doubleToFixed" do
    it "produces the same value when passing through both functions" $ property \value -> do
      doubleToFixed (fixedToDouble value) `shouldBe` value
  describe "fixedToDouble" do
    it "behaves like wl_fixed_to_double" $ property \value -> do
      fixedToDouble value `shouldBe` libwaylandFixedToDouble value
    it "is correct when interacting with wl_fixed_from_double" $ property \value -> do
      libwaylandDoubleToFixed (fixedToDouble value) `shouldBe` value
  describe "doubleToFixed" do
    it "behaves like wl_fixed_from_double" $ property \value -> do
      doubleToFixed value `shouldBe` libwaylandDoubleToFixed value
    it "is correct when interacting with wl_fixed_to_double"  $ property \value -> do
      doubleToFixed (libwaylandFixedToDouble value) `shouldBe` value
