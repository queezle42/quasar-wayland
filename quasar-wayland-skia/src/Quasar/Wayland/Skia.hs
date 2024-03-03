{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia (
  test,
) where

import Quasar.Prelude
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.C.Inline.Cpp qualified as CPP

C.context CPP.cppCtx

C.include "include/core/SkGraphics.h"

test :: IO ()
test = do
  [C.block|void { SkGraphics::Init(); }|]
  putStrLn "hi"
