module Main (main) where

import Control.Concurrent (runInBoundThread)
import Quasar.Prelude
import Quasar.Wayland.Gles
import Quasar.Wayland.Skia (test)
import System.IO

main :: IO ()
main = runInBoundThread do
  egl <- initializeGles
  glTexture <- test egl

  pure ()
