module Quasar.Wayland.Region (
  IsRegion,
  Region,
  newRegion,
  addDownstream,
  Rectangle(..),
  addToRegion,
  subtractFromRegion,
  destroyRegion,
  initializeServerRegion,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import GHC.Records


type IsRegion a = (
  HasField "destroy" a (STM ()),
  HasField "add" a (Int32 -> Int32 -> Int32 -> Int32 -> STM ()),
  HasField "subtract" a (Int32 -> Int32 -> Int32 -> Int32 -> STM ())
  )

data SomeRegion = forall a. IsRegion a => SomeRegion a

instance HasField "destroy" SomeRegion (STM ()) where
  getField (SomeRegion region) = region.destroy
instance HasField "add" SomeRegion (Int32 -> Int32 -> Int32 -> Int32 -> STM ()) where
  getField (SomeRegion region) = region.add
instance HasField "subtract" SomeRegion (Int32 -> Int32 -> Int32 -> Int32 -> STM ()) where
  getField (SomeRegion region) = region.subtract

data Region = Region {
  operations :: TVar [RegionOperation],
  downstreams :: TVar [SomeRegion],
  isDestroyed :: TVar Bool
}

initializeServerRegion :: Object 'Server Interface_wl_region -> STM ()
initializeServerRegion wlRegion = do
  region <- newRegion
  setMessageHandler wlRegion RequestHandler_wl_region {
    destroy = region.destroy,
    add = region.add,
    subtract = region.subtract
  }
  setInterfaceData wlRegion region

instance HasField "destroy" Region (STM ()) where
  getField = destroyRegion
instance HasField "add" Region (Int32 -> Int32 -> Int32 -> Int32 -> STM ()) where
  getField region x y width height = addToRegion region (Rectangle x y width height)
instance HasField "subtract" Region (Int32 -> Int32 -> Int32 -> Int32 -> STM ()) where
  getField region x y width height = subtractFromRegion region (Rectangle x y width height)

data RegionOperation = Add Rectangle | Subtract Rectangle

data Rectangle = Rectangle {
  x :: Int32,
  y :: Int32,
  width :: Int32,
  height :: Int32
}

contains :: Rectangle -> Rectangle -> Bool
contains (Rectangle x0 y0 width0 height0) (Rectangle x1 y1 width1 height1) =
  x0 <= x1 &&
  y0 <= y1 &&
  x0 + width0 >= x1 + width1 &&
  y0 + height0 >= y1 + height1

newRegion :: STM Region
newRegion = Region <$> newTVar mempty <*> newTVar mempty <*> newTVar False

addDownstream :: IsRegion a => Region -> a -> STM ()
addDownstream region (SomeRegion -> downstream) = do
  modifyTVar region.downstreams (downstream:)
  -- Replay operations for new downstream
  applyOperations downstream =<< readTVar region.operations


applyOperations :: SomeRegion -> [RegionOperation] -> STM ()
applyOperations region ops = mapM_ (applyOperation region) (reverse ops)

applyOperation :: SomeRegion -> RegionOperation -> STM ()
applyOperation region (Add rect) = region.add `appRect` rect
applyOperation region (Subtract rect) = region.subtract `appRect` rect

appRect :: (Int32 -> Int32 -> Int32 -> Int32 -> a) -> Rectangle -> a
appRect fn (Rectangle x y width height) = fn x y width height

callDownstreams :: (SomeRegion -> STM ()) -> Region -> STM ()
callDownstreams fn region = do
  downstreams <- readTVar region.downstreams
  -- Filter broken (e.g. disconnected) downstreams
  newDownstreams <- mapM (\downstream -> ((Just downstream <$ fn downstream) `catchAll` \_ -> pure Nothing) ) downstreams
  writeTVar region.downstreams (catMaybes newDownstreams)

addToRegion :: Region -> Rectangle -> STM ()
addToRegion region rect = do
  whenM (readTVar region.isDestroyed) (throwM (ProtocolUsageError "`add` called on destroyed Region"))
  whenM (addIsRequired rect <$> readTVar region.operations) do
    modifyTVar region.operations (addNormalized rect)
    callDownstreams (\downstream -> downstream.add `appRect` rect) region

addIsRequired :: Rectangle -> [RegionOperation] -> Bool
addIsRequired rect (Add top:_) = not (top `contains` rect)
addIsRequired _ _ = True

addNormalized :: Rectangle -> [RegionOperation] -> [RegionOperation]
addNormalized rect [] = [Add rect]
addNormalized rect old@(Add top:others)
  | rect `contains` top = addNormalized rect others
  | otherwise = Add rect : old
addNormalized rect old@(Subtract top:others)
  | rect `contains` top = addNormalized rect others
  | otherwise = Add rect : old


subtractFromRegion :: Region -> Rectangle -> STM ()
subtractFromRegion region rect = do
  whenM (readTVar region.isDestroyed) (throwM (ProtocolUsageError "`subtract` called on destroyed Region"))
  whenM (subtractIsRequired rect <$> readTVar region.operations) do
    modifyTVar region.operations (subtractNormalized rect)
    callDownstreams (\downstream -> downstream.subtract `appRect` rect) region

subtractIsRequired :: Rectangle -> [RegionOperation] -> Bool
subtractIsRequired rect (Subtract top:_) = not (top `contains` rect)
subtractIsRequired _ _ = True

subtractNormalized :: Rectangle -> [RegionOperation] -> [RegionOperation]
subtractNormalized _rect [] = []
subtractNormalized rect old@(Add top:others)
  | rect `contains` top = subtractNormalized rect others
  | otherwise = Subtract rect : old
subtractNormalized rect old@(Subtract top:others)
  | rect `contains` top = subtractNormalized rect others
  | otherwise = Subtract rect : old


destroyRegion :: Region -> STM ()
destroyRegion region = do
  unlessM (swapTVar region.isDestroyed True) do
    callDownstreams (.destroy) region
    writeTVar region.downstreams mempty
