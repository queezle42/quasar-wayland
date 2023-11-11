module Quasar.Wayland.Region (
  IsRegion,
  Region,
  newRegion,
  addDownstream,
  Rectangle(..),
  appRect,
  appAsRect,
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
  HasField "destroy" a (STMc NoRetry '[] ()),
  HasField "add" a (Int32 -> Int32 -> Int32 -> Int32 -> STMc NoRetry '[SomeException] ()),
  HasField "subtract" a (Int32 -> Int32 -> Int32 -> Int32 -> STMc NoRetry '[SomeException] ())
  )

data SomeRegion = forall a. IsRegion a => SomeRegion a

instance HasField "destroy" SomeRegion (STMc NoRetry '[] ()) where
  getField (SomeRegion region) = region.destroy
instance HasField "add" SomeRegion (Int32 -> Int32 -> Int32 -> Int32 -> STMc NoRetry '[SomeException] ()) where
  getField (SomeRegion region) = region.add
instance HasField "subtract" SomeRegion (Int32 -> Int32 -> Int32 -> Int32 -> STMc NoRetry '[SomeException] ()) where
  getField (SomeRegion region) = region.subtract

data Region = Region {
  operations :: TVar [RegionOperation],
  downstreams :: TVar [SomeRegion],
  isDestroyed :: TVar Bool
}

initializeServerRegion :: NewObject 'Server Interface_wl_region -> STMc NoRetry '[] ()
initializeServerRegion wlRegion = do
  region <- newRegion
  setMessageHandler wlRegion RequestHandler_wl_region {
    destroy = liftSTMc region.destroy,
    add = region.add,
    subtract = region.subtract
  }
  setInterfaceData wlRegion region

instance HasField "destroy" Region (STMc NoRetry '[] ()) where
  getField = destroyRegion
instance HasField "add" Region (Int32 -> Int32 -> Int32 -> Int32 -> STMc NoRetry '[SomeException] ()) where
  getField region x y width height = addToRegion region (Rectangle x y width height)
instance HasField "subtract" Region (Int32 -> Int32 -> Int32 -> Int32 -> STMc NoRetry '[SomeException] ()) where
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

newRegion :: STMc NoRetry '[] Region
newRegion = Region <$> newTVar mempty <*> newTVar mempty <*> newTVar False

addDownstream :: IsRegion a => Region -> a -> STMc NoRetry '[] ()
addDownstream region (SomeRegion -> downstream) = do
  -- Replay operations for new downstream
  result <- tryAllSTMc (applyOperations downstream =<< readTVar region.operations)

  case result of
    Left _ex -> pure () -- Drop downstream if it is broken (e.g. disconnected)
    -- Attach downstream
    Right () -> modifyTVar region.downstreams (downstream:)


applyOperations :: SomeRegion -> [RegionOperation] -> STMc NoRetry '[SomeException] ()
applyOperations region ops = mapM_ (applyOperation region) (reverse ops)

applyOperation :: SomeRegion -> RegionOperation -> STMc NoRetry '[SomeException] ()
applyOperation region (Add rect) = region.add `appRect` rect
applyOperation region (Subtract rect) = region.subtract `appRect` rect

appRect :: (Int32 -> Int32 -> Int32 -> Int32 -> a) -> Rectangle -> a
appRect fn (Rectangle x y width height) = fn x y width height

appAsRect :: (Rectangle -> a) -> Int32 -> Int32 -> Int32 -> Int32 -> a
appAsRect fn x y width height = fn (Rectangle x y width height)

callDownstreams :: (SomeRegion -> STMc NoRetry '[SomeException] ()) -> Region -> STMc NoRetry '[] ()
callDownstreams fn region = do
  downstreams <- readTVar region.downstreams
  -- Filter broken (e.g. disconnected) downstreams
  newDownstreams <- mapM (\downstream -> (Just downstream <$ fn downstream) `catchAllSTMc` \_ -> pure Nothing) downstreams
  writeTVar region.downstreams (catMaybes newDownstreams)

addToRegion :: Region -> Rectangle -> STMc NoRetry '[SomeException] ()
addToRegion region rect = do
  whenM (readTVar region.isDestroyed) (throwM (ProtocolUsageError "`add` called on destroyed Region"))
  whenM (addIsRequired rect <$> readTVar region.operations) do
    modifyTVar region.operations (addNormalized rect)
    liftSTMc $ callDownstreams (\downstream -> downstream.add `appRect` rect) region

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


subtractFromRegion :: Region -> Rectangle -> STMc NoRetry '[SomeException] ()
subtractFromRegion region rect = do
  whenM (readTVar region.isDestroyed) (throwM (ProtocolUsageError "`subtract` called on destroyed Region"))
  whenM (subtractIsRequired rect <$> readTVar region.operations) do
    modifyTVar region.operations (subtractNormalized rect)
    liftSTMc $ callDownstreams (\downstream -> downstream.subtract `appRect` rect) region

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


destroyRegion :: Region -> STMc NoRetry '[] ()
destroyRegion region = do
  unlessM (swapTVar region.isDestroyed True) do
    downstreams <- readTVar region.downstreams
    mapM_ (.destroy) downstreams
    writeTVar region.downstreams mempty
