module Quasar.Wayland.Protocol (
  -- * Wayland wire protocol
  -- | This module exports everything required to implement Wayland interfaces generated by
  -- 'Quasar.Wayland.Protocol.TH'.

  -- ** Wire types
  ObjectId,
  GenericObjectId,
  NewId(..),
  Fixed(..),

  -- ** Classes for generated interfaces
  IsInterface(Request, Event, InterfaceName, interfaceName),
  Side(..),
  IsSide(Up, Down),
  IsInterfaceSide,

  -- ** Protocol execution
  ProtocolHandle,
  initializeProtocol,
  feedInput,
  takeOutbox,
  setException,

  -- ** Low-level protocol interaction
  ProtocolM,
  runProtocolTransaction,
  runProtocolM,
  Object,
  newObject,
  sendMessage,

  Callback(..),
  internalFnCallback,
  traceCallback,
  ignoreMessage,

  -- * Protocol exceptions
  CallbackFailed(..),
  ParserFailed(..),
  ProtocolException(..),
  MaximumIdReached(..),
  ServerError(..),
) where

import Quasar.Wayland.Protocol.Core
