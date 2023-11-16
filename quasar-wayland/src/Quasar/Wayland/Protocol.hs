module Quasar.Wayland.Protocol (
  -- * Wayland wire protocol
  -- | This module exports everything required to implement Wayland interfaces generated by
  -- "Quasar.Wayland.Protocol.TH".

  Object(objectProtocol, version),
  NewObject,
  CallM,
  tryCall,
  CallbackM,
  setEventHandler,
  setRequestHandler,
  setMessageHandler,
  getMessageHandler,
  setInterfaceData,
  getInterfaceData,
  isObjectDestroyed,
  attachFinalizer,

  -- ** Wayland types
  WlFixed(..),
  fixedToDouble,
  doubleToFixed,
  WlString(..),
  toString,
  nullWlString,
  SharedFd,

  -- ** Protocol execution
  ProtocolHandle,
  initializeProtocol,
  feedInput,
  takeOutbox,
  setException,

  -- * Protocol exceptions
  WireCallbackFailed(..),
  ParserFailed(..),
  ProtocolException(..),
  ProtocolUsageError(..),
  MaximumIdReached(..),
  ServerError(..),
  InternalError(..),

  -- ** Classes for generated interfaces
  IsInterface(InterfaceName),
  interfaceName,
  Version,
  maxVersion,
  interfaceVersion,
  Side(..),
  IsSide(MessageHandler),
  IsInterfaceSide,

  -- * For wl_display
  handleWlDisplayError,
  handleWlDisplayDeleteId,

  -- * For wl_registry
  GenericNewId,
  bindNewObject,
) where

import Quasar.Wayland.Protocol.Core
