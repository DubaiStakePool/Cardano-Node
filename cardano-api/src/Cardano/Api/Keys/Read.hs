{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Keys.Read
  ( readKeyFile
  , readKeyFileTextEnvelope
  , readKeyFileAnyOf
  ) where

import           Control.Exception
import           Data.Bifunctor
import           Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty)

import           Cardano.Api.DeserialiseAnyOf
import           Cardano.Api.Error
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.IO (File (..), FileDirection (..))
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Utils

-- | Read a cryptographic key from a file.
--
-- The contents of the file can either be Bech32-encoded, hex-encoded, or in
-- the text envelope format.
readKeyFile
  :: AsType a
  -> NonEmpty (InputFormat a)
  -> File 'In
  -> IO (Either (FileError InputDecodeError) a)
readKeyFile asType acceptedFormats path = do
  eContent <- fmap Right (readFileBlocking path) `catches` [Handler handler]
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError (unFile path)) $ deserialiseInput asType acceptedFormats content
 where
  handler :: IOException -> IO (Either (FileError InputDecodeError) BS.ByteString)
  handler e = return . Left $ FileIOError (unFile path) e

-- | Read a cryptographic key from a file.
--
-- The contents of the file must be in the text envelope format.
readKeyFileTextEnvelope
  :: HasTextEnvelope a
  => AsType a
  -> File 'In
  -> IO (Either (FileError InputDecodeError) a)
readKeyFileTextEnvelope asType fp =
    first (fmap InputTextEnvelopeError) <$> readFileTextEnvelope asType fp

-- | Read a cryptographic key from a file given that it is one of the provided
-- types.
--
-- The contents of the file can either be Bech32-encoded or in the text
-- envelope format.
readKeyFileAnyOf
  :: forall b.
     [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> File 'In
  -> IO (Either (FileError InputDecodeError) b)
readKeyFileAnyOf bech32Types textEnvTypes path = do
  eContent <- fmap Right (readFileBlocking path) `catches` [Handler handler]
  case eContent of
    Left e -> return $ Left e
    Right content ->
      return . first (FileError (unFile path)) $ deserialiseInputAnyOf bech32Types textEnvTypes content
 where
  handler :: IOException -> IO (Either (FileError InputDecodeError) BS.ByteString)
  handler e = return . Left $ FileIOError (unFile path) e

