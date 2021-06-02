{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.File
  ( writeTraceObjectsToFile
  ) where

import           Control.Monad (unless)
import           Data.Aeson (ToJSON, Value (..), (.=), object, toJSON)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))

import           Cardano.Logging

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (createLogAndSymLink, doesSymLinkValid,
                                                   symLinkName)
import           Cardano.Tracer.Types (NodeId, NodeName)

writeTraceObjectsToFile
  :: NodeId
  -> NodeName
  -> FilePath
  -> LogFormat
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToFile _ _ _ _ [] = return ()
writeTraceObjectsToFile nodeId nodeName rootDir format traceObjects = do
  createDirectoryIfMissing True rootDir  
  createDirectoryIfMissing True subDirForLogs
  symLinkIsHere <- doesFileExist pathToCurrentLog
  unless symLinkIsHere $
    createLogAndSymLink subDirForLogs format
  -- Symlink can be broken, check it.
  doesSymLinkValid pathToCurrentLog >>= \case
    True ->
      writeTraceObjects pathToCurrentLog (formatter format) traceObjects
    False -> do
      -- Symlink is here, but it's broken.
      removeFile pathToCurrentLog
      createLogAndSymLink subDirForLogs format
 where
  subDirForLogs = rootDir </> nodeFullId
  nodeFullId = if T.null nodeName
                 then show nodeId
                 else T.unpack nodeName <> "-" <> show nodeId
  -- This is a symlink to the current log file, please see rotation parameters.
  pathToCurrentLog = subDirForLogs </> symLinkName format

  formatter AsText = traceObjectToText
  formatter AsJSON = traceObjectToJSON

  writeTraceObjects logPath formatIt =
    -- It's much more efficiently to encode 'Text' explicitly and
    -- then perform 'ByteString'-level 'IO' than perform 'Text'-level 'IO'.
      LBS.appendFile logPath
    . encodeUtf8
    . TL.append nl
    . TL.intercalate nl
    . mapMaybe formatIt

#if defined(mingw32_HOST_OS)
  nl = "\r\n"
#else
  nl = "\n"
#endif

traceObjectToText :: TraceObject -> Maybe TL.Text
traceObjectToText (TraceObject _   Nothing            _) = Nothing
traceObjectToText (TraceObject ctx (Just msgForHuman) _) = Just $
  "[" <> host <> name <> ":" <> sev <> ":" <> thId <> "] [" <> time <> "] " <> msg
 where
  host = ""
  name = mkName $ lcNamespace ctx
  sev = TL.pack . show $ lcSeverity ctx
  thId = ""
  time = ""
  msg = TL.fromStrict msgForHuman

mkName :: Namespace -> TL.Text
mkName [] = "noname"
mkName names = TL.fromStrict $ T.intercalate "." names

data TraceObjectForJSON = TraceObjectForJSON
  { jAt   :: !UTCTime
  , jNS   :: !TL.Text
  , jData :: !T.Text -- !Value
  , jHost :: !T.Text
  , jSev  :: !T.Text
  , jTId  :: !T.Text
  }

instance ToJSON TraceObjectForJSON where
  toJSON TraceObjectForJSON{..} =
    object [ "at"     .= formatTime defaultTimeLocale "%FT%T%2Q%Z" jAt
           , "ns"     .= TL.toStrict jNS
           , "data"   .= jData
           , "host"   .= jHost
           , "sev"    .= jSev
           , "thread" .= jTId
           ]

traceObjectToJSON :: TraceObject -> Maybe TL.Text
traceObjectToJSON (TraceObject _   _ Nothing) = Nothing
traceObjectToJSON (TraceObject ctx _ (Just msgForMachine)) = Just . encodeToLazyText $
  TraceObjectForJSON
    { jAt   = undefined --tstamp lometa
    , jNS   = mkName $ lcNamespace ctx
    , jData = msgForMachine
    , jHost = undefined --hostname lometa
    , jSev  = T.pack . show $ lcSeverity ctx
    , jTId  = undefined --T.filter isDigit (T.pack . show . tid $ lometa)
    }
