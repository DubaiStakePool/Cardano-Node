{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.Tracer.Handlers.Logs.Journal
  ( writeTraceObjectsToJournal
  ) where

#if defined(LINUX)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Systemd.Journal (JournalFields, Priority (..), message, mkJournalField,
                                  priority, sendJournalFields, syslogIdentifier)

import           Cardano.Logging (TraceObject (..))
import qualified Cardano.Logging as L

import           Cardano.Tracer.Types
#else
import           System.IO (hPutStrLn, stderr)

import           Cardano.Logging

import           Cardano.Tracer.Types
#endif

#if defined(LINUX)
writeTraceObjectsToJournal
  :: NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToJournal _ _ [] = return ()
writeTraceObjectsToJournal nodeId nodeName traceObjects =
  mapM_ (sendJournalFields . mkJournalFields) traceObjects
 where
  mkJournalFields (TraceObject _   Nothing            Nothing)              = HM.empty
  mkJournalFields (TraceObject ctx (Just msgForHuman) Nothing)              = mkJournalFields' ctx msgForHuman
  mkJournalFields (TraceObject ctx Nothing            (Just msgForMachine)) = mkJournalFields' ctx msgForMachine
  mkJournalFields (TraceObject ctx (Just msgForHuman) (Just _))             = mkJournalFields' ctx msgForHuman

  mkJournalFields' ctx msg =
       syslogIdentifier (nodeName <> T.pack (show nodeId))
    <> message msg
    <> priority (mkPriority $ L.lcSeverity ctx)
    <> HM.fromList [ (namespace, encodeUtf8 (mkName $ L.lcNamespace ctx))
                   -- , (thread,    encodeUtf8 . T.pack . show . tid $ lometa)
                   -- , (time,      encodeUtf8 . formatAsIso8601 . tstamp $ lometa)
                   ]

  mkName [] = "noname"
  mkName names = T.intercalate "." names

  namespace = mkJournalField "namespace"
  -- thread    = mkJournalField "thread"
  -- time      = mkJournalField "time"

  -- formatAsIso8601 = T.pack . formatTime defaultTimeLocale "%F %T%12QZ"

mkPriority :: Maybe L.SeverityS -> Priority
mkPriority Nothing            = Info
mkPriority (Just L.Debug)     = Debug
mkPriority (Just L.Info)      = Info
mkPriority (Just L.Notice)    = Notice
mkPriority (Just L.Warning)   = Warning
mkPriority (Just L.Error)     = Error
mkPriority (Just L.Critical)  = Critical
mkPriority (Just L.Alert)     = Alert
mkPriority (Just L.Emergency) = Emergency
#else
writeTraceObjectsToJournal
  :: NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToJournal _ _ _ =
  hPutStrLn stderr "Writing to systemd's journal is available on Linux only."
#endif
