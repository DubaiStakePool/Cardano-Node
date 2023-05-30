{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Formatter (
    metricsFormatter
  , preFormatted
  , forwardFormatter
  , forwardFormatter'
  , machineFormatter
  , machineFormatter'
  , humanFormatter
  , humanFormatter'
) where

import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString.Lazy as BS
import           Data.Functor.Contravariant
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, stripPrefix)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder as TB
import           Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)

import           Cardano.Logging.Types
import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Network.HostName


-- | Format this trace as metrics
metricsFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Text
  -> Trace m FormattedMessage
  -> Trace m a
metricsFormatter application (Trace tr) = Trace $
    T.contramap
      (\ case
        (lc, Right v) ->
          let metrics = asMetrics v
          in (lc { lcNSPrefix = application : lcNSPrefix lc}
                 , Right (FormattedMetrics metrics))
        (lc, Left ctrl) ->
             (lc { lcNSPrefix = application : lcNSPrefix lc}
                 , Left ctrl))
      tr


-- | Transform this trace to a preformatted message, so that double serialization
-- is avoided
preFormatted ::
  (  LogFormatting a
  ,  Monad m
  ,  MonadIO m)
  => [BackendConfig]
  -> Trace m (PreFormatted a)
  -> m (Trace m a)
preFormatted backends' (Trace tr) = do
  hostname <- liftIO getHostName
  pure $ Trace $ T.arrow $ T.emit $
    \ case
      (lc, Right msg) -> do
        time     <- liftIO getCurrentTime
        threadId <- liftIO myThreadId
        let threadText = fromMaybe
                            ((pack . show) threadId)
                            ((stripPrefix "ThreadId " . pack . show) threadId)
            timestamp = time
            details = fromMaybe DNormal (lcDetails lc)
            condForHuman = if elem (Stdout HumanFormatUncoloured) backends'
                          || elem (Stdout HumanFormatColoured) backends'
                          || elem Forwarder backends'
                          then Just $ forHuman msg
                          else Nothing
            machineFormatted = forMachine details msg
        T.traceWith tr (lc, Right (PreFormatted
                            { pfMessage = msg
                            , pfForHuman = condForHuman
                            , pfForMachine = machineFormatted
                            , pfTimestamp = timeFormatted timestamp
                            , pfTime = timestamp
                            , pfNamespace = lcNSPrefix lc ++ lcNSInner lc
                            , pfHostname = hostname
                            , pfThreadId = threadText
                            }))
      (lc, Left ctrl) ->
        T.traceWith tr (lc, Left ctrl)

-- | Format this trace as TraceObject for the trace forwarder
forwardFormatter'
  :: forall a m .
     MonadIO m
  => Maybe Text
  -> Trace m FormattedMessage
  -> Trace m (PreFormatted a)
forwardFormatter' condApplication (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
            let ns' = case condApplication of
                                  Just app -> app : pfNamespace v
                                  Nothing -> pfNamespace v
                ns'' = mconcat (intersperse (singleton '.')
                                    (map fromText ns'))
                nsText = toStrict (toLazyText ns'')
                machineObj = AE.pairs $
                                  "at"       .= pfTimestamp v
                                <> "ns"      .= nsText
                                <> "data"    .= pfForMachine v
                                <> "sev"     .= fromMaybe Info (lcSeverity lc)
                                <> "thread"  .= pfThreadId v
                                <> "host"    .= pfHostname v
                forMachine' = decodeUtf8
                              $ BS.toStrict
                                $ AE.encodingToLazyByteString machineObj
                to = TraceObject {
                    toHuman     = pfForHuman v
                  , toMachine   = forMachine'
                  , toNamespace = ns'
                  , toSeverity  = fromMaybe Info (lcSeverity lc)
                  , toDetails   = fromMaybe DNormal (lcDetails lc)
                  , toTimestamp = pfTime v
                  , toHostname  = pfHostname v
                  , toThreadId  = pfThreadId v
                }
            in (lc, Right (FormattedForwarder to))
      (lc, Left ctrl) ->
        (lc { lcNSPrefix = case condApplication of
                                            Just app -> app : lcNSPrefix lc
                                            Nothing  -> lcNSPrefix lc}
            , Left ctrl))
      tr

-- | Format this trace as TraceObject for the trace forwarder
machineFormatter'
  :: forall a m .
     MonadIO m
  => Maybe Text
  -> Trace m FormattedMessage
  -> Trace m (PreFormatted a)
machineFormatter' condApplication (Trace tr) = Trace $
  contramap
    (\ case
      (lc, Right v) ->
            let ns' = case condApplication of
                                  Just app -> app : pfNamespace v
                                  Nothing -> pfNamespace v
                ns'' = mconcat (intersperse (singleton '.')
                                    (map fromText ns'))
                nsText = toStrict (toLazyText ns'')
                machineObj = AE.pairs $
                                  "at"       .= pfTimestamp v
                                <> "ns"      .= nsText
                                <> "data"    .= pfForMachine v
                                <> "sev"     .= fromMaybe Info (lcSeverity lc)
                                <> "thread"  .= pfThreadId v
                                <> "host"    .= pfHostname v
                forMachine' = decodeUtf8
                               $ BS.toStrict
                                 $ AE.encodingToLazyByteString machineObj
            in (lc, Right (FormattedMachine forMachine'))
      (lc, Left ctrl) ->
        (lc { lcNSPrefix = case condApplication of
                                            Just app -> app : lcNSPrefix lc
                                            Nothing  -> lcNSPrefix lc}
            , Left ctrl))
      tr

-- | Format this trace as TraceObject for the trace forwarder
humanFormatter'
  :: forall a m .
     MonadIO m
  => Bool
  -> Maybe Text
  -> Trace m FormattedMessage
  -> Trace m (PreFormatted a)
humanFormatter' withColor condApplication (Trace tr) =
  Trace $
      contramap
        (\ case
          (lc, Right v) ->
              let ns' = case condApplication of
                                    Just app -> app : pfNamespace v
                                    Nothing -> pfNamespace v
                  ns'' = mconcat (intersperse (singleton '.')
                                      (map fromText ns'))
                  severity' = fromMaybe Info (lcSeverity lc)
                  ns       = colorBySeverity
                                withColor
                                severity'
                                $ fromString (pfHostname v)
                                  <> singleton ':'
                                  <> ns''
                  tadd     = fromText " ("
                              <> fromString (show severity')
                              <> singleton ','
                              <> fromText (pfThreadId v)
                              <> fromText ") "
                  forHuman' = fromMaybe
                                (decodeUtf8
                                  $ BS.toStrict
                                    $ AE.encodingToLazyByteString
                                        $ AE.pairs ("data"    .= pfForMachine v))
                                (pfForHuman v)
                  forHuman'' = toStrict
                                $ toLazyText
                                  $ squareBrackets (fromText (pfTimestamp v))
                                    <> singleton ' '
                                    <> squareBrackets ns
                                    <> tadd
                                    <> fromText forHuman'
                  in (lc, Right (FormattedHuman withColor forHuman''))
          (lc, Left ctrl) ->
            (lc { lcNSPrefix = case condApplication of
                                                Just app -> app : lcNSPrefix lc
                                                Nothing  -> lcNSPrefix lc}
                , Left ctrl))
          tr

squareBrackets :: Builder -> Builder
squareBrackets b = singleton '[' <> b <> singleton ']'

-- | Color a text message based on `Severity`. `Error` and more severe errors
-- are colored red, `Warning` is colored yellow, and all other messages are
-- rendered in the default color.
colorBySeverity :: Bool -> SeverityS -> Builder -> Builder
colorBySeverity withColor severity' msg =
    case severity' of
      Emergency -> red msg
      Alert     -> red msg
      Critical  -> red msg
      Error     -> red msg
      Warning   -> yellow msg
      _         -> msg
  where
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s

timeFormatted :: UTCTime -> Text
timeFormatted = pack . formatTime defaultTimeLocale "%F %H:%M:%S%4QZ"

humanFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Bool
  -> Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
humanFormatter withColor condApplication tr = do
    let tr' = humanFormatter' withColor condApplication tr
    preFormatted [Stdout HumanFormatColoured] tr'

machineFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
machineFormatter condApplication tr = do
    let tr' = machineFormatter' condApplication tr
    preFormatted [Stdout MachineFormat] tr'

forwardFormatter
  :: forall a m .
     MonadIO m
  => LogFormatting a
  => Maybe Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
forwardFormatter condApplication tr = do
    let tr' = forwardFormatter' condApplication tr
    preFormatted [Stdout MachineFormat, Stdout HumanFormatColoured] tr'
