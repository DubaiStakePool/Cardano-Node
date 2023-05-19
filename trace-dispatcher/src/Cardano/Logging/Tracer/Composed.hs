{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Monad law, left identity" -}

module Cardano.Logging.Tracer.Composed (
    traceTracerInfo
  , mkCardanoTracer
  , mkCardanoTracer'
  , mkMetricsTracer
  ) where

import           Cardano.Logging.Configuration
import           Cardano.Logging.Formatter
import           Cardano.Logging.Trace
import           Cardano.Logging.TraceDispatcherMessage
import           Cardano.Logging.Types
import qualified Control.Tracer as T

import           Control.Monad (when)
import           Data.IORef
import qualified Data.List as L
import           Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import           Data.Text hiding (map)



-- | Construct a tracer according to the requirements for cardano node.
-- The tracer gets a 'name', which is appended to its namespace.
-- The tracer has to be an instance of LogFormat-ting for the display of
-- messages and an instance of MetaTrace for meta information such as
-- severity, privacy, details and backends'.
-- The tracer gets the backends': 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
-- The returned tracer needs to be configured with a configuration.

mkCardanoTracer :: forall evt.
     ( LogFormatting evt
     , MetaTrace evt)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> IO (Trace IO evt)
mkCardanoTracer trStdout trForward mbTrEkg tracerPrefix =
    mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix noHook
  where
    noHook :: Trace IO evt -> IO (Trace IO evt)
    noHook = pure

-- | Adds the possibility to add special tracers via the hook function
mkCardanoTracer' :: forall evt evt1.
     ( LogFormatting evt1
     , MetaTrace evt1)
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkCardanoTracer' trStdout trForward mbTrEkg tracerPrefix hook = do

    internalTr <- fmap (appendPrefixNames ["Reflection"])
                       (withBackendsFromConfig (backendsAndFormat trStdout trForward))

    -- handle the messages
    messageTrace <- withBackendsFromConfig (backendsAndFormat trStdout trForward)
                    >>= withLimitersFromConfig internalTr
                    >>= traceNamespaceErrors internalTr
                    >>= addContextAndFilter
                    >>= maybeSilent isSilentTracer tracerPrefix False
                    >>= hook

    -- handle the metrics
    metricsTrace <- (maybeSilent hasNoMetrics tracerPrefix True
                        . filterTrace (\ (_, v) -> not (Prelude.null (asMetrics v))))
                        (case mbTrEkg of
                            Nothing -> Trace T.nullTracer
                            Just ekgTrace -> metricsFormatter "Cardano" ekgTrace)
                    >>= hook

    pure (messageTrace <> metricsTrace)

  where
    addContextAndFilter :: Trace IO evt1 -> IO (Trace IO evt1)
    addContextAndFilter tr = do
      tr'  <- withDetailsFromConfig tr
      tr'' <- filterSeverityFromConfig tr'
      pure $ withDetails
            $ withSeverity
              $ withPrivacy
                $ withInnerNames
                  $ appendPrefixNames tracerPrefix tr''

    traceNamespaceErrors ::
         Trace IO TraceDispatcherMessage
      -> Trace IO evt1
      -> IO (Trace IO evt1)
    traceNamespaceErrors internalTr (Trace tr) = do
        pure $ Trace (T.arrow (T.emit
          (\case
            (lc, Right e) -> process lc (Right e)
            (lc, Left e) -> T.traceWith tr (lc, Left e))))
      where
        process :: LoggingContext -> Either TraceControl evt1 -> IO ()
        process lc cont = do
          when (isNothing (lcPrivacy lc)) $
                  traceWith
                    internalTr
                    (UnknownNamespace (lcNSPrefix lc) (lcNSInner lc) UKFPrivacy)
          when (isNothing (lcSeverity lc)) $
                  traceWith
                    internalTr
                    (UnknownNamespace (lcNSPrefix lc) (lcNSInner lc) UKFSeverity)
          when (isNothing (lcDetails lc)) $
                  traceWith
                    internalTr
                    (UnknownNamespace (lcNSPrefix lc) (lcNSInner lc) UKFDetails)
          T.traceWith tr (lc, cont)

backendsAndFormat ::
     LogFormatting a
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe [BackendConfig]
  -> Trace IO x
  -> IO (Trace IO a)
backendsAndFormat trStdout trForward mbBackends _ =
  let backends' = fromMaybe
                  [EKGBackend, Forwarder, Stdout HumanFormatColoured]
                  mbBackends
  in do
    let mbForwardTrace  = if Forwarder `L.elem` backends'
                            then Just $ filterTraceByPrivacy (Just Public)
                                (forwardFormatter' Nothing trForward)
                            else Nothing
        mbStdoutTrace   | Stdout HumanFormatColoured `L.elem` backends'
                        = Just (humanFormatter' True Nothing trStdout)
                        | Stdout HumanFormatUncoloured `L.elem` backends'
                        = Just (humanFormatter' False Nothing trStdout)
                        | Stdout MachineFormat `L.elem` backends'
                        = Just (machineFormatter' Nothing trStdout)
                        | otherwise = Nothing
    case mbForwardTrace <> mbStdoutTrace of
      Nothing -> pure $ Trace T.nullTracer
      Just tr -> preFormatted backends' tr

traceTracerInfo ::
     Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> ConfigReflection
  -> IO ()
traceTracerInfo trStdout trForward cr = do
    internalTr <- backendsAndFormat
                      trStdout
                      trForward
                      (Just [Forwarder, Stdout MachineFormat])
                      (Trace T.nullTracer)
    silentSet <- readIORef (crSilent cr)
    metricSet <- readIORef (crNoMetrics cr)
    allTracerSet <- readIORef (crAllTracers cr)
    let silentList  = map (intercalate (singleton '.')) (Set.toList silentSet)
    let metricsList = map (intercalate (singleton '.')) (Set.toList metricSet)
    let allTracersList = map (intercalate (singleton '.')) (Set.toList allTracerSet)
    traceWith (withInnerNames (appendPrefixNames ["Reflection"] internalTr))
              (TracerInfo silentList metricsList allTracersList)
    writeIORef (crSilent cr) Set.empty
    writeIORef (crNoMetrics cr) Set.empty
    writeIORef (crAllTracers cr) Set.empty

-- A basic ttracer just for metrics
mkMetricsTracer :: Maybe (Trace IO FormattedMessage) -> Trace IO FormattedMessage
mkMetricsTracer mbTrEkg = case mbTrEkg of
                          Nothing -> Trace T.nullTracer
                          Just ekgTrace -> ekgTrace
