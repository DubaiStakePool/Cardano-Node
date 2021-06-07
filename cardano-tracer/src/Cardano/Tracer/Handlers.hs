module Cardano.Tracer.Handlers
  ( runHandlers
  ) where

import           Control.Concurrent.Async (async, waitAnyCancel)
import           Control.Monad (void)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems)
import           Cardano.Tracer.Handlers.Logs.Run (runLogsHandler)
import           Cardano.Tracer.Handlers.Metrics.Run (runMetricsHandler)
import           Cardano.Tracer.Handlers.RTView.Run (runRTView)

runHandlers
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runHandlers config acceptedItems = do
  lThr <- async $ runLogsHandler    config acceptedItems
  mThr <- async $ runMetricsHandler config acceptedItems
  void $ case hasRTView config of
    Just endPoint -> do
      rThr <- async $ runRTView endPoint acceptedItems
      waitAnyCancel [lThr, mThr, rThr]
    Nothing ->
      waitAnyCancel [lThr, mThr]
