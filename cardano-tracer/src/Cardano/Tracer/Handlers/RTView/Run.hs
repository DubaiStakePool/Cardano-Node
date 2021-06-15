{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  ) where

import           Control.Concurrent.Async (async, waitAnyCancel)
import           Control.Monad (void)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.WebServer (runWebServer)
import           Cardano.Tracer.Types (AcceptedItems)

runRTView
  :: Endpoint
  -> AcceptedItems
  -> IO ()
runRTView endpoint acceptedItems = do
  serverThr <- async $ runWebServer endpoint acceptedItems
  void $ waitAnyCancel [serverThr]
