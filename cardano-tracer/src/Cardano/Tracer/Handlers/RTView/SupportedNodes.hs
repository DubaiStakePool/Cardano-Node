module Cardano.Tracer.Handlers.RTView.SupportedNodes
  ( supportedNodesVersions
  ) where

-- | It is assumed that RTView works correctly with these versions of the nodes only.
supportedNodesVersions :: [String]
supportedNodesVersions =
  [ "1.27.0"
  , "1.27.1"
  ]
