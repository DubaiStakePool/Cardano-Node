{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Types
  ( AcceptedItems
  , TraceObjects
  , Metrics
  , NodeId (..)
  , NodeName
  , getNodeName
  , addressToNodeId
  , initAcceptedItems
  , prepareAcceptedItems
  ) where

import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import           Control.Monad (unless)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Text (Text, pack, splitOn, unpack)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import qualified System.Metrics as EKG

import           Cardano.Logging (TraceObject)

import           Trace.Forward.Protocol.Type (NodeInfoStore)

import           System.Metrics.Store.Acceptor (MetricsLocalStore, emptyMetricsLocalStore)

-- | Human-readable name of node.
type NodeName = Text

getNodeName :: NodeInfoStore -> IO (Maybe NodeName)
getNodeName niStore = lookup "NodeName" <$> readIORef niStore

-- | Unique identifier of node: its IP:port.
data NodeId = NodeId
  { nodeIP   :: !String
  , nodePort :: !Word16
  } deriving (Eq, Generic, Hashable, Ord)

instance Show NodeId where
  show (NodeId pipe 0)  = pipe
  show (NodeId ip port) = ip <> "-" <> show port

addressToNodeId :: String -> NodeId
addressToNodeId remoteAddress =
  -- The string 'remoteAddress' can contain the normal address (IP:port)
  -- or the path to local pipe file.
  case splitOn ":" . pack $ remoteAddress of
    [ip, port] -> NodeId (unpack ip) (read (unpack port) :: Word16)
    _          -> NodeId "LocalPipe" 0

type TraceObjects = TBQueue TraceObject

type Metrics = (EKG.Store, IORef MetricsLocalStore)

type AcceptedItems = IORef (HashMap NodeId (NodeInfoStore, TraceObjects, Metrics))

initAcceptedItems :: IO AcceptedItems
initAcceptedItems = newIORef HM.empty

prepareAcceptedItems
  :: NodeId
  -> AcceptedItems
  -> IO ()
prepareAcceptedItems nodeId itemsIORef = do
  items' <- readIORef itemsIORef
  -- If such 'nodeId' is already presented in 'items', it means that this node
  -- already worked with the tracer and now it's re-connect to the tracer.
  -- No need to re-create its stores.
  unless (nodeId `HM.member` items') $ do
    niStore    <- newIORef []
    trObQueue  <- newTBQueueIO 2000
    ekgStore   <- EKG.newStore
    localStore <- newIORef emptyMetricsLocalStore
    let storesForNewNode = (niStore, trObQueue, (ekgStore, localStore))
    atomicModifyIORef' itemsIORef $ \items ->
      (HM.insert nodeId storesForNewNode items, ())
