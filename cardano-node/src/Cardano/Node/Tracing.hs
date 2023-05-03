{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}


module Cardano.Node.Tracing
  ( Tracers (..)
  , ConsensusStartupException (..)
  ) where

import           Prelude (IO)

import           Codec.CBOR.Read (DeserialiseFailure)
import           "contra-tracer" Control.Tracer (Tracer(..))

import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Network.Diffusion as Diffusion

import           Ouroboros.Network.NodeToClient (LocalAddress, NodeToClientVersion)
import           Ouroboros.Network.NodeToNode (NodeToNodeVersion, RemoteAddress)

import           Cardano.Node.Handlers.Shutdown (ShutdownTrace)
import           Cardano.Node.Startup (NodeInfo, NodeStartupInfo, StartupTrace(..))

import           Cardano.Logging.Resources
import           Cardano.Node.Tracing.StateRep (NodeState)
import           Cardano.Node.Tracing.Tracers.ConsensusStartupException
                   (ConsensusStartupException (..))
import           Cardano.Node.Tracing.Tracers.Peer (PeerT)

data Tracers peer localPeer blk p2p = Tracers
  { -- | Trace the ChainDB
    chainDBTracer         :: ! (Tracer IO (ChainDB.TraceEvent blk))
    -- | Consensus-specific tracers.
  , consensusTracers      :: ! (Consensus.Tracers IO peer localPeer blk)
    -- | Tracers for the node-to-node protocols.
  , nodeToNodeTracers     :: ! (NodeToNode.Tracers IO peer blk DeserialiseFailure)
    --, serialisedBlockTracer :: NodeToNode.SerialisedTracer IO peer blk (SerialisedBlockTrace)
    -- | Tracers for the node-to-client protocols
  , nodeToClientTracers   :: ! (NodeToClient.Tracers IO localPeer blk DeserialiseFailure)
    -- | Diffusion tracers
  , diffusionTracers      :: ! (Diffusion.Tracers RemoteAddress NodeToNodeVersion
                                               LocalAddress  NodeToClientVersion
                                              IO)
  , diffusionTracersExtra :: ! (Diffusion.ExtraTracers p2p)

  , startupTracer         :: ! (Tracer IO (StartupTrace blk))
  , shutdownTracer        :: ! (Tracer IO ShutdownTrace)
  , nodeInfoTracer        :: ! (Tracer IO NodeInfo)
  , nodeStartupInfoTracer :: ! (Tracer IO NodeStartupInfo)
  , nodeStateTracer       :: ! (Tracer IO NodeState)
  , resourcesTracer       :: ! (Tracer IO ResourceStats)
  , peersTracer           :: ! (Tracer IO [PeerT blk])
  }
--   deriving (Generic, NFData)


-- deriving instance _ => Generic (Diffusion.Tracers RemoteAddress NodeToNodeVersion
--                                                     LocalAddress  NodeToClientVersion
--                                                     IO)
-- deriving instance _ => NFData (Diffusion.Tracers RemoteAddress NodeToNodeVersion
--                                                     LocalAddress  NodeToClientVersion
--                                                     IO)

-- deriving instance _ => Generic (Consensus.Tracers' peer localPeer blk (Tracer IO))
-- deriving instance _ => NFData (Consensus.Tracers' peer localPeer blk (Tracer IO))

-- deriving instance _ => Generic (NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO))
-- deriving instance _ => NFData (NodeToNode.Tracers' peer blk DeserialiseFailure (Tracer IO))

-- deriving instance _ => Generic (NodeToClient.Tracers' localPeer blk DeserialiseFailure (Tracer IO))
-- deriving instance _ => NFData (NodeToClient.Tracers' localPeer blk DeserialiseFailure (Tracer IO))

-- deriving instance _ => Generic (ChainDB.TraceEvent blk)
-- deriving instance _ => NFData (ChainDB.TraceEvent blk)

-- deriving instance _ => Generic (Diffusion.ExtraTracers p2p)
-- deriving instance _ => NFData (Diffusion.ExtraTracers p2p)

-- deriving instance _ => Generic (StartupTrace blk)
-- deriving instance _ => NFData (StartupTrace blk)

-- deriving instance _ => NFData (Resources Word64)
-- deriving instance _ => NFData (PeerT blk)
-- deriving instance _ => NFData (ConnectionId RemoteAddress)
-- deriving instance _ => Generic MuxTrace
-- deriving instance _ => NFData MuxTrace


-- deriving instance Generic a => Generic (Tracer IO a)
-- deriving instance NFData a => NFData (Tracer IO a)



-- deriving instance _ => Generic (HandshakeTr RemoteAddress NodeToNodeVersion)
-- deriving instance _ => NFData (HandshakeTr RemoteAddress NodeToNodeVersion)

-- deriving instance _ => NFData (WithMuxBearer (ConnectionId RemoteAddress)
--                                           MuxTrace)



