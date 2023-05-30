module Cardano.Node.Tracing.DefaultTraceConfig
  ( defaultCardanoConfig
  ) where

import qualified Data.Map.Strict as Map
import           Prelude

import           Cardano.Logging


defaultCardanoConfig :: TraceConfig
defaultCardanoConfig = emptyTraceConfig {
  tcOptions = Map.fromList
    [([],
         [ ConfSeverity (SeverityF (Just Notice)) -- Means Silence
         , ConfDetail DNormal
         , ConfBackend  [Stdout MachineFormat
                        , EKGBackend
                        , Forwarder
                        ]])

-- more important tracers going here
    ,(["BlockFetch", "Decision"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["ChainDB"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["ChainSync", "Client"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["Net", "ConnectionManager", "Remote"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["DNSSubscription"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["DiffusionInit"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["ErrorPolicy"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["Forge"],
         [ ConfSeverity (SeverityF (Just Info))])
        -- includes ["Forge", "KESInfo"]
    ,(["Net", "InboundGovernor", "Remote"],
         [ ConfSeverity (SeverityF (Just Info))])
        -- includes ["Net", "InboundGovernor", "Remote", "Transition"]
    ,(["IpSubscription"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["LocalErrorPolicy"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["Mempool"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["Net", "Mux", "Remote"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["Net", "PeerSelection"],
         [ ConfSeverity (SeverityF (Just Info))])
    ,(["Resources"],
         [ ConfSeverity (SeverityF (Just Info))])

-- Limiters
    ,(["ChainDB","AddBlockEvent","AddedBlockToQueue"],
         [ ConfLimiter 2.0])
    ,(["ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],
         [ ConfLimiter 2.0])
    ,(["ChainDB","AddBlockEvent","AddBlockValidation", "ValidCandidate"],
         [ ConfLimiter 2.0])
    ,(["ChainDB", "CopyToImmutableDBEvent", "CopiedBlockToImmutableDB"],
         [ ConfLimiter 2.0])
    ,(["ChainSync","Client","DownloadedHeader"],
         [ ConfLimiter 2.0])
    ,(["BlockFetch", "Client", "CompletedBlockFetch"],
        [ ConfLimiter 2.0])
    ]
  }
