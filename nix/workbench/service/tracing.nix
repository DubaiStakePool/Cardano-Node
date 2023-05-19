{ lib
, nodeSpec
, tracer
, tracing_backend
}:
cfg:

with lib;
let
  trace-dispatcher =
    recursiveUpdate
    (removeLegacyTracingOptions cfg)
  {
    UseTraceDispatcher   = true;

  ## Please see the generated tracing configuration reference at:
  ##
  ## https://github.com/input-output-hk/cardano-node/blob/master/doc/new-tracing/tracers_doc_generated.md#trace-messages
  ##
    TraceOptions  = {
      "" =
        { severity = "Notice";
          backends = [
            "Stdout MachineFormat"
            "EKGBackend"
            ] ++ optional tracer
              "Forwarder";
        };

      ## These are comparision-specific config deviations from the default.
      ##
      ## "Resources".backends = ["EKGBackend"];
      "Net.Net.AcceptPolicy".severity = "Info";
      "ChainDB".severity = "Info";
      "Net.ConnectionManager.Local".severity = "Info";
      "Net.ConnectionManager.Remote".severity = "Info";
      "Net.DNSResolver".severity = "Info";
      "Net.Subscription.DNS".severity = "Info";
      "Net.Startup.DiffusionInit".severity = "Info";
      "Net.ErrorPolicy.Remote".severity = "Info";
      "Forge.Loop".severity = "Info";
      "Net.Subscription.IP".severity = "Info";
      "Net.InboundGovernor".severity = "Info";
      "Net.ErrorPolicy.Local".severity = "Info";
      "Net.Peers".severity = "Info";
      "Mempool".severity = "Info";
      "Net.PeerSelection".severity = "Info";
      "Net.Peers.PublicRoot".severity = "Info";
      "Net.Server".severity = "Info";

      "BlockFetch.Client.CompletedBlockFetch".severity = "Debug";
      "BlockFetch.Client.SendFetchRequest".severity = "Debug";
      "BlockFetch.Server.SendBlock".severity = "Debug";
      "ChainDB.AddBlockEvent.AddedToCurrentChain".severity = "Debug";
      "ChainDB.LedgerEvent.TookSnapshot".severity = "Debug";
      "ChainSync.Client.DownloadedHeader".severity = "Debug";
      "ChainSync.ServerHeader.Update".severity = "Debug";
      "Forge.Loop.AdoptedBlock".severity = "Debug";
      "Forge.Loop.BlockContext".severity = "Debug";
      "Forge.Loop.ForgedBlock".severity = "Debug";
      "Forge.Loop.LedgerState".severity = "Debug";
      "Forge.Loop.LedgerView".severity = "Debug";
      "Forge.Loop.MempoolSnapshot".severity = "Debug";
      "Forge.Loop.NodeIsLeader".severity = "Debug";
      "Forge.Loop.NodeNotLeader".severity = "Debug";
      "Forge.Loop.StartLeadershipCheck".severity = "Debug";
      "Forge.Loop.TickedLedgerState".severity = "Debug";
      "Mempool.AddedTx".severity = "Debug";
      "Mempool.RejectedTx".severity = "Debug";
      "Mempool.RemoveTxs".severity = "Debug";
      "TraceBenchTxSubServAck".severity = "Debug";
      "TraceBenchTxSubSummary".severity = "Debug";
      "TraceTxSubmissionCollected".severity = "Debug";
      "TraceTxSubmissionProcessed".severity = "Debug";
    };
  };

  iohk-monitoring =
    removeAttrs cfg
      [ "setupScribes" ]
  {
    defaultScribes = [
      [ "StdoutSK" "stdout" ]
    ];
    setupScribes =
      [{
        scKind   = "StdoutSK";
        scName   = "stdout";
        scFormat = "ScJson";
      }];
    minSeverity                 = "Debug";
    TraceMempool                = true;
    TraceTxInbound              = true;
    TraceBlockFetchClient       = true;
    TraceBlockFetchServer       = true;
    TraceChainSyncHeaderServer  = true;
    TraceChainSyncClient        = true;
    options = {
      mapBackends = {
        "cardano.node.resources" = [ "KatipBK" ];
      };
    };
  };

  ##
  ## removeLegacyTracingOptions :: NodeConfig -> NodeConfig
  ##
  removeLegacyTracingOptions = cfg:
    removeAttrs cfg
    [
      "TraceAcceptPolicy"
      "TraceBlockchainTime"
      "TraceBlockFetchClient"
      "TraceBlockFetchDecisions"
      "TraceBlockFetchProtocol"
      "TraceBlockFetchProtocolSerialised"
      "TraceBlockFetchServer"
      "TraceChainDb"
      "TraceChainSyncClient"
      "TraceChainSyncBlockServer"
      "TraceChainSyncHeaderServer"
      "TraceChainSyncProtocol"
      "TraceConnectionManager"
      "TraceConnectionManagerCounters"
      "TraceConnectionManagerTransitions"
      "DebugPeerSelectionInitiator"
      "DebugPeerSelectionInitiatorResponder"
      "TraceDiffusionInitialization"
      "TraceDnsResolver"
      "TraceDnsSubscription"
      "TraceErrorPolicy"
      "TraceForge"
      "TraceForgeStateInfo"
      "TraceHandshake"
      "TraceIpSubscription"
      "TraceKeepAliveClient"
      "TraceLedgerPeers"
      "TraceLocalChainSyncProtocol"
      "TraceLocalConnectionManager"
      "TraceLocalErrorPolicy"
      "TraceLocalHandshake"
      "TraceLocalInboundGovernor"
      "TraceLocalRootPeers"
      "TraceLocalServer"
      "TraceLocalStateQueryProtocol"
      "TraceLocalTxMonitorProtocol"
      "TraceLocalTxSubmissionProtocol"
      "TraceLocalTxSubmissionServer"
      "TraceMempool"
      "TraceMux"
      "TraceLocalMux"
      "TracePeerSelection"
      "TracePeerSelectionCounters"
      "TracePeerSelectionActions"
      "TracePublicRootPeers"
      "TraceServer"
      "TraceInboundGovernor"
      "TraceInboundGovernorCounters"
      "TraceInboundGovernorTransitions"
      "TraceTxInbound"
      "TraceTxOutbound"
      "TraceTxSubmissionProtocol"
      "TraceTxSubmission2Protocol"
      "TracingVerbosity"
      "defaultBackends"
      "defaultScribes"
      "hasEKG"
      "hasPrometheus"
      "minSeverity"
      "options"
      "rotation"
      "setupBackends"
      "setupScribes"
    ];
in
{
  inherit trace-dispatcher iohk-monitoring;
}.${tracing_backend}
