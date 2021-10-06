{ runCommand, cardano-node, jq, snapshot }:

let
  topology = { Producers = []; };
  flags = "+RTS -RTS";
  membench = runCommand "membench" {
    buildInputs = [ cardano-node jq ];
    topology = builtins.toJSON topology;
    passAsFile = [ "topology" ];
  } ''
    pwd
    cp -r ${snapshot}/chain chain
    chmod -Rv +w chain

    ls -ltrh chain
    jq '.setupScribes = [
        .setupScribes[0] * { "scFormat":"ScJson" },
        {
          scFormat:"ScJson",
          scKind:"FileSK",
          scName:"log.json",
          scRotation:{
            rpLogLimitBytes: 300000000,
            rpMaxAgeHours:   24,
            rpKeepFilesNum:  20
          }
        }
      ]
      | .defaultScribes = .defaultScribes + [ [ "FileSK", "log.json" ] ]
      ' ${./configuration/cardano/mainnet-config.json} > config.json
    cp -v ${./configuration/cardano}/*-genesis.json .
    cardano-node ${flags} run --database-path chain/ --config config.json --topology $topologyPath --shutdown-on-slot-synced 2000
    #sleep 600
    #kill -int $!
    pwd
    ls -ltrh chain/ledger/
    mkdir $out
    egrep 'ReplayFromSnapshot|ReplayedBlock|will terminate|Ringing the node shutdown|TookSnapshot|cardano.node.resources' log.json > $out/summary.json
    mv -vi log*json config.json $out/
    mv chain $out/
  '';
in
runCommand "membench-post-process" {
  buildInputs = [ jq ];
} ''
  cp -r ${membench} $out
  chmod -R +w $out
  cd $out
  # so the node wont get GC'd, and you could confirm the source it came from
  ln -s ${cardano-node}/bin/cardano-node .
  totaltime=$({ head -n1 log.json ; tail -n1 log.json;} | jq --slurp 'def katip_timestamp_to_iso8601: .[:-4] + "Z" | fromdateiso8601; map(.at | katip_timestamp_to_iso8601) | .[1] - .[0]')

  jq --slurp < summary.json 'def minavgmax: length as $len | { min: (min/1024), avg: ((add / $len)/1024), max: (max/1024) }; map(select(.ns[0] == "cardano.node.resources") | .data) | { RSS: map(.RSS) | minavgmax, Heap: map(.Heap) | minavgmax, CentiCpuMax: map(.CentiCpu) | max, CentiMutMax: map(.CentiMut) | max, CentiGC: map(.CentiGC) | max, CentiBlkIO: map(.CentiBlkIO) | max, flags: "${flags}", chain: { startSlot: ${toString snapshot.snapshotSlot}, stopFile: ${toString snapshot.finalEpoch} }, totaltime:'$totaltime' }' > refined.json
''
