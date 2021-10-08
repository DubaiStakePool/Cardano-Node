{ runCommand, cardano-node, jq, snapshot, vmTools, strace, util-linux, e2fsprogs, gnugrep, procps, time }:

let
  params = builtins.fromJSON (builtins.readFile ./membench_params.json);
  topology = { Producers = []; };
  flags = params.rtsFlags;
  topologyPath = builtins.toFile "topology.json" (builtins.toJSON topology);
  passMem = (builtins.fromJSON (builtins.readFile ./pass.json)).memSize;
  failMem = (builtins.fromJSON (builtins.readFile ./fail.json)).memSize;
  avgMem = (passMem+failMem) / 2;
  membench = vmTools.runInLinuxVM (runCommand "membench" {
    memSize = if (params.memSize == "auto") then avgMem else params.memSize;
    buildInputs = [ cardano-node jq strace util-linux e2fsprogs procps time ];
    succeedOnFailure = true;
    preVM = ''
      truncate disk.img --size 2G
      export diskImage=$(realpath disk.img)
    '';
    failureHook = ''
      pwd
      ls -ltrh
    '';
    postVM = ''
      echo postvm
      pwd
      ls -ltrh xchg/
      mv -vi xchg/*.json $out/ || true

      cd $out
      ${gnugrep}/bin/egrep 'ReplayFromSnapshot|ReplayedBlock|will terminate|Ringing the node shutdown|TookSnapshot|cardano.node.resources' log.json > $out/summary.json
    '';
  } ''
    echo 0 > /tmp/xchg/in-vm-exit
    mkdir -pv $out/nix-support
    echo 42 > $out/nix-support/failed

    # never overcommit
    echo 2 > /proc/sys/vm/overcommit_memory

    pwd
    free -m
    mkfs.ext4 /dev/vda
    mkdir /state
    mount /dev/vda /state
    cp -r ${snapshot}/chain /state/chain
    chmod -R +w /state/chain

    cd /tmp/xchg

    ls -ltrh /state/chain
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
    command time -f %M -o $out/highwater cardano-node ${flags} run --database-path /state/chain/ --config config.json --topology ${topologyPath} --shutdown-on-slot-synced 2000
    #sleep 600
    #kill -int $!
    pwd
    df -h
    free -m
    ls -ltrh /state/chain/ledger/
    mv -vi log*json config.json $out/
    mv /state/chain $out/
    rm $out/nix-support/failed
  '');
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
  highwater=$(cat ${membench}/highwater)

  if [ -f ${membench}/nix-support/failed ]; then
    export FAILED=true
    mkdir $out/nix-support -p
    cp ${membench}/nix-support/failed $out/nix-support/failed
  else
    export FAILED=false
  fi

  jq --slurp < summary.json 'def minavgmax: length as $len | { min: (min/1024/1024), avg: ((add / $len)/1024/1024), max: (max/1024/1024) }; map(select(.ns[0] == "cardano.node.resources") | .data) | { RSS: map(.RSS) | minavgmax, Heap: map(.Heap) | minavgmax, CentiCpuMax: map(.CentiCpu) | max, CentiMutMax: map(.CentiMut) | max, CentiGC: map(.CentiGC) | max, CentiBlkIO: map(.CentiBlkIO) | max, flags: "${flags}", chain: { startSlot: ${toString snapshot.snapshotSlot}, stopFile: ${toString snapshot.finalEpoch} }, totaltime:'$totaltime', failed:'$FAILED', memSize: ${toString membench.memSize}, highwaterMB: '$highwater'/1024 }' > refined.json
''
