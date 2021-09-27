{ runCommand, db-analyser, mainnet-chain, jq }:

runCommand "snapshot-generation" {
  buildInputs = [ db-analyser jq ];
} ''
  db-analyser --help
  pwd
  cp -rs ${mainnet-chain} chain
  chmod +w chain chain/immutable
  rm chain/immutable/01861.*
  cp ${mainnet-chain}/immutable/01861.* chain/immutable -vi
  chmod +w chain/immutable/01861.*

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
  set -x

  db-analyser --db chain/ cardano --configByron config.json --configShelley mainnet-shelley-genesis.json --nonce 1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81 --configAlonzo mainnet-alonzo-genesis.json --store-ledger 1234
''
