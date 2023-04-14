# our packages overlay
final: prev:

let
  inherit (prev) customConfig;
  inherit (final) pkgs cardanoNodePackages;
  inherit (prev.pkgs) lib;

  # A generic, fully parameteric version of the workbench development environment.
  workbench = pkgs.callPackage ./workbench {};

  # A conveniently-parametrisable workbench preset.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # The general idea is:
  # 1. backendName -> stateDir -> basePort -> useCabalRun -> backend
  # 2. batchName -> profileName -> backend -> workbench -> runner
  # * `workbench` is in case a pinned version of the workbench is needed.
  workbench-runner =
  let
    backendRegistry =
      {
        nixops          = params:
          import ./workbench/backend/nixops.nix       params;
        nomadcloud      = params:
          import ./workbench/backend/nomad/cloud.nix  params;
        nomadexec       = params:
          import ./workbench/backend/nomad/exec.nix   params;
        nomadpodman     = params:
          import ./workbench/backend/nomad/podman.nix params;
        supervisor      = params:
          import ./workbench/backend/supervisor.nix   params;
      }
    ;
  in
    { stateDir           ? customConfig.localCluster.stateDir
    , batchName          ? customConfig.localCluster.batchName
    , profileNix         ? null
    , profileName        ? if profileNix != null then profileNix.profileName
                           else customConfig.localCluster.profileName
    , backendName        ? customConfig.localCluster.backendName
    , basePort           ? customConfig.localCluster.basePort
    , useCabalRun        ? customConfig.localCluster.useCabalRun
    , workbenchDevMode   ? customConfig.localCluster.workbenchDevMode
    , profiling          ? customConfig.profiling
    , cardano-node-rev   ? null
    , workbench          ? pkgs.workbench
    }:
    let
        # The `useCabalRun` flag is set in the backend to allow the backend to
        # override its value. The runner uses the value of `useCabalRun` from
        # the backend to prevent a runner using a different value.
        backend = (backendRegistry."${backendName}")
                   { inherit pkgs lib stateDir basePort useCabalRun; };
    in import ./workbench/backend/runner.nix
      {
        inherit pkgs lib cardanoNodePackages;
        inherit batchName profileName backend;
        inherit cardano-node-rev;
        inherit workbench workbenchDevMode profiling;
      };

  # Workbench instantiated by parameters from customConfig:
  custom-config-workbench-runner = workbench-runner {};

in with final;
{
  inherit (cardanoNodeProject.args) compiler-nix-name;
  inherit workbench workbench-runner;

  cabal = haskell-nix.cabal-install.${compiler-nix-name};

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = {ghc8107 = "3.4.1";}.${compiler-nix-name} or "3.5";
    index-state = "2023-01-20T05:50:56Z";
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    index-state = "2023-01-20T05:50:56Z";
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" rec {
    src = haskell-nix.sources."hls-1.10";
    cabalProject = builtins.readFile (src + "/cabal.project");
    sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    index-state = "2023-01-20T05:50:56Z";
  };

  cardanolib-py = callPackage ./cardanolib-py { };

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  clusterTests = import ./workbench/tests { inherit pkgs; };

  plutus-scripts = callPackage ./plutus-scripts.nix { plutus-builder = plutus-example; };

  dockerImage =
    let
      defaultConfig = {
        stateDir = "/data";
        dbPrefix = "db";
        socketPath = "/ipc/node.socket";
      };
    in
    callPackage ./docker {
      exe = "cardano-node";
      scripts = import ./scripts.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "node";
    };

  submitApiDockerImage =
    let
      defaultConfig = {
        socketPath = "/node-ipc/node.socket";
        listenAddress = "0.0.0.0";
      };
    in
    callPackage ./docker/submit-api.nix {
      exe = "cardano-submit-api";
      scripts = import ./scripts-submit-api.nix {
        inherit pkgs;
        customConfigs = [ defaultConfig customConfig ];
      };
      script = "submit-api";
    };

  all-profiles-json = workbench.profile-names-json;

  # Disable failing python uvloop tests
  python38 = prev.python38.override {
    packageOverrides = pythonFinal: pythonPrev: {
      uvloop = pythonPrev.uvloop.overrideAttrs (attrs: {
        disabledTestPaths = [ "tests/test_tcp.py" "tests/test_sourcecode.py" "tests/test_dns.py" ];
      });
    };
  };
} //
custom-config-workbench-runner.overlay final prev
