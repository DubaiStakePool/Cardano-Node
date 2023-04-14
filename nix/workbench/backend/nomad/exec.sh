usage_nomadexec() {
  # Using a unique help message for all Nomad "sub-backends"
  usage_nomad
}

backend_nomadexec() {

  op=${1:?$(usage_nomadexec)}; shift

  case "${op}" in

    name )
      # Can be:
      # nomadpodman       (Using podman task driver in the cloud is not planned)
      # nomadexec    (Starts Nomad agents supporting the nix_installable stanza)
      # nomadcloud  (IOG Nomad Agents and Amazon S3 with credentials from Vault)
      echo 'nomadexec'
    ;;

    # Generic backend sub-commands, shared code between Nomad sub-backends.

    describe-run )
      backend_nomad describe-run         "$@"
    ;;

    is-running )
      backend_nomad is-running           "$@"
    ;;

    start )
      backend_nomad start                "$@"
    ;;

    cleanup-cluster )
      backend_nomad cleanup-cluster      "$@"
    ;;

    start-nodes )
      backend_nomad start-nodes          "$@"
    ;;

    start-node )
      backend_nomad start-node           "$@"
    ;;

    stop-node )
      backend_nomad stop-node            "$@"
    ;;

    start-generator )
      backend_nomad start-generator      "$@"
    ;;

    get-node-socket-path )
      backend_nomad get-node-socket-path "$@"
    ;;

    wait-node )
      backend_nomad wait-node            "$@"
    ;;

    wait-node-stopped )
      backend_nomad wait-node-stopped    "$@"
    ;;

    wait-pools-stopped )
      backend_nomad wait-pools-stopped   "$@"
    ;;

    stop-cluster )
      backend_nomad stop-cluster         "$@"
    ;;

    cleanup-cluster )
      backend_nomad cleanup-cluster      "$@"
    ;;

    # Sets jq envars "profile_container_specs_file" ,"nomad_environment",
    # "nomad_task_driver" and "one_tracer_per_node"
    setenv-defaults )
      local usage="USAGE: wb backend $op BACKEND-DIR"
      local backend_dir=${1:?$usage}; shift

      setenvjqstr 'nomad_task_driver'   "exec"
      setenvjqstr 'nomad_server_name'   "srv1"
      # As one task driver runs as a normal user and the other as a root, use
      # different names to allow restarting/reusing without cleaup, this way
      # data folders already there can be accessed without "permission denied"
      # errors.
      setenvjqstr 'nomad_client_name'   "cli1-exe"

      # Store the location of the Nix-built "container-specs" file.
      # TODO/FIXME: This is the only way to be able to later copy it to "$dir" ?
      local profile_container_specs_file
      profile_container_specs_file="${backend_dir}"/container-specs.json
      setenvjqstr 'profile_container_specs_file' "${profile_container_specs_file}"
      setenvjqstr 'nomad_environment'   "local"
      setenvjqstr 'one_tracer_per_node' "true"
    ;;

    allocate-run )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift

      # Copy the container specs file (container-specs.json)
      # This is the output file of the Nix derivation
      local profile_container_specs_file=$(envjqr 'profile_container_specs_file')
      # Create a nicely sorted and indented copy
      jq . "${profile_container_specs_file}" > "${dir}"/container-specs.json

      # Create nomad folder and copy the Nomad job spec file to run.
      mkdir -p "${dir}"/nomad
      # Select which version of the Nomad job spec file we are running and
      # create a nicely sorted and indented copy it "nomad/nomad-job.json".
      jq -r ".nomadJob.exec.oneTracerPerNode"      \
        "${dir}"/container-specs.json              \
      > "${dir}"/nomad/nomad-job.json
      # The job file is "slightly" modified (jq) to suit the running environment.
      backend_nomad allocate-run-nomad-job-patch-namespace "${dir}" "default"
      backend_nomad allocate-run-nomad-job-patch-nix       "${dir}"

      backend_nomad allocate-run "${dir}"
    ;;

    deploy-genesis )
      local usage="USAGE: wb backend $op RUN-DIR"
      local dir=${1:?$usage}; shift
      local nomad_job_name=$(jq -r ". [\"job\"] | keys[0]" "${dir}"/nomad/nomad-job.json)
      local server_name=$(envjqr 'nomad_server_name')
      local client_name=$(envjqr 'nomad_client_name')

      # Add genesis to HTTP cache server
      local nomad_agents_were_already_running=$(envjqr 'nomad_agents_were_already_running')
      if ! wb_nomad webfs is-running
      then
        if ! wb_nomad webfs start
        then
          if test "${nomad_agents_were_already_running}" = "false"
          then
            msg "Startup of webfs failed, cleaning up ..."
            wb_nomad agents stop "${server_name}" "${client_name}" "exec"
            backend_nomad stop-nomad-job "${dir}"
          fi
          fatal "Failed to start HTTP server"
        fi
      fi
      if ! wb_nomad webfs add-genesis-dir "${dir}"/genesis "${nomad_job_name}"
      then
        if test "${nomad_agents_were_already_running}" = "false"
        then
          msg "Startup of webfs failed, cleaning up ..."
          wb_nomad agents stop "${server_name}" "${client_name}" "exec"
          backend_nomad stop-nomad-job "${dir}"
        fi
        fatal "Failed to add genesis to HTTP server"
      fi
      backend_nomad deploy-genesis-wget "${dir}" \
        "http://127.0.0.1:12000/${nomad_job_name}.tar.zst"
    ;;

    * )
      # TODO: Replace with `usage_nomadexec` and make the nomad helper commands
      # use a new top level sub-command `wb nomad`
      backend_nomad "${op}" "$@"
    ;;

  esac

}
