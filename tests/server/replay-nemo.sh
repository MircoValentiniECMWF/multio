#!/bin/bash

client_binary=${1:-multio-replay}
server_binary=${2:-multio-probe}

function launch_mpi {
    local cmd="mpiexec --oversubscribe -np 7 $client_binary : -np 1 $server_binary --test"
    eval $cmd
}

launch_mpi
