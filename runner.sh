#!/bin/bash

basedir=$( cd "$(dirname "$0")" ; pwd -P )
mkdir output

args=(--agb_path "${1}")
args+=(--lc_path "${2}")
args+=(--boreal_vector_path "${3}")
args+=(--predict_var "${4}")
args+=(--tile_num "${5}")
args+=(--year "${6}")

source activate r

command=(Rscript "${basedir}/boreal_summary.R" "${args[@]}")

echo "${command[@]}"
"${command[@]}"
