#!/bin/bash
# Professor's model for one pair, end to end: both fits, then the leave-one-region-out
# tests (resumable; two runs per model share the regions from both ends).
#   bash scripts/pest2_all.sh ips_typ pit_spp fir
set -u
P1=$1; P2=$2; H=$3
Rscript scripts/pest2_run.R $P1 $P2 $H --no-loro
Rscript scripts/pest2_run_hurdle.R $P1 $P2 $H --no-loro
Rscript scripts/pest2_loro.R $P1 $P2 $H gaussian & Rscript scripts/pest2_loro.R $P1 $P2 $H gaussian --reverse & wait
Rscript scripts/pest2_loro.R $P1 $P2 $H hurdle & Rscript scripts/pest2_loro.R $P1 $P2 $H hurdle --reverse & wait
echo PEST2_ALL_DONE
