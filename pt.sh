#!/usr/bin/env sh
set -eu

PT_ULIMIT_VMEM_KB="${PT_ULIMIT_VMEM_KB:-5242880}"

sh Test/PT/scripts/prepare-perf-data.sh
ulimit -Sv "$PT_ULIMIT_VMEM_KB"
runghc -iSrc -i. Test/PT/RunPerf.hs
