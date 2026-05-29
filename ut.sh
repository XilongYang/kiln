#!/usr/bin/env sh
set -eu

runghc -iSrc -i. Test/UT/RunTest.hs
