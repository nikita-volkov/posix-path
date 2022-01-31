#!/bin/bash
set -eo pipefail

ormolu --mode inplace -ce \
$(find . -name "*.hs" \
  -not -path "./*.stack-work/*" \
  -not -path "./.git/*")

stack build --fast

./.stack-work/dist/x86_64-osx/Cabal-3.4.1.0/build/doctest/doctest
