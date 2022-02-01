#!/bin/bash
set -eo pipefail

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./*.stack-work/*" \
    -not -path "./.git/*")
}

function build_and_test {
  stack build \
  --test \
  --fast

  ./.stack-work/dist/x86_64-osx/Cabal-3.4.1.0/build/doctest/doctest
}

function build {
  stack build \
  --fast
}

format
build_and_test
