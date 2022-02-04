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
}

function build_and_test_with_doctest {
  stack build \
  --test \
  --fast \
  --flag coalmine:doctest
}

function build {
  stack build \
  --fast
}

format
build_and_test
