#!/bin/bash
set -e
cd "`dirname "$0"`"
cabal-dev build
dist/build/bneijt.nl/bneijt.nl clean
dist/build/bneijt.nl/bneijt.nl build

