#!/bin/bash
set -e
rm hakyll
cd "`dirname "$0"`"
if [ ! -x hakyll ]; then
    echo "Hakyll binary missing, trying to build it"
    ghc --make hakyll.hs
    rm hakyll.o
    rm hakyll.hi
fi
./hakyll clean
./hakyll build

