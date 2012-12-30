#!/bin/bash
set -e
cd "`dirname "$0"`"
if [ ! -x hakyll ]; then
    echo "Hakyll binary missing, trying to build it"
    ghc --make hakyll.hs
    rm hakyll.o
    rm hakyll.hi
fi
./hakyll build
rsync --delete --recursive --progress _site/ logfish.net:/home/bram/vhost/bneijt.nl/_/blog
