#!/bin/bash

set -e
cd "`dirname "$0"`"
cabal-dev build
dist/build/bneijt.nl/bneijt.nl clean

#Recompress all files
find static -name '*.gz' | \
while read OUTFILE; do
    SRCFILE="`dirname "${OUTFILE}"`/`basename "${OUTFILE}" .gz`"
    echo "${SRCFILE}"
    if [ -f "${SRCFILE}" ]; then
        gzip --best -c "${SRCFILE}" > "${OUTFILE}"
    fi
done

dist/build/bneijt.nl/bneijt.nl build

