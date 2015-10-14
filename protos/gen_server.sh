#!/bin/bash

thisdir="$(cd "$(dirname "$0")"; pwd)"
cwd="$(pwd)"

targetdir="$thisdir/../src/gen/scala/"
targetdir="$(echo $(cd $(dirname $targetdir); pwd)/$(basename $targetdir))"

protoc="$thisdir/tools/protoc.exe"

echo "Exporting to $targetdir."
rm -rf "$targetdir"/*
for dir in game control; do
  cd "$thisdir/$dir"
  echo "Exporting in $dir"
  $protoc "--plugin=protoc-gen-scala=$thisdir/tools/scalapb-0.4.9.exe" \
    *.proto --scala_out="$targetdir"
done
cd "$cwd"
