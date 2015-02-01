#!/bin/bash

thisdir="$(dirname $0)"
cwd="$(pwd)"

targetdir="$thisdir/../src/gen/java/"
targetdir="$(echo $(cd $(dirname $targetdir); pwd)/$(basename $targetdir))"

cd "$thisdir"
echo "Exporting to $targetdir"
tools/protoc.exe *.proto "--java_out=$targetdir"
cd "$cwd"
