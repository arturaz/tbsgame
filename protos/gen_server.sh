#!/bin/bash

thisdir="$(dirname $0)"
cwd="$(pwd)"

targetdir="$thisdir/../src/gen/scala/"
targetdir="$(echo $(cd $(dirname $targetdir); pwd)/$(basename $targetdir))"

protoc="tools/protoc.exe"

cd "$thisdir"
echo -n "Exporting to $targetdir."
#echo -n " Java..."
#$protoc *.proto "--java_out=$targetdir"
echo -n " Scala..."
$protoc --plugin=protoc-gen-scala=tools/scalapb-0.4.9.exe *.proto --scala_out="$targetdir"
echo " Done!"
cd "$cwd"
