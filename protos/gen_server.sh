#!/bin/bash

thisdir="$(dirname $0)"
cwd="$(pwd)"

targetdir="$thisdir/../src/gen/java/"
targetdir="$(echo $(cd $(dirname $targetdir); pwd)/$(basename $targetdir))"

protoc="tools/protoc-2.6.1.exe"

cd "$thisdir"
echo -n "Exporting to $targetdir."
echo -n " Java..."
$protoc *.proto "--java_out=$targetdir"
echo -n " Scala..."
$protoc --plugin=protoc-gen-scala=tools/scalapb-0.4.9.exe *.proto --scala_out="java_conversions:d:\\work\\scala\\tbsgame\\src\\gen\\scala"
echo " Done!"
cd "$cwd"
