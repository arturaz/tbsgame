#!/bin/bash

thisdir="$(dirname $0)"
cwd="$(pwd)"

cd "$thisdir"
tools/protoc.exe *.proto --java_out=../src/gen/java/
cd "$cwd"
