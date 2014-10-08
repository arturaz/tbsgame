#!/bin/bash

thisdir="$(dirname $0)"
cwd="$(pwd)"

cd "$thisdir"
protoc.exe *.proto --java_out=../src/gen/java/
cd "$cwd"