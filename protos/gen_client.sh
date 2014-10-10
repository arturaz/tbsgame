#!/bin/bash

client_dir="$1"

if [ "$client_dir" == "" ]; then
  echo "Usage: $0 client_root_dir"
  exit 1
fi

targetdir="$client_dir/unity/Assets/gen/netmsg/"
thisdir="$(dirname $0)"
cwd="$(pwd)"

cd "$thisdir"
mkdir -p "$targetdir"
tools/ProtoGen.exe *.proto -namespace=netmsg -output_directory="$targetdir" -nest_classes=true
cd "$cwd"
