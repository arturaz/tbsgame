#!/bin/bash

client_dir="$1"

if [ "$client_dir" == "" ]; then
  echo "Usage: $0 client_root_dir"
  exit 1
fi

targetdir="$client_dir/code/game-general/Networking/Messages/"
targetdir="$(echo $(cd $(dirname $targetdir); pwd)/$(basename $targetdir))"
thisdir="$(dirname $0)"
cwd="$(pwd)"

cd "$thisdir/game"
echo "Exporting to $targetdir"
mkdir -p "$targetdir"
rm -rf "$targetdir"/*.cs
../tools/ProtoGen.exe *.proto -namespace="game_general.Networking.Messages" -output_directory="$targetdir" -nest_classes=true
cd "$cwd"
