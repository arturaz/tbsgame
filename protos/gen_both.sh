#!/bin/bash

thisdir="$(dirname $0)"

echo "Generating server"
"$thisdir/gen_server.sh" $@
echo "Generating client"
"$thisdir/gen_client.sh" $@