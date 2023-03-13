#!/usr/bin/env bash

set -euo pipefail

crate_name=ssz

if [ $# -ne 2 ]
then
    echo "Usage: ./$(basename $0) <from> <to>"
    exit 1
fi

from=$1
to=$2

# sanity check
grep "^version = \"$from\"" "$crate_name/Cargo.toml" > /dev/null || (echo "Wrong version: $from" && exit 1)

# update package versions
# these sed incantations are portable across BSD and GNU sed
sed -i.bak -e "s/^version = \"$from\"/version = \"$to\"/" "$crate_name/Cargo.toml"
sed -i.bak -e "s/^version = \"$from\"/version = \"$to\"/" "${crate_name}_derive/Cargo.toml"

# update main crate's dev dependency on derive crate (if any)
sed -i.bak -e "s/\(${crate_name}_derive.*version\) = \"$from\"/\1 = \"$to\"/" "$crate_name/Cargo.toml"

# update derive crate's dev dependency on main crate (if any)
sed -i.bak -e "s/\(${crate_name}.*version\) = \"$from\"/\1 = \"$to\"/" "${crate_name}_derive/Cargo.toml"

rm "$crate_name/Cargo.toml.bak"
rm "${crate_name}_derive/Cargo.toml.bak"
