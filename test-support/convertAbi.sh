#!/bin/sh

cd build/contracts/
mkdir -p abis

for file in *.json; do
  jq '.abi' < "$file" > "abis/$file"
done

