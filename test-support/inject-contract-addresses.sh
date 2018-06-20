#!/usr/bin/env bash

set -ex

EXPORT_STORE=${EXPORT_STORE:-/tmp/detected-vars}
SEARCH_DIRECTORY=${SEARCH_DIRECTORY:-build/contracts}

function detect_contract_addresses() {
  for file in $(find $SEARCH_DIRECTORY -maxdepth 1 -type f -name '*.json'); do
    maybe_address=`jq -r .networks[].address $file | grep -v null | head -n 1`
    if [ -z "${maybe_address}" ]; then
      echo no addresses detected for $file >&2
    else
      contract_name=`echo $file | sed -e 's@'$SEARCH_DIRECTORY'/@@g' -e 's/.json//g'`
      echo found address "${maybe_address}" for "${contract_name}" >&2
      contract_var=`echo $contract_name | tr '[:lower:]' '[:upper:]'`_CONTRACT_ADDRESS
      echo the var for "${contract_name}" is "${contract_var}" >&2
      echo "export ${contract_var}=`echo ${maybe_address} | sed s/0x//`"
    fi
  done
}

detect_contract_addresses 2>/dev/null >$EXPORT_STORE

source $EXPORT_STORE
$@
