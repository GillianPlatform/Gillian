#!/usr/bin/env bash

if [[ "${GITHUB_ACTIONS}" ]]; then
	GILLIAN_C="gillian-c"
else
  GILLIAN_C="dune exec -- gillian-c"
fi

$GILLIAN_C bulk-exec concrete
