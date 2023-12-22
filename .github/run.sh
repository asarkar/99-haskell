#!/bin/bash

set -e

no_test=0
no_lint=0
stack_opts=""

while (( $# > 0 )); do
   case "$1" in
   	--help)
			printf "run.sh [OPTION]... [DIR]\n"
			printf "options:\n"
			printf "\t--help			Show help\n"
			printf "\t--no-test		Skip tests\n"
			printf "\t--no-lint		Skip linting\n"
			exit 0
      	;;
      --no-test)
			no_test=1
			shift
      	;;
      --no-lint)
			no_lint=1
			shift
			;;
		--profile)
			stack_opts="--profile --force-dirty $stack_opts"
			shift
			;;
		*)
			break
	      ;;
   esac
done

args=( "${@/#/\"}" )
args=( "${args[@]/%/\"}" )

if (( no_test == 0 )); then
		# profiling https://stackoverflow.com/a/40922201/839733
		stack test $stack_opts --ta "${args[*]}"
fi

if (( no_lint == 0 )); then
	if [[ -x "$(command -v hlint)" ]]; then
		hlint .
	else
		printf "hlint not found"
	fi

	ormolu_mode="check"
	if [[ "$OSTYPE" == "darwin"* ]]; then
		ormolu_mode="inplace"
	fi
	
	if [[ -x "$(command -v ormolu)" ]]; then
		ormolu -m "$ormolu_mode" $(find . -type f -name '*.hs')
	else
		printf "ormolu not found"
	fi
fi
