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
    # Try GHCup first.
    stack_path="$HOME/.ghcup/bin/stack"
    if [[ ! -x "$(command -v "$stack_path")" ]]; then
	  stack_path=stack
	fi
	  
	# profiling https://stackoverflow.com/a/40922201/839733
	"$stack_path" test $stack_opts --ta "${args[*]}"
fi

if (( no_lint == 0 )); then
	red=$(tput -Txterm-256color setaf 1)
	default=$(tput -Txterm-256color sgr0)

	if [[ -x "$(command -v hlint)" ]]; then
		hlint .
	else
		printf "%bhlint not found%b\n" "$red" "$default"
	fi

	ormolu_mode="check"
	if [[ -z "$CI" ]]; then
		ormolu_mode="inplace"
	fi
	
	if [[ -x "$(command -v ormolu)" ]]; then
		ormolu -m "$ormolu_mode" $(find . -type f -name '*.hs')
	else
		printf "%bormolu not found%b\n" "$red" "$default"
	fi
fi
