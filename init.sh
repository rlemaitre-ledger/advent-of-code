#!/usr/bin/env bash

#########################################################################################
#                                                                                       #
#  Bash script template from https://betterdev.blog/minimal-safe-bash-script-template/  #
#                                                                                       #
#########################################################################################
set -Eeuo pipefail
trap cleanup SIGINT SIGTERM ERR EXIT

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd -P)

usage() {
  cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") [-h] [-v] [--year year] [--day day] [--token token]
Initializes class and test for an Advent of Code Problem. It also retrieve the input data
Available options:
-h, --help      Print this help and exit
-v, --verbose   Print script debug info
-y, --year      The year of advent of code problem. Default is the current year
-d, --day       The day. Default is the current day number
-t, --token     Advent of code cookie token.
                  If not present, it reads from .token file in the current directory
--no-color      Do not use color in output messages
EOF
  exit
}

cleanup() {
  trap - SIGINT SIGTERM ERR EXIT
  # script cleanup here
}

setup_colors() {
  if [[ -t 2 ]] && [[ -z "${NO_COLOR-}" ]] && [[ "${TERM-}" != "dumb" ]]; then
    NOFORMAT='\033[0m' RED='\033[0;31m' GREEN='\033[0;32m' ORANGE='\033[0;33m' BLUE='\033[0;34m' PURPLE='\033[0;35m' CYAN='\033[0;36m' YELLOW='\033[1;33m'
  else
    NOFORMAT='' RED='' GREEN='' ORANGE='' BLUE='' PURPLE='' CYAN='' YELLOW=''
  fi
}

msg() {
  echo >&2 -e "${1-}"
}

die() {
  local msg=$1
  local code=${2-1} # default exit status 1
  msg "$msg"
  exit "$code"
}

parse_params() {
  # default values of variables set from params
  year=$(date "+%Y")
  day=$(date "+%d")
  token=''
  if [[ -f ".token" ]]; then
      token="$(cat .token)"
  fi

  while :; do
    case "${1-}" in
    -h | --help) usage ;;
    -v | --verbose) set -x ;;
    --no-color) NO_COLOR=1 ;;
    -y | --year)
      year="${2-}"
      shift
      ;;
    -d | --day)
      day="${2-}"
      shift
      ;;
    -t | --token)
      token="${2-}"
      shift
      ;;
    -?*) die "Unknown option: $1" ;;
    *) break ;;
    esac
    shift
  done

  # check required params and arguments
  [[ -z "${year-}" ]] && die "Missing required parameter: year"
  [[ -z "${day-}" ]] && die "Missing required parameter: day"
  simple_day=$(echo $day | sed 's/^0*//')

  return 0
}

check_dependencies() {
  echo >&2 -n "${GREEN}Checking dependencies...${NOFORMAT} "
  local dependencies=("curl" "sbt")
  for name in "${dependencies[@]}"; do
    [[ $(which $name 2>/dev/null) ]] || { echo >&2 -en "\n$name needs to be installed. Use 'brew install $name'";deps=1; }
  done
  [[ ${deps-} -ne 1 ]] && echo >&2 "OK" || die "Install the above and rerun this script"
}

parse_params "$@"
setup_colors
check_dependencies

# script logic here
msg "${GREEN}Generating files${NOFORMAT}"
sbt "g8Scaffold problem --year=${year} --day=${day}" > /dev/null 2>&1
msg "    ${BLUE}OK${NOFORMAT}"
msg "${GREEN}Retrieving input data${NOFORMAT}"
curl --silent --fail --cookie "session=$token" "https://adventofcode.com/$year/day/$simple_day/input" > src/main/resources/$year/day$day.txt
msg "    ${BLUE}OK${NOFORMAT}"
