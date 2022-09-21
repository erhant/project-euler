#!/bin/bash

# Read filename
if [[ $# -ne 1 ]] ; then
  echo "Please provide a file:"
  echo "$0 <path to file>"
  exit 1
fi
FILE=$1 
EXT="${FILE##*.}"

# run file
case $EXT in
  "py") # Python script
    python3 $1
    ;;
  "c") # C code
    gcc -std=c99 -Wall -Wextra -Wpedantic -Werror $1 -o tmp && \
      ./tmp && \
      rm tmp
    ;;  
  "cpp") # C++ code
    g++ --std=c++11 -Wall -Wextra -Wpedantic -Werror $1 -o tmp && \
      ./tmp && \
      rm tmp
    ;; 
  *)
    echo "Unexpected file extension: $EXT"
    exit 1
    ;;
esac