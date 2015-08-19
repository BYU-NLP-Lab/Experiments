#!/bin/sh
set -e

if [ -z $1 ]; then
  echo "kill all java jobs on the machines listed in potatoes_file"
  echo "usage: ./gather.sh [potatoes_file]"
  exit 1
fi

cat $1 | sort | uniq > /tmp/killpots
pssh -h /tmp/killpots -i "killall java"
  
