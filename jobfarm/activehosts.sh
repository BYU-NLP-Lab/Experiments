#!/bin/sh
# list all hosts that are still active (by using 
# the heuristic that a finished host will print a 
# "closing connection ..." line at the very end.)
if [ -z $1 ]; then
  echo "usage: activehosts.sh [output directory]"
  exit
fi
grep --files-without-match "closing connection" $1/*.out
