#!/bin/sh

if [ -z $2 ]; then
  echo "moves files from remote machines to local machine"
  echo "usage: ./gather.sh [potatoes_file] [local_results_dir]"
  exit 1
fi

results_dir="plf1/git/Experiments/plf1/TACL-2015-Measurements-submission/results"

results=`basename $2`
for f in `cat $1 | sort | uniq`; do
  srcdir="/net/$f/$results_dir/$results"
  echo "cleaning $srcdir"
  ./cleanup.sh $srcdir
  echo "moving $srcdir to $2"
  mv $srcdir/* $2/
  echo ""
done

