#!/bin/sh

if [ -z $2 ]; then
  echo "copies files from remote machines to local machine"
  echo "usage: ./gather.sh [potatoes_file] [local_results_dir]"
  exit 1
fi

results_dir="plf1/git/Experiments/plf1/TACL-2015-Vector-submission/results"
results=`basename $2`
for f in `cat $1 | sort | uniq`; do
  srcdir="/net/$f/$results_dir/$results"
  echo "copying $srcdir to $2"
  cp $srcdir/* $2/
  echo ""
done

