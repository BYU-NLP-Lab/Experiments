#!/bin/sh

if [ -z $1 ]; then
  echo "usage: ./distribute.sh [potatoes_file]"
  exit 1
fi

# copy experiments directory
for f in `cat $1 | sort | uniq`; do
  echo "copying Experiments to $f:/local/plf1/git/Experiments"
  ssh $f "mkdir -p /local/plf1/git"
  rsync -Paz /local/plf1/git/Experiments $f:/local/plf1/git/
done

# copy data directories over
for f in `cat $1 | sort | uniq`; do
  echo "copying data to $f:/local/plf1/data"
  rsync -Paz /local/plf1/data $f:/local/plf1/
done

