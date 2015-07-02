#!/bin/sh

rm slurm-*

if [ -z $1 ]; then
  pass
else
  find $1 -type f -empty -delete
fi

