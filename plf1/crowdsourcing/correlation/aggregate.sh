#!/bin/sh

head -q -n1 $1/`ls $1 | grep results | head -n1`
#tail -q -n1 $1/*results.csv | grep -v num_annotations
find $1 -name '*results.csv' -exec cat '{}' \; | grep -v num_annotations
