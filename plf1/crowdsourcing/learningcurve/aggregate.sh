#!/bin/sh

# dredze data table (many annotator columns)
head -q -n1 $1/`ls -tr $1 | grep results | grep DREDZE | tail -n1`> /tmp/dredze.csv
find $1 -name '*DREDZE*results.csv' -exec cat '{}' \; | grep -v num_annotations >> /tmp/dredze.csv

#cfgroups data
head -q -n1 $1/`ls -tr $1 | grep results | grep GROUPS1000 | tail -n1`> /tmp/cfgroups1000.csv
find $1 -name '*GROUPS1000*results.csv' -exec cat '{}' \; | grep -v num_annotations >> /tmp/cfgroups1000.csv

# 5 simulated annotators 
head -q -n1 $1/`ls -tr $1 | grep results | grep grr | tail -n1` > /tmp/simulated.csv
find $1 -name '*grr*results.csv' -exec cat '{}' \; | grep -v num_annotations >> /tmp/simulated.csv


# join the tables and report
python ~/git/utils/mycsv/append.py /tmp/dredze.csv /tmp/cfgroups1000.csv /tmp/simulated.csv
