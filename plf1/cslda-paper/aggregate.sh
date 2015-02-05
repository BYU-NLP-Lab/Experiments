#!/bin/sh

# dredze data table (many annotator columns)
FINISHED_FILE=`wc $1/* | grep DREDZE | grep -v " 0 " | tr -s ' ' | cut -d\  -f5 | head -n1`
if [ -z $FINISHED_FILE ]; then
  rm /tmp/dredze.csv
else
  head -q -n1 $FINISHED_FILE  > /tmp/dredze.csv
  find $1 -name '*DREDZE*results.csv' -exec cat '{}' \; | grep -v num_annotations >> /tmp/dredze.csv
fi

#cfgroups data
FINISHED_FILE=`wc $1/* | grep PS1000 | grep -v " 0 " | tr -s ' ' | cut -d\  -f5 | head -n1`
if [ -z $FINISHED_FILE ]; then
  rm /tmp/cfgroups1000.csv
else
  head -q -n1 $FINISHED_FILE  > /tmp/cfgroups1000.csv
  find $1 -name '*PS1000*results.csv' -exec cat '{}' \; | grep -v num_annotations >> /tmp/cfgroups1000.csv
fi

# 5 simulated annotators 
FINISHED_FILE=`wc $1/* | grep kdeep | grep -v " 0 " | tr -s ' ' | cut -d\  -f5 | head -n1`
if [ -z $FINISHED_FILE ]; then
  rm /tmp/simulated.csv
else
  head -q -n1 $FINISHED_FILE  > /tmp/simulated.csv
  find $1 -name '*kdeep*results.csv' -exec cat '{}' \; | grep -v num_annotations >> /tmp/simulated.csv
fi


# join the tables and report
python ~/git/utils/mycsv/append.py /tmp/dredze.csv /tmp/cfgroups1000.csv /tmp/simulated.csv
