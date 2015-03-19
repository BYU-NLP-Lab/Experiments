#!/bin/sh
python ~/git/utils/mycsv/append.py results/fig4a/* results/fig4b/* results/fig5/* results/fig5conflictmild/* results/fig6/* > csv/naacl-2015-1.csv
python ~/git/utils/mycsv/append.py results/table2/* > csv/naacl-2015-2.csv
