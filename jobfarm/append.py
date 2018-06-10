#!/usr/bin/python3
import os
import sys
import string
import csv
import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser("This program appends csv files to one another, matching columns by header and substituting default values when no values for a column exist in a given file.")
    parser.add_argument('-n','--na',default='NA',help='The value that should be used when a cell value doesnt exist')
    parser.add_argument('files',nargs='+',help='A list of csv files that should be appended to one another.')
    args = parser.parse_args()

    # get all header values
    header = set()
    for f in args.files:
        if not os.path.exists(f):
            sys.stderr.write("File %s does not exist. Skipping.\n" % f)
        else:
            try:
                fieldnames = csv.DictReader(open(f)).fieldnames
            except Exception as e:
                sys.stderr.write("Unable to open file %s:\n\t%s"%(f,e))
            if fieldnames is not None:
                cols = [key for key in fieldnames]
                header.update(cols)
    header = sorted(header)

    # print header
    print(','.join(header))

    # read each file
    for f in args.files:
        if not os.path.exists(f):
            pass
        else:
            firsterr = True
            # read each row
            for r in csv.DictReader(open(f)):
                # get each field, substituting default value if field doesn't exist for this row
                row = [r.get(field,args.na) for field in header]
                # substitute out any None values 
                # these happen when a csv is self-inconsistent
                if None in row:
                    if firsterr: 
                        sys.stderr.write("%s has incomplete rows. Inserting null values.\n" % f)
                        firsterr = False
                    row = [args.na if val is None else val for val in row]
                print(','.join(row))

