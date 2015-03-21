#!/usr/bin/python

import subprocess
import math
import os
import sys
import glob
import multiann

MAX_JOBS = 5000
FIRST_EXPERIMENT = 1

######################################################
#                   Runtime settings
#####################################################
mem = 4000
nodes = 1  # nodes
ntasks = 1  # procs
name = "multiresp"
batchsize = 3
hours,mins = 0,30*batchsize

def do_batch(batch):
        # join batch jobs with a semicolon, and 
        # add interpreter to the first of each job
        bashscript = "#!/bin/sh\n %s" % '; \n'.join(batch)
        # supercomputer 
        cmd = "sbatch --gid=fslg_nlp --time={hours}:{mins}:00 --ntasks={ntasks} --nodes={nodes} --mem-per-cpu={mem} -J {name}".format(hours=hours, mins=mins, ntasks=ntasks, nodes=nodes, mem=mem, name=name)
        #print bashscript
        #sys.exit(1)
        # submit to batch system
        p = subprocess.Popen(cmd.split(), stdin=subprocess.PIPE)
        p.communicate(bashscript)
        p.wait()


######################################################
#                   MAIN
#####################################################
if __name__ == "__main__":

    dir = os.getcwd()
    results_dir = os.path.join(dir, 'results/nips')
    try:
        os.mkdir(results_dir, 0755)
    except OSError:
        pass

    # Main
    batchcount = int(subprocess.Popen(["squeue -u $(whoami) | wc | tr -s ' ' | cut -d\  -f2"], shell=True, stdout=subprocess.PIPE).communicate()[0])-1
    if batchcount>=MAX_JOBS:
        sys.exit("Job limit reached (%d/%d)" % (batchcount,MAX_JOBS))
    raw_input("About to submit up to %d batches. Press enter to continue (ctrl+c to cancel) " % (MAX_JOBS))
    print "go!"
    batch = []
    for job in multiann.jobs(FIRST_EXPERIMENT, results_dir, "%im"%mem):
        if job is not None:
            # build and submit batches
            batch.append(job)
            if len(batch)==batchsize:
                # do job batch
                do_batch(batch)
                batch = []
                # track batches
                batchcount += 1
                if batchcount > 0 and batchcount % 50 == 0:
                    print "batch %d" % batchcount
                # quit when it's time
                if batchcount >= MAX_JOBS:
                    sys.exit("Job limit reached")

