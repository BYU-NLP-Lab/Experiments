#!/usr/bin/python
import jobfarm
import argparse
import multiann
import os
from subprocess import call

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    # jobfarm args
    parser.add_argument("--hosts","-n",required=True,help="hosts file with one machine name per line (a machine may be listed multiple times)")
    parser.add_argument("--logdir","-o",default=None,help="Where should client machine stdout/err be logged? (if not specified, output is ignored)")
    parser.add_argument("--pollint","-p",default=1,help="How often should hosts be polled for job completion?")
    parser.add_argument("--test","-t",default=False,action='store_true',help="do a dummy run (without contacting remotes or running jobs")
    # crowdsourcing args
    parser.add_argument("--copy-dependencies","-c",action='store_true',default=False,help="Reload maven dependencies before running?")
    parser.add_argument("--mem","-m",default="4g",help="How much memory to allocate each job jre?")
    parser.add_argument("--first-experiment","-f",default=101,help="Start running experiments from this number on")
    parser.add_argument("--results-dir","-r",required=True,help="Where should job results be stored?")
    parser.add_argument("--topics-dir",default="topic_vectors",help="Where should the job look for pre-computed topic vectors in files named DATASET-numtopics?")
    args = parser.parse_args()

    # download dependencies
    if args.copy_dependencies:
        call(["mvn","clean","dependency:copy-dependencies"])

    # setup results directories
    outdir = os.getcwd()
    results_dir = os.path.join(outdir, args.results_dir)
    try:
        os.mkdir(results_dir, 0755)
    except OSError:
        pass

    # farm jobs out to clients
    job_generator = multiann.jobs(args.first_experiment, results_dir, args.topics_dir, args.mem)
    jobfarm.farm_jobs(job_generator, args.hosts, args.pollint, args.logdir, args.test)

