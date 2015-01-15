#!/usr/bin/python
import jobfarm
import argparse
import multiann
import os

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    # jobfarm args
    parser.add_argument("--hosts","-n",required=True,help="hosts file with one machine name per line (a machine may be listed multiple times)")
    parser.add_argument("--logdir","-o",default=None,help="Where should client machine stdout/err be logged? (if not specified, output is ignored)")
    parser.add_argument("--pollint","-p",default=1,help="How often should hosts be polled for job completion?")
    parser.add_argument("--test","-t",default=False,action='store_true',help="do a dummy run (without contacting remotes or running jobs")
    # crowdsourcing args
    parser.add_argument("--mem","-m",default="4g",help="How much memory to allocate each job jre?")
    parser.add_argument("--stage","-s",default=2,help="Stages are 1 (run chains) or 2 (initialize with chains and run optimization) or 3 (re-run diagonalization on chains)")
    parser.add_argument("--first-experiment","-f",default=101,help="Start running experiments from this number on")
    parser.add_argument("--results-dir","-r",required=True,help="Where should job results be stored?")
    args = parser.parse_args()

    # setup results directories
    outdir = os.getcwd()
    results_dir = os.path.join(outdir, args.results_dir)
    chain_results_dir = os.path.join(results_dir, 'chains')
    optimized_results_dir = os.path.join(results_dir, 'optimized')
    try:
        os.mkdir(results_dir, 0755)
    except OSError:
        pass
    try:
        os.mkdir(chain_results_dir, 0755)
    except OSError:
        pass
    try:
        os.mkdir(optimized_results_dir, 0755)
    except OSError:
        pass

    # farm jobs out to clients
    job_generator = multiann.jobs(args.stage, args.first_experiment, chain_results_dir, optimized_results_dir, args.mem)
    jobfarm.farm_jobs(job_generator, args.hosts, args.pollint, args.logdir, args.test)
