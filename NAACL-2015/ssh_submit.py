#!/usr/bin/python
import jobfarm
import argparse
import fig4a
import fig4b
import fig5
import fig5conflictmild
import table2
import fig6
import os
from subprocess import call

def mkdir_q(dr):
    try:
        os.mkdir(dr, 0755)
    except OSError:
        pass

def composite_job_generator(first_experiment, results_dir, mem):
    mkdir_q(os.path.join(results_dir,'fig4a'))
    mkdir_q(os.path.join(results_dir,'fig4b'))
    mkdir_q(os.path.join(results_dir,'fig5'))
    mkdir_q(os.path.join(results_dir,'fig5conflictmild'))
    mkdir_q(os.path.join(results_dir,'table2'))
    mkdir_q(os.path.join(results_dir,'fig6'))
    for job in fig4a.jobs(first_experiment, os.path.join(results_dir,'fig4a'), None, mem):
        yield job
    for job in fig4b.jobs(first_experiment, os.path.join(results_dir,'fig4b'), None, mem):
        yield job
    for job in fig5.jobs(first_experiment, os.path.join(results_dir,'fig5'), None, mem):
        yield job
    for job in fig5conflictmild.jobs(first_experiment, os.path.join(results_dir,'fig5conflictmild'), None, mem):
        yield job
    for job in table2.jobs(first_experiment, os.path.join(results_dir,'table2'), None, mem):
        yield job
    for job in fig6.jobs(first_experiment, os.path.join(results_dir,'fig6'), None, mem):
        yield job

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
    args = parser.parse_args()

    # download dependencies
    if args.copy_dependencies:
        call(["mvn","clean","dependency:copy-dependencies"])

    # setup results directories
    results_dir = os.path.join(os.getcwd(), 'results')
    mkdir_q(results_dir)

    # farm jobs out to clients
    job_generator = composite_job_generator(args.first_experiment, results_dir, args.mem)
    jobfarm.farm_jobs(job_generator, args.hosts, args.pollint, args.logdir, args.test)
