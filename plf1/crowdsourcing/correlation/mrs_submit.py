#!/usr/bin/python

import mrs
import multiann
import itertools
import os
import subprocess

class JobLauncher(mrs.GeneratorCallbackMR):
    """Uses mrs to schedule arbitrary command-line jobs on host machines"""

    def __init__(self, opts, args):
        super(JobLauncher, self).__init__(opts, args)
        self.stage = opts.stage
        self.first_experiment = opts.first_experiment
        self.splits = 300
        # setup results directories
        outdir = os.getcwd()
        results_dir = os.path.join(outdir, 'results/correlation')
        self.baseline_results_dir = os.path.join(results_dir, 'baseline')
        self.samples_dir = os.path.join(results_dir, 'samples')
        try:
            os.mkdir(results_dir, 0755)
        except OSError:
            pass
        try:
            os.mkdir(self.baseline_results_dir, 0755)
        except OSError:
            pass
        try:
            os.mkdir(self.samples_dir, 0755)
        except OSError:
            pass



    '''
    This method is called every time the main program loop yields. 
    I choose to do this between every iteration, and use the 
    opportunity to write out the current contigs.

    The dataset is an abstract object, and pieces of it may be 
    stored on many different machines. In order to see all 
    of the contigs assembled from all of the machines on the 
    cluster, I must first call fetchall() on the dataset. 
    This sends all of the data (contigs) to the master node 
    so that I can write all of the contigs to an out file. 
    
    End the first time this is called, because we are running 
    a single big loop for simplicity
    '''
    def callback(self,ds):
        print "------CALL BACK-----"
        return False

    '''
    This method is called once at the beginning of the job, and is in charge
    of iteratively mapping and reducing. It can take a break as often as
    it chooses by yielding a dataset representing the current state 
    of computation, along with a callback method that is in charge 
    of determining whether or not the program is finished, based 
    on the state of the dataset.

    The splits parameter you pass around reflects how many pieces 
    the dataset should be chunked into before being passed 
    over the network to your machines. This should be set to 
    something reasonable like many 3 times the number of machines 
    in your cluster.
    '''
    def generator(self, job):
        itr = 0
        # bogus dataset. We will run only 1 iteration
        # and the map function will do all the work
        # of generating jobs
        ds = job.local_data([(1,["bogus"])],splits=self.splits)

        while True:

            # iteratively map (divide into batches) and reduce (assemble batches)
            ds = job.map_data(ds,mapper=self.map,splits=self.splits) # key=batch, val=contig
            ds = job.reduce_data(ds,self.reduce,splits=self.splits)
            itr = itr + 1
            yield (ds, self.callback)

    '''
    inputs
        key: the key value previously associated with these values. 
        values: values that need to be assigned new keys
    outputs
        yield a number of key value pairs
    '''
    def map(self, key, values):
        """ enumerate the jobs, ignoring old key/values """
        print "generating jobs . . ."
        i = 0
        for job in multiann.jobs(self.stage, self.first_experiment, self.baseline_results_dir, self.samples_dir):
            if job is not None:
                i+=1
                yield (i, job)

    '''
    inputs
        batch: the single key value associated with all of 
        the values being reduced
        contigs: the list of values associated with the key
    outputs
        do some operation on the values and yield the resulting new value
    '''
    def reduce(self, key, values):
        """ run the job """
        for value in values:
            print "\nRunning cmd %d\n\t%s" % (key,value)
            os.system(value)

        print "============= Done! ==================="
        yield ["Done"]
        #yield None # run_job() # TODO


    '''
    Define a set of command line parameters whose values will be 
    passed to your program in the opts parameter of the __init__ method.
    '''
    @classmethod
    def update_parser(cls, parser):
        parser.add_option('--stage',
                type='int',
                dest='stage',
                default=1,
                help='Stages are 1 (run chains) or 2 (initialize with chains and run optimization)',
                )
        parser.add_option('--first-experiment',
                type='int',
                dest='first_experiment',
                default=101,
                help='Start running experiments from this number on',
                )
        return parser

if __name__ == '__main__':
    mrs.main(JobLauncher)

# vim: et sw=4 sts=4
