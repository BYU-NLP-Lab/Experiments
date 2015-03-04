#!/usr/bin/python
import math
import os
from os import path
import sys
import glob
import broom # custom library from ~plf1/git/utils/broom

######################################################
#                   Helper Functions
#####################################################
def parabolic_points(lower,upper,num):
    points = range(int(math.sqrt(lower)), int(round(math.sqrt(upper))), int(round(math.sqrt(upper)/num)))
    return [p*p for p in points] 
def shortenval(val,maxlength=10):
    return str(val)[-maxlength:]

def topicfilename(keys,state):
    return "%s-%s" % (state['--dataset-type'],state['--num-topics'])

class FileNamer():
    def __init__(self,directory):
        self.directory=directory
        self.prevnames = {}
        #self.prevnames = set()
    def generator(self,keys,state):
        name = "{dr}/{name}".format(dr=self.directory,name=topicfilename(keys,state))
        if name in self.prevnames:
            oldkeys,oldstate = self.prevnames[name]
            raise Exception("two jobs received the same name! \n\t%s\n\t%s\n\t%s" % (name,oldstate,state))
        self.prevnames[name] = (keys,state)
        if len(path.basename(name))>255: # leave a few characters for filename 
            raise Exception("file name is longer than 255 characters: \n\t%s" % path.basename(name))
        yield name


######################################################
#                   MAIN
#####################################################
def jobs(first_experiment, results_dir, mem):

    # java runtime
    main = 'edu.byu.nlp.al.app.CrowdsourcingLearningCurve'
    classpath = ['config','"target/dependency/*"'] 
    java = "java -Xmx{mem} -cp {classpath} {main}".format(mem=mem, classpath=':'.join(classpath), main=main)
    javacommand = 'cd {cwd}/.. && {java}'.format(cwd=os.getcwd(), java=java)

    num_evalpoints = 5
    repeats = 1
    chains = 1 # TODO: consider more for sampling runs

    # sweep parameters
    cwd = os.getcwd()
    swept = broom.sweep(
        # main command
        javacommand,

        # constants (omitted because we aren't going to experiment with these any more)
        #('--num-batches',1),

        # dataset-related params
        ('--basedir',(
            'data/newsgroups',
            'data/cfgroups1000',
            'data/dredze/derived',
            'data/enron',
            'data/r8',
            'data/webkb',
            'data/cade12',
            'data/r52',
            )),
        ('--dataset-type', broom.Mapper('--basedir',{
            'naivebayes-20':'NB20',
            'multiresp-2':'NB2',
            'newsgroups':'NEWSGROUPS',
            'groups1000':'CFGROUPS1000',
            'ng':'NG',
            'enron':'ENRON',
            'dredze':'DREDZE',
            'cade12':'CADE12',
            'r8':'R8',
            'r52':'R52',
            'webkb':'WEBKB',
            },matchsubstrings=True).generator),
        ('--dataset', broom.Mapper('--basedir',{
            'cfgroups1000':'cfgroups1000.json',
            'cfsimplegroups1000a':'cfsimplegroups1000a.json',
            'enron':'ldc_split',
            'dredze':'1v0.json',
            },default='full_set',matchsubstrings=True).generator),
        ('--eval-point', 10),

        # aggresive feature selection
        ('--feature-count-cutoff',5),
        ('--top-n-features-per-document',-1),
        ('--labeling-strategy','cslda'),

        # annotations
        ('--num-topics',(10,50,100)),
        ('--training','sample-z-3000'),
        ('--training-percent', 85), 
        ('--validation-percent', 0), 
        ('--inline-hyperparam-tuning'),

        # weak priors
        ('--b-theta',broom.Mapper('--labeling-strategy',{
            'cslda': 0.1,
        }, default=1.0).generator),
        ('--b-phi','0.1'),
        ('--b-mu','0.1'),
        ('--c-mu','1'),
        ('--c-gamma','1'),
        ## weak gamma prior #1 (static)
        #('--b-gamma','0.1'),
        # weak gamma prior #2 (scale with num-class)
        # Accuracies depend on the number of class labels.
        # To be somewhat general/uninformed, let's set accuracy
        # to be delta better than random (1+delta)/(numclasses+delta)
        ('--b-gamma',broom.Mapper('--basedir',{
            'naivebayes-20':(1.+2.)/(20.+2.),
            'newsgroups':(1.+2.)/(20.+2.),
            'cfgroups1000':(1.+2.)/(20.+2.),
            'cfsimplegroups1000a':(1.+2.)/(10.+2.),
            'ng':(1.+2.)/(20.+2.),
            'enron':(1.+2.)/(32.+2.), 
            'dredze':(1.+2.)/(2.+2.),
            'r8':(1.+2.)/(8.+2.),
            'r52':(1.+2.)/(52.+2.),
            'cade12':(1.+2.)/(12.+2.),
            'webkb':(1.+2.)/(4.+2.),
            }, default=-1, matchsubstrings=True).generator),

        # random runs and chains
        ('--data-seed', 1),      
        ('--algorithm-seed', 1),
        ('--serialize-to-file',FileNamer(results_dir).generator),

    )

    # report parameters 
    for keys,state in swept:
        filename = state['--serialize-to-file']
        if os.path.exists(filename):
            print '.',
        else:
            cmd = broom.join(keys,state,delim=' ',equals='=')
            # jopt doesn't allow equals signs after single character options
            cmd = cmd.replace('-k=','-k ',1)
            yield cmd

if __name__ == "__main__":
    import os
    headn = 500
    firstexperiment = 101

    outdir = '.'
    #outdir = '/tmp/multiannresults'
    try:
        os.makedirs(outdir)
    except:
        pass
    print
    print '============================================'
    print '= first %d jobs' % headn
    print '============================================'
    total = 0
    for i,job in enumerate(jobs(firstexperiment,outdir,"4g")):
        if i<=headn:
            print 
            print job
        total = i+1
    print '\ntotal jobs=%d' % total

