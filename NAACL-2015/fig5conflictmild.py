#!/usr/bin/python
import math
import os
from os import path
import sys
import glob
import broom # custom library from ~plf1/git/utils/broom


data_size = {
    'newsgroups':20000,
    'cade12':40000,
    'enron':5000,
    'r8':8000,
    'r52':10000,
    'webkb':5000,
}
num_classes = {
    'newsgroups':20,
    'cade12':12,
    'enron':32,
    'r8':8,
    'r52':52,
    'webkb':4
}
depth = 3

######################################################
#                   Helper Functions
#####################################################
def parabolic_points(lower,upper,num):
    points = range(int(math.sqrt(lower)), int(round(math.sqrt(upper))), int(round(math.sqrt(upper)/num)))
    return [p*p for p in points] 
def shortenval(val,maxlength=10):
    return str(val)[-maxlength:]

def jobname(keys,state,delim=':',equals=''):
    #print state['--eval-point']
    keys = [key for key in keys if '-' in str(key)] # only include options
    keys = [key for key in keys if 'file' not in key] # exclude undesirable opts
    keys = [key for key in keys if state[key] is not None]
    # manual tweaks
    keys.remove('--b-theta')
    keys.remove('--b-phi')
    keys.remove('--b-mu')
    keys.remove('--c-mu')
    keys.remove('--b-gamma')
    keys.remove('--c-gamma')
    parts = []
    for key in keys:
        parts.append("%s%s%s" % (broom.shorten_option(key,maxlength=4),equals,shortenval(state[key],maxlength=9)))
    name = delim.join(parts)
    name = name.replace('-','') # remove hyphens
    name = name.replace('.','') # remove path dots
    name = name.replace('/','') # remove path slashes
    return name

class FileNamer():
    def __init__(self,directory,suffix):
        self.directory=directory
        self.suffix=suffix
        self.prevnames = {}
    def generator(self,keys,state):
        name = "{dr}/{name}-{sfx}.csv".format(dr=self.directory,name=jobname(keys,state),sfx=self.suffix)
        if name in self.prevnames:
            oldkeys,oldstate = self.prevnames[name]
            raise Exception("two jobs received the same name! \n\t%s\n\t%s\n\t%s" % (name,oldstate,state))
        self.prevnames[name] = (keys,state)
        if len(path.basename(name))>255: # leave a few characters for filename suffix
            raise Exception("file name is longer than 255 characters: \n\t%s" % path.basename(name))
        yield name


######################################################
#                   MAIN
#####################################################
def jobs(first_experiment, results_dir, topics_dir, mem):

    # java runtime
    main = 'edu.byu.nlp.al.app.CrowdsourcingLearningCurve'
    classpath = ['config','"target/dependency/*"'] 
    java = "java -Xmx{mem} -cp {classpath} {main}".format(mem=mem, classpath=':'.join(classpath), main=main)
    javacommand = 'cd {cwd} && {java}'.format(cwd=os.getcwd(), java=java)

    num_evalpoints = 10
    repeats = 5
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
            #'data/naivebayes-20',
            #'data/multiresp-2.tgz',
            #'data/cfgroups/cfsimplegroups1000a',
            #'data/cfgroups/cfsimplegroups1000b',
            #'data/cfgroups/cfsimplegroups1000c',
            #'data/cfgroups/cfgroups1000',
            'data/newsgroups',
            #'data/enron',
            #'data/r8',
            #'data/webkb',
            #'data/cade12',
            #'data/r52',
            )),
        ('--dataset-type', broom.Mapper('--basedir',{
            'naivebayes-20':'NB20',
            'multiresp-2':'NB2',
            'newsgroups':'NEWSGROUPS',
            'groups1000':'CFGROUPS1000',
            'ng':'NG',
            'enron':'ENRON',
            'cade12':'CADE12',
            'r8':'R8',
            'r52':'R52',
            'webkb':'WEBKB',
            },matchsubstrings=True).generator),
        ('--dataset', broom.Mapper('--basedir',{
            'cfgroups1000':'cfgroups1000.json',
            'enron':'ldc_split',
            },default='full_set',matchsubstrings=True).generator),
        ('--eval-point', broom.Mapper('--basedir',{
            'naivebayes-20':parabolic_points(150,data_size['newsgroups']*depth,num_evalpoints),
            'newsgroups':parabolic_points(150,data_size['newsgroups']*depth,num_evalpoints),
            'ng':parabolic_points(150,data_size['newsgroups']*depth,num_evalpoints),
            'groups1000':parabolic_points(150,10000,num_evalpoints),
            'cade12':parabolic_points(150,data_size['cade12']*depth,num_evalpoints),
            'enron':parabolic_points(150,data_size['enron']*depth,num_evalpoints),
            'r8':parabolic_points(150,data_size['r8']*depth,num_evalpoints),
            'r52':parabolic_points(150,data_size['r52']*depth,num_evalpoints),
            'webkb':parabolic_points(150,data_size['webkb']*depth,num_evalpoints),
            }, matchsubstrings=True).generator),
        ('--feature-normalization-constant',broom.Mapper('--basedir',{
            'r8':60,
            'r52':60,
            }, default=100, matchsubstrings=True).generator),

        # aggresive feature selection
        ('--feature-count-cutoff',5),
        ('--top-n-features-per-document',-1),

        # annotations
        ('--annotation-strategy', broom.Mapper('--basedir',{
            'groups1000':'real',
            }, default='kdeep', matchsubstrings=True).generator),
        ('--annotator-accuracy', broom.Mapper('--basedir',{
            'groups1000':None,
            }, default=("CONFLICT_MILD"), matchsubstrings=True).generator),
        ('-k', broom.Mapper('--annotation-strategy',{
            'real':None,
            }, default=depth).generator),

        # no observed trusted labels
        ('--num-observed-labels',0),

        # inference 
        ('--labeling-strategy',['ubaseline','varmomresp','varrayk']), 
        ('--initialization-strategy','baseline'), 

        ('--training',broom.Mapper('--labeling-strategy',{
            'momresp':'sample-all-500',
        },default='maximize-all').generator),

        ('--diagonalization-method',"GOLD"),
        ('--gold-instances-for-diagonalization',-1),

        ('--training-percent', 85), 
        ('--validation-percent', 0), 

        ('--truncate-unannotated-data',broom.Mapper('--labeling-strategy',{
            'varmomresp':(None,''),
        },default=None).generator),

        # weak priors
        ('--b-theta',1.0),
        ('--b-phi','0.1'),
        ('--b-mu','0.1'),
        ('--c-mu','1'),
        ('--c-gamma','1'),
        # Accuracies depend on the number of class labels.
        # To be somewhat general/uninformed, let's set accuracy
        # to be delta better than random (1+delta)/(numclasses+delta)
        ('--b-gamma',broom.Mapper('--basedir',{
            'naivebayes-20':(1.+2.)/(20.+2.),
            'newsgroups':(1.+2.)/(20.+2.),
            'cfgroups1000':(1.+2.)/(20.+2.),
            'ng':(1.+2.)/(20.+2.),
            'enron':(1.+2.)/(32.+2.), 
            'r8':(1.+2.)/(8.+2.),
            'r52':(1.+2.)/(52.+2.),
            'cade12':(1.+2.)/(12.+2.),
            'webkb':(1.+2.)/(4.+2.),
            }, default=-1, matchsubstrings=True).generator),

        # random runs and chains
        ('--data-seed', broom.Mapper('--annotation-strategy',{
            # for real data, data/annotations are predetermined so all randomness must come from algorithm
            'real': first_experiment+1,
            }, 
            matchsubstrings=True, 
            default=range(first_experiment+1,first_experiment+1+repeats)).generator),
        #('--algorithm-seed', broom.Range(1,1+chains).generator), # chains
        ('--algorithm-seed', broom.Mapper('--annotation-strategy',{
            # for real data, all randomness must come from algorithm
            'real': range(1,1+(chains*repeats))
            }, matchsubstrings=True, default=range(1,1+chains)).generator),
        # output files
        ('--results-file',FileNamer(results_dir,'results').generator),
        #('--serialize-to-file',FileNamer(results_dir,'vars').generator),
        #('--debug-file',FileNamer(results_dir,'settings').generator),
        #('--tabular-file',FileNamer(results_dir,'tab').generator),

    )

    # report parameters 
    for keys,state in swept:
        if path.exists(state['--results-file']):
            # experiment has already been run
            print '.',
            sys.stdout.flush()
        else:
            cmd = broom.join(keys,state,delim=' ',equals='=')
            # jopt doesn't allow equals signs after single character options
            cmd = cmd.replace('-k=','-k ',1)
            yield cmd

if __name__ == "__main__":
    import os
    import sys
    headn = 500
    firstexperiment = 101

    outdir = '/tmp/bogus' if len(sys.argv)<=1 else sys.argv[1]
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
    for i,job in enumerate(jobs(firstexperiment,outdir,'topic_vectors',"4g")):
        if i<=headn:
            print 
            print job
        total = i+1
    print '\ntotal jobs=%d' % total

