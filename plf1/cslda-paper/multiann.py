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

def jobname(keys,state,delim=':',equals=''):
    #print state['--eval-point']
    keys = [key for key in keys if '-' in str(key)] # only include options
    keys = [key for key in keys if 'file' not in key] # exclude undesirable opts
    # manual tweaks
    keys.append('--annotator-file')
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
        #self.prevnames = set()
    def generator(self,keys,state):
        name = "{dr}/{name}-{sfx}.csv".format(dr=self.directory,name=jobname(keys,state),sfx=self.suffix)
        if name in self.prevnames:
            oldkeys,oldstate = self.prevnames[name]
            raise Exception("two jobs received the same name! \n\t%s\n\t%s\n\t%s" % (name,oldstate,state))
        self.prevnames[name] = (keys,state)
        if len(path.basename(name))>255: # leave a few characters for filename suffix
            raise Exception("file name is longer than 255 characters: \n\t%s" % path.basename(name))
        yield name

def serializedinputfiles(keys,state):
    yield "infile1 infile2"


######################################################
#                   MAIN
#####################################################
def jobs(first_experiment, results_dir, mem):

    # java runtime
    main = 'edu.byu.nlp.al.app.CrowdsourcingLearningCurve'
    classpath = ['config','"target/dependency/*"'] 
    java = "java -Xmx{mem} -cp {classpath} {main}".format(mem=mem, classpath=':'.join(classpath), main=main)
    javacommand = 'cd {cwd} && {java}'.format(cwd=os.getcwd(), java=java)

    num_evalpoints = 10
    repeats = 1
    chains = 1 # TODO: consider more for sampling runs
    inputfiles = None # TODO: if necessary, init w chains

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
            'data/newsgroups',
            'data/cfgroups1000',
            #'data/dredze/derived',
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
            'cfgroups1000':'CFGROUPS1000',
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
            'enron':'ldc_split',
            'dredze':'1v0.json',
            },default='full_set',matchsubstrings=True).generator),
        ('--eval-point', broom.Mapper('--basedir',{
            'naivebayes-20':parabolic_points(150,60000,num_evalpoints),
            'newsgroups':parabolic_points(150,60000,num_evalpoints),
            'ng':parabolic_points(150,60000,num_evalpoints),
            'cfgroups1000':parabolic_points(150,10000,num_evalpoints),
            'dredze':parabolic_points(150,21000,num_evalpoints),
            'cade12':parabolic_points(150,200000,num_evalpoints),
            'enron':parabolic_points(150,14000,num_evalpoints),
            'r8':parabolic_points(150,22000,num_evalpoints),
            'r52':parabolic_points(150,25000,num_evalpoints),
            'webkb':parabolic_points(150,12000,num_evalpoints),
            }, matchsubstrings=True).generator),
        ('--feature-normalization-constant',broom.Mapper('--basedir',{
            'dredze':10,
            'r8':60,
            'r52':60,
            }, default=100, matchsubstrings=True).generator),

        # aggresive feature selection
        ('--feature-count-cutoff',5),
        ('--top-n-features-per-document',-1),

        # annotations
        ('--annotation-strategy', broom.Mapper('--basedir',{
            'dredze':'real',
            'cfgroups1000':'real',
            }, default='kdeep', matchsubstrings=True).generator),
        ('--annotator-accuracy', broom.Mapper('--annotation-strategy',{
            'real':None,
            'kdeep':("FILE","LOW","EXPERT","CONFLICT"),
            #'kdeep':("HIGH","MED","LOW","CONFLICT"),
            }).generator),
        ('--annotator-file', broom.Mapper('--annotator-accuracy',{
            'FILE':("annotators/all","annotators/kmeans-5","annotators/kmeans-20","annotators/kmeans-50"),
            }, default=None).generator),
        ('-k', broom.Mapper('--annotation-strategy',{
            'real':None,
            'kdeep':(3),
            }).generator),

        # observed trusted labels
        ('--num-observed-labels',0),
        #('--num-observed-labels-per-annotator',2),
        #('--num-observed-labels-per-class',broom.Mapper('--basedir',{
        #    'cfgroups1000':None
        #}, default=10, matchsubstrings=True).generator),

        # inference
        #('--labeling-strategy',['momresp']), 
        #('--labeling-strategy',['rayktrunc','varrayk']), 
        #('--labeling-strategy',['rayktrunc','varrayk']), 
        #('--labeling-strategy',['varmomresp','varrayk']), 
        #('--labeling-strategy',['ubaseline','varmomresp','varrayk']), 
        #('--labeling-strategy',['ubaseline','varitemresp','varmomresp','varrayk']), 
        #('--labeling-strategy',['ubaseline','varitemresp','varmomresp','varrayk','rayktrunc']), 
        #('--labeling-strategy',['ubaseline','itemresp','momresp','multiresp','varitemresp','varmomresp','varmultiresp','rayktrunc','varrayk','cslda']), 
        #('--labeling-strategy','itemresp'), 
        #('--labeling-strategy',['cslda']), 
        ('--labeling-strategy',['cslda','ubaseline','varmomresp','varitemresp','varrayk']), 
        #('--labeling-strategy',['ubaseline','varmomresp']), 
        ('--num-topics',('20','100','500','1000')),
        ('--training',broom.Mapper('--labeling-strategy',{
            'cslda':'sample-all-500',
        },default='maximize-all').generator),
        ('--diagonalization-method',"GOLD"),
        ('--gold-instances-for-diagonalization',-1),
        #('--lambda',1),
        ('--training-percent', 85), 
        ('--validation-percent', 10), 
        ('--hyperparam-training', broom.Mapper('--labeling-strategy',{
            'cslda': ['maximize-bgamma+cgamma-GRID-1-itemresp-acc-maximize-all'],
            'momresp': ['maximize-bgamma+cgamma-GRID-1-itemresp-acc-maximize-all'],
            'itemresp': ['maximize-bgamma+cgamma-GRID-1'],
        },default='none',matchsubstrings=True).generator), 
        #('--truncate-unannotated-data', broom.Mapper('--labeling-strategy',{
        #    'multiresp':('',None), # run multiresp with and without this option
        #    'momresp':('',None), # run multiresp with and without this option
        #    }, default=None).generator),

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
            'ng':(1.+2.)/(20.+2.),
            'enron':(1.+2.)/(32.+2.), 
            'dredze':(1.+2.)/(2.+2.),
            'r8':(1.+2.)/(8.+2.),
            'r52':(1.+2.)/(52.+2.),
            'cade12':(1.+2.)/(12.+2.),
            'webkb':(1.+2.)/(4.+2.),
            }, default=-1, matchsubstrings=True).generator),
        #('--b-mu',broom.Mapper('--basedir',{
        #    'naivebayes-20':(1.+1.)/(20.+1.),
        #    'newsgroups':(1.+1.)/(20.+1.),
        #    'cfgroups1000':(1.+1.)/(20.+1.),
        #    'ng':(1.+1.)/(20.+1.),
        #    'enron':(1.+1.)/(50.+1.), # how many classes?
        #    'dredze':(1.+1.)/(2.+1.),
        #    'r8':(1.+1.)/(8.+1.),
        #    'r52':(1.+1.)/(52.+1.),
        #    'cade12':(1.+1.)/(12.+1.),
        #    'webkb':(1.+1.)/(4.+1.),
        #    #'dredze':0.65, # christophe prior
        #    }, default=-1, matchsubstrings=True).generator),

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
            'real': range(1,1+chains+repeats)
            }, matchsubstrings=True, default=range(1,1+chains)).generator),
        # output files
        ('--results-file',FileNamer(results_dir,'results').generator),
        #('--serialize-to-file',FileNamer(results_dir,'vars').generator),
        #('--debug-file',FileNamer(results_dir,'settings').generator),
        #('--tabular-file',FileNamer(results_dir,'tab').generator),

        # positional args
        inputfiles,
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
    headn = 50
    firstexperiment = 101

    outdir = '/tmp/bogus'
    #outdir = '/tmp/multiannresults'
    try:
        os.makedirs(outdir)
    except:
        pass
    print
    print '============================================'
    print '= first %d jobs' % headn
    print '============================================'
    for i,job in enumerate(jobs(firstexperiment,outdir,"4g")):
        if i<=headn:
            print 
            print job
        total = i
    print '\ntotal jobs=%d' % total

