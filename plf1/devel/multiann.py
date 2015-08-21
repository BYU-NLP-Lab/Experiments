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
    'weather':1000,
    'airlines':16000,
    'companies':16000,
}
num_classes = {
    'newsgroups':20,
    'twitterparaphrase':2,
    'twittersentiment':2,
    'compatibility':3,
    'cade12':12,
    'enron':32,
    'dredze':2,
    'r8':8,
    'r52':52,
    'webkb':4,
    'weather':5,
    'airlines':3,
    'companies':6,
}
depth = 7
lowpoint = 200

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
    def generator(self,keys,state):
        name = "{dr}/{name}-{sfx}.csv".format(dr=self.directory,name=jobname(keys,state),sfx=self.suffix)
        if name in self.prevnames:
            oldkeys,oldstate = self.prevnames[name]
            raise Exception("two jobs received the same name! \n\t%s\n\t%s\n\t%s" % (name,oldstate,state))
        self.prevnames[name] = (keys,state)
        if len(path.basename(name))>255: # leave a few characters for filename suffix
            raise Exception("file name is longer than 255 characters: \n\t%s" % path.basename(name))
        yield name

class TopicsFileName():
    def __init__(self,directory):
        self.directory=directory
    def generator(self,keys,state):
        yield None

######################################################
#                   MAIN
#####################################################
def jobs(first_experiment, results_dir, topics_dir, mem):

    # java runtime
    main = 'edu.byu.nlp.al.app.CrowdsourcingLearningCurve'
    classpath = ['config','"target/dependency/*"'] 
    java = "java -Xmx{mem} -cp {classpath} {main}".format(mem=mem, classpath=':'.join(classpath), main=main)
    #javacommand = 'cd {cwd} && {java}'.format(cwd=os.getcwd(), java=java)
    #cwd = "/net/perplexity/plf1/git/Experiments/plf1/cslda-paper"
    cwd = "/local/plf1/git/Experiments/plf1/cslda-paper"
    javacommand = 'cd {cwd} && {java}'.format(cwd=cwd, java=java)

    num_evalpoints = 8
    repeats = 5
    chains = 1 # TODO: consider more for sampling runs
    #datadir = "/net/perplexity/plf1/data"
    datadir = "/local/plf1/data"

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
            #'data/cfsimplegroups1000a',
            #'data/cfsimplegroups1000b',
            #'data/cfsimplegroups1000c',
            #'data/cfsimplegroups1000d',
            #'%s/jsonbasedirs/twitterparaphrase-twit'%datadir,
            #'%s/jsonbasedirs/twitterparaphrase-w2v'%datadir,
            #'%s/jsonbasedirs/twitterparaphrase-d2v'%datadir,
            #'%s/jsonbasedirs/twittersentiment-twit'%datadir,
            #'%s/jsonbasedirs/twittersentiment-w2v'%datadir,
            #'%s/jsonbasedirs/twittersentiment-d2v'%datadir,
            #'%s/jsonbasedirs/compatibility-w2v'%datadir,
            '%s/jsonbasedirs/weather'%datadir,
            #'%s/jsonbasedirs/weather-w2v'%datadir,
            #'%s/jsonbasedirs/weather-d2v'%datadir,
            #'%s/jsonbasedirs/cfgroups1000'%datadir,
            #'%s/jsonbasedirs/cfgroups1000-w2v'%datadir,
            #'%s/jsonbasedirs/cfgroups1000-d2v'%datadir,
            #'%s/jsonbasedirs/newsgroups'%datadir,
            #'%s/jsonbasedirs/newsgroups-d2v'%datadir,
            #'%s/jsonbasedirs/newsgroups-w2v'%datadir,
            #'data/airlines-twit',
            #'data/dredze-twit',
            #'data/companies',
            #'data/enron',
            #'data/r8',
            #'data/webkb',
            #'data/cade12',
            #'data/r52',
            )),
        ('--dataset-type', broom.Mapper('--basedir',{
            '.*naivebayes-20':'NB20',
            '.*multiresp-2':'NB2',
            '.*newsgroups$':'NEWSGROUPS',
            '.*newsgroups-[wd]2v$':'INDEXED_VEC',
            '.*(?<!newsgroups)-[wd]2v':'JSON_VEC',
            '.*-lda':'JSON_VEC',
            '.*cfgroups1000$':'CFGROUPS1000',
            '.*weather$':'WEATHER',
            '.*-twit':'TWITTER', 
            '.*companies':'COMPANIES',
            '.*enron':'ENRON',
            '.*cade12':'CADE12',
            '.*r8':'R8',
            '.*r52':'R52',
            '.*webkb':'WEBKB',
            }).generator),
        ('--dataset', broom.Mapper('--basedir',{
            '.*twitterparaphrase-twit':'%s/twitterparaphrase/dataset/twitterparaphrase.json'%datadir,
            '.*twitterparaphrase-w2v':'%s/twitterparaphrase/dataset/twitterparaphrase-w2v.json'%datadir,
            '.*twitterparaphrase-d2v':'%s/twitterparaphrase/dataset/twitterparaphrase-d2v.json'%datadir,
            '.*twittersentiment-twit':'%s/twittersentiment/dataset/twittersentiment.json'%datadir,
            '.*twittersentiment-w2v':'%s/twittersentiment/dataset/twittersentiment-w2v.json'%datadir,
            '.*twittersentiment-d2v':'%s/twittersentiment/dataset/twittersentiment-d2v.json'%datadir,
            '.*compatibility-w2v':'%s/compatibility/dataset/compatibility-w2v.json'%datadir,
            '.*cfgroups1000.*':'%s/cfgroups/cfgroups1000.json'%datadir,
            '.*weather$':'%s/weather/dataset/weather.json'%datadir,
            '.*weather-w2v':'%s/weather/dataset/weather-w2v.json'%datadir,
            '.*weather-d2v':'%s/weather/dataset/weather-d2v.json'%datadir,
            '.*airlines':'/aml/data/plf1/airlines/airlines.json',
            '.*companies':'/aml/data/plf1/companies/companies.json',
            '.*cfsimplegroups1000a':'/aml/data/plf1/cfgroups/derived/cfsimplegroups1000a.json',
            '.*cfsimplegroups1000b':'/aml/data/plf1/cfgroups/derived/cfsimplegroups1000b.json',
            '.*cfsimplegroups1000c':'/aml/data/plf1/cfgroups/derived/cfsimplegroups1000c.json',
            '.*cfsimplegroups1000d':'/aml/data/plf1/cfgroups/derived/cfsimplegroups1000d.json',
            '.*enron':'ldc_split',
            '.*dredze':'/aml/data/plf1/dredze/derived/1v0.json',
            },default='full_set').generator),
        ('--eval-point', broom.Mapper('--basedir',{
            '.*naivebayes-20':parabolic_points(lowpoint,data_size['newsgroups']*depth,num_evalpoints),
            '.*weath.*':parabolic_points(lowpoint,22000,num_evalpoints),
            '.*airlines':parabolic_points(lowpoint,65000,num_evalpoints),
            '.*companies':parabolic_points(lowpoint,30000,num_evalpoints),
            '.*newsgroups.*':parabolic_points(lowpoint,data_size['newsgroups']*depth,num_evalpoints),
            '.*cfgroups1000':parabolic_points(lowpoint,10000,num_evalpoints),
            '.*twitterparaphrase.*':parabolic_points(lowpoint,22000,num_evalpoints),
            '.*twittersentiment.*':parabolic_points(lowpoint,6000,num_evalpoints),
            '.*compatibility.*':parabolic_points(lowpoint,200000,num_evalpoints),
            '.*dredze':parabolic_points(lowpoint,21000,num_evalpoints),
            '.*cade12':parabolic_points(lowpoint,data_size['cade12']*depth,num_evalpoints),
            '.*enron':parabolic_points(lowpoint,data_size['enron']*depth,num_evalpoints),
            '.*r8':parabolic_points(lowpoint,data_size['r8']*depth,num_evalpoints),
            '.*r52':parabolic_points(lowpoint,data_size['r52']*depth,num_evalpoints),
            '.*webkb':parabolic_points(lowpoint,data_size['webkb']*depth,num_evalpoints),
            }).generator),
        ('--feature-normalization-constant',broom.Mapper('--basedir',{
            '.*dredze':20,
            '.*weather$':20,
            '.*airlines':20,
            '.*companies':20,
            '.*twitter.*':20,
            '.*r8':60,
            '.*r52':60,
            }, default=100).generator),

        # aggresive feature selection
        ('--feature-count-cutoff',broom.Mapper('--dataset-type',{
            'TWITTER': -1,
            '.*_VEC': -1,
            'WEATHER': -1,
        }, default=5).generator),
        #('--top-n-features-per-document',-1),
        ('--top-n-features-per-document',broom.Mapper('--dataset-type',{
            'COMPANIES': 3,
        }, default=-1).generator),

        # annotations
        ('--annotation-strategy', broom.Mapper('--basedir',{
            '.*dredze':'reallayers',
            '.*weather.*':'real',
            #'weath':'reallayers',
            '.*airlines':'reallayers',
            '.*companies':'reallayers',
            '.*cfgroups1000':'real',
            '.*twitterparaphrase*':'real',
            '.*twittersentiment*':'reallayers',
            '.*compatibility*':'reallayers',
            #}, default='grr').generator),
            }, default='kdeep').generator),
        ('--annotator-accuracy', broom.Mapper('--basedir',{
            #'.*newsgroups.*':"FILE",
            '.*cfgroups1000.*':None,
            '.*twitter.*':None,
            '.*dredze':None,
            '.*weath.*':None,
            '.*airlines':None,
            '.*companies':None,
            #'kdeep':("FILE","LOW","EXPERT","CONFLICT"),
            #'kdeep':("HIGH","MED","LOW","CONFLICT"),
            }, default=("CFBETA")).generator),
        ('--annotator-file', broom.Mapper('--annotator-accuracy',{
            'FILE':"annotators/all",
            #'FILE':("annotators/all","annotators/kmeans-5","annotators/kmeans-20"),
            #'FILE':("annotators/all","annotators/kmeans-5","annotators/kmeans-20","annotators/kmeans-50"),
            }, default=None).generator),
        ('-k', broom.Mapper('--annotation-strategy',{
            'real.*':None,
            #}, default=1).generator),
            }, default=depth).generator),
        #('--annotate-top-k-choices', broom.Mapper('--annotation-strategy',{
        #    'real':None,
        #    #}, default=1).generator),
        #    }, default=(None,3)).generator),
        #('--num-annotator-clusters', broom.Mapper('--annotation-strategy',{
        #    'real':-1,
        #    #'real':(5,20,-1),
        #    }, default=None).generator),
        #('--cluster-method', broom.Mapper('--annotation-strategy',{
        #    'real':('KM_MV','KM_GOLD'),
        #    }, default=None).generator),

        # observed trusted labels
        ('--num-observed-labels',0),
        #('--num-observed-labels-per-annotator',2),
        #('--num-observed-labels-per-class',broom.Mapper('--basedir',{
        #    '.*cfgroups1000':None
        #}, default=10).generator),

        # inference #('--labeling-strategy',['MOMRESP']), #('--labeling-strategy',['LOGRESP','VARLOGRESP']), 
        ('--labeling-strategy',broom.Mapper('--basedir',{
            '.*compatibility-w2v$': ('UBASELINE','VARITEMRESP','VARLOGRESP','DISCRIM'), # has no suitable lexical representation, so we only eval w2v
            '.*(?<!compatibility)-[wd]2v$': ('VARLOGRESP','DISCRIM'), # catch all the other w2v,d2v cases (momresp,cslda, make no sense here)
        },default=['UBASELINE','CSLDA','CSLDAP','CSLDALEX','DISCRIM','VARMOMRESP','VARLOGRESP','VARITEMRESP']).generator), 
        #},default=['UBASELINE','CSLDA','CSLDAP','CSLDALEX','DISCRIM','VARMOMRESP','VARLOGRESP','VARITEMRESP']).generator), 
        ('--initialization-strategy','BASELINE'),
        ('--num-topics',broom.Mapper('--basedir',{
            '.*naivebayes-20': int(round(1.5*num_classes['newsgroups'])),
            '.*newsgroups.*': int(round(1.5*num_classes['newsgroups'])),
            '.*cfgroups1000.*': int(round(1.5*num_classes['newsgroups'])),
            '.*twitterparaphrase.*': int(round(1.5*num_classes['twitterparaphrase'])),
            '.*twittersentiment.*': int(round(1.5*num_classes['twittersentiment'])),
            '.*dredze': int(round(1.5*num_classes['dredze'])),
            '.*cade12': int(round(1.5*num_classes['cade12'])),
            '.*enron': int(round(1.5*num_classes['enron'])),
            '.*r8': int(round(1.5*num_classes['r8'])),
            '.*r52': int(round(1.5*num_classes['r52'])),
            '.*webkb': int(round(1.5*num_classes['webkb'])),
            '.*weath.*': int(round(1.5*num_classes['weather'])),
            '.*airlines.*': int(round(1.5*num_classes['airlines'])),
            '.*companies.*': int(round(1.5*num_classes['companies'])),
        }, default=-1).generator),
        ('--training',broom.Mapper('--labeling-strategy',{
            'CSLDA$':('sample-z-500:sample-all-900:sample-all-100'),
            'CSLDALEX':('sample-z-500:sample-all-900:sample-all-100'),
            'CSLDAP$':('sample-z-500:maximize-y'),
            'ITEMRESP':('maximize-all'),
        },default='maximize-all').generator),
        ('--diagonalization-method',"GOLD"),
        ('--gold-instances-for-diagonalization',-1),
        #('--lambda',1),
        ('--training-percent', 100), 

        #('--inline-hyperparam-tuning', ('',None)),
        ('--inline-hyperparam-tuning'),

        #('--vary-annotator-rates',broom.Mapper('--annotator-accuracy',{
        #    'FILE': ('',None),
        #},default=None).generator),

        ('--validation-percent', 0), 

        # weak priors
        ('--b-theta',broom.Mapper('--labeling-strategy',{
            'CSLDA.*': 0.1,
        }, default=1.0).generator),
        ('--b-phi','0.1'),
        ('--b-mu','0.1'),
        ('--c-mu','1'),
        ('--c-gamma','1'),
        #('--eta-variance',(1,0.1)),
        ## weak gamma prior #1 (static)
        #('--b-gamma','0.1'),
        # weak gamma prior #2 (scale with num-class)
        # Accuracies depend on the number of class labels.
        # To be somewhat general/uninformed, let's set accuracy
        # to be delta better than random (1+delta)/(numclasses+delta)
        ('--b-gamma',broom.Mapper('--basedir',{
            '.*newsgroups.*':(1.+2.)/(num_classes['newsgroups']+2.),
            '.*cfgroups.*':(1.+2.)/(num_classes['newsgroups']+2.),
            '.*cfsimplegroups1000a':(1.+2.)/(10.+2.),
            '.*cfsimplegroups1000b':(1.+2.)/(num_classes['newsgroups']+2.),
            '.*cfsimplegroups1000c':(1.+2.)/(num_classes['newsgroups']+2.),
            '.*cfsimplegroups1000d':(1.+2.)/(2.+2.),
            '.*twitterparaphrase.*':(1.+2.)/(num_classes['twitterparaphrase']+2.),
            '.*twittersentiment.*':(1.+2.)/(num_classes['twittersentiment']+2.),
            '.*compatibility.*':(1.+2.)/(num_classes['compatibility']+2.),
            '.*weather.*':(1.+2.)/(num_classes['weather']+2.),
            '.*airlines':(1.+2.)/(num_classes['airlines']+2.),
            '.*companies':(1.+2.)/(num_classes['companies']+2.),
            '.*ng':(1.+2.)/(num_classes['newsgroups']+2.),
            '.*enron':(1.+2.)/(num_classes['enron']+2.), 
            '.*dredze':(1.+2.)/(num_classes['dredze']+2.),
            '.*r8':(1.+2.)/(num_classes['r8']+2.),
            '.*r52':(1.+2.)/(52.+2.),
            '.*cade12':(1.+2.)/(12.+2.),
            '.*webkb':(1.+2.)/(4.+2.),
            }, default=-1).generator),
        #('--b-mu',broom.Mapper('--basedir',{
        #    '.*newsgroups':(1.+1.)/(20.+1.),
        #    '.*cfgroups1000':(1.+1.)/(20.+1.),
        #    '.*ng':(1.+1.)/(20.+1.),
        #    '.*enron':(1.+1.)/(50.+1.), # how many classes?
        #    '.*dredze':(1.+1.)/(2.+1.),
        #    '.*r8':(1.+1.)/(8.+1.),
        #    '.*r52':(1.+1.)/(52.+1.),
        #    '.*cade12':(1.+1.)/(12.+1.),
        #    '.*webkb':(1.+1.)/(4.+1.),
        #    #'.*dredze':0.65, # christophe prior
        #    }, default=-1).generator),

        # random runs and chains
        ('--data-seed', broom.Mapper('--annotation-strategy',{
            # for real data, data/annotations are predetermined so all randomness must come from algorithm
            'real': first_experiment+1,
            }, 
            default=range(first_experiment+1,first_experiment+1+repeats)).generator),
        #('--algorithm-seed', broom.Range(1,1+chains).generator), # chains
        ('--algorithm-seed', broom.Mapper('--annotation-strategy',{
            # for real data, all randomness must come from algorithm
            'real': range(1,1+(chains*repeats))
            }, default=range(1,1+chains)).generator),
        # output files
        ('--results-file',FileNamer(results_dir,'results').generator),
        #('--serialize-to-file',FileNamer(results_dir,'vars').generator),
        #('--debug-file',FileNamer(results_dir,'settings').generator),
        #('--tabular-file',FileNamer(results_dir,'tab').generator),

        # positional args
        TopicsFileName(topics_dir).generator,
        #inputfiles,
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
    headn = 5000
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

