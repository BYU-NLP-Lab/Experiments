import subprocess
import math
import os
import sys
import glob

######################################################
#                   Runtime settings
#####################################################
main = 'edu.byu.nlp.al.txt.FSLLearningCurve'
classpath = ['../../bin'] + glob.glob('../../lib/*.jar')
mem = 2000
java = "java -Xmx{mem}m -cp {classpath} {main}".format(mem=mem, classpath=':'.join(classpath), main=main)


######################################################
#                   Helper Functions
#####################################################
def parabolic_points(max,num):
    points = range(1, int(round(math.sqrt(max))), int(round(math.sqrt(max)/num)))
    return [p*p for p in points]

def kv2param(k,v):
    if v is None:
        param = "--%s" % str(k)
    elif len(str(k)) > 1:
        param = "--%s=%s" % (str(k), str(v))
    else:
        param = "-%s%s" % (str(k), str(v))	
    return param

def job(name,optional_params,positional_params,hours,mins):

    # job params
    param_str = " ".join(kv2param(k,v) for k,v in optional_params.iteritems())
    param_str = "%s %s" % (param_str, " ".join(positional_params))

    # shell command to start job
    dir = os.getcwd()
    cmd = "cd {dir} && {java} {param_str}".format(dir=dir, java=java, param_str=param_str)

    return cmd

def baseline_experiment_name(accuracy_level, corpus, annotation_strategy, labeling_strategy, eval_point, k, seed, feature_normalization_constant):
    return "{set}-{ann}-{lab}-{corp}-{acc}-n{pt}-k{k:02d}-fn{featnorm}-{seed:02d}".format(acc=accuracy_level, corp=corpus[1], set=corpus[2], ann=annotation_strategy, lab=labeling_strategy, pt=eval_point, k=k,seed=seed, featnorm=feature_normalization_constant) 

def experiment_name(accuracy_level, corpus, annotation_strategy, labeling_strategy, eval_point, k,labeled_doc_weight, unlabeled_doc_weight, seed, samples, feature_normalization_constant):
    return "{set}-{ann}-{lab}-{corp}-{acc}-n{pt}-k{k:02d}-x{samples}-dl{doclab}-du{docunlab}-fn{featnorm}-{seed:02d}".format(acc=accuracy_level, corp=corpus[1], set=corpus[2], ann=annotation_strategy, lab=labeling_strategy, pt=eval_point, k=k,doclab=labeled_doc_weight, docunlab=unlabeled_doc_weight, seed=seed, samples=samples, featnorm=feature_normalization_constant) 

def create_jobs(seed, samples, k, eval_point, labeled_doc_weight, unlabeled_doc_weight, annotation_strategy, labeling_strategy, hours, mins, corpus, accuracy_level, stage, annealing_schedule, baseline_results_dir, samples_dir, baseline_eval_points, feature_normalization_constant):
    # parameters that are common to stage 1 (baseline) and stage 2 (samples based on baseline initialization)
    opt_params = { 
          "k" : k,
          "annotation-strategy" : annotation_strategy,
          "labeling-strategy" : labeling_strategy,
          "accuracy-level" : accuracy_level,
          "eval-point" : eval_point,
          "basedir" : corpus[0],
          "dataset-type" : corpus[1],
          "dataset" : corpus[2],
          "samples" : samples,
          "data-seed" : seed,
          "labeled-document-weight" : labeled_doc_weight,
          "unlabeled-document-weight" : unlabeled_doc_weight,
          "predict-single-last-sample" : None,
          "use-label-switching-cheat" : None,
          "label-switching-cheat-full-confusion-matrix" : None, # we will eval on 0 annotated data, so we much do this or label switching kills our results
          "num-batches" : 1,
          "feature-count-cutoff" : 5,
          "annealing-schedule" : annealing_schedule,
        } 
    pos_params = []


    #print name
    if stage == 1:
        if "baseline" != labeling_strategy:
            raise Exception("it shouldn't be possible to run non-baselines in stage 1")
        else:
            feature_normalization_constant = -1 # no normalizing for baseline
            for baseline_eval_point in baseline_eval_points:
                name = baseline_experiment_name(accuracy_level, corpus, annotation_strategy, labeling_strategy, baseline_eval_point, k,seed, feature_normalization_constant) 
                opt_params["eval-point"] = baseline_eval_point
                # Run baseline
                opt_params["algorithm-seed"] = 1
                #opt_params["annotations-file"] = os.path.join(baseline_results_dir, name) + '-anns.csv'
                opt_params["results-file"] = os.path.join(baseline_results_dir, name) + '-results.csv'
                opt_params["debug-file"] = os.path.join(baseline_results_dir, name) + '-settings.csv'
                opt_params["serialize-to-file"] = os.path.join(baseline_results_dir, name) + '-vars.csv'
                if os.path.exists(opt_params["results-file"]):
                    print ".", # skipping existing result
                else:
                    yield job(name,opt_params,pos_params,hours,mins)

    elif stage == 2:
        basename = experiment_name(accuracy_level, corpus, annotation_strategy, labeling_strategy, eval_point, k,labeled_doc_weight, unlabeled_doc_weight, seed, samples, feature_normalization_constant) 
        # do multiple runs for each baseline setting
        for baseline_eval_point in baseline_eval_points:
            name = "%s-baseinit%s" % (basename,baseline_eval_point)
            #opt_params["annotations-file"] = os.path.join(samples_dir, name) + '-anns.csv'
            opt_params["feature-normalization-constant"] = feature_normalization_constant
            opt_params["results-file"] = os.path.join(samples_dir, name) + '-results.csv'
            opt_params["debug-file"] = os.path.join(samples_dir, name) + '-settings.csv'
            opt_params["serialize-to-file"] = os.path.join(samples_dir, name) + '-vars.csv'
            #opt_params["samples"] = 0 # don't sample
            if samples==0:
                opt_params["optimize-weights"] = None # do optimize

            # initialize with baseline run
            init_experiment = baseline_experiment_name(accuracy_level, corpus, annotation_strategy, 'baseline', baseline_eval_point, k, seed, -1) 
            pos_params = [os.path.join(baseline_results_dir, init_experiment)+'-vars.csv']

            if not os.path.exists(pos_params[0]):
                raise Exception("%s does not exist.\nRun stage 1 before stage 2." % pos_params[0])

            if os.path.exists(opt_params["results-file"]):
                print ".", # Skipping existing result

            yield job(name,opt_params,pos_params,hours,mins)
    else:
        raise(Exception("unknown stage: %s" % stage))




######################################################
#                   MAIN
#####################################################
def jobs(stage, first_experiment, baseline_results_dir, samples_dir):

    # require a single argument: 1 (for stage 1) or 2 (for stage 2)
    if stage < 1 or 2 < stage:
        sys.exit("Invalid stage %d (Valid stages are 1 to run baselines; 2 to run samples initialized by the each baseline eval_point)" % stage)

    if stage==1:
        labeling_strategies = [('baseline',-1,-1)]
    else:
        labeling_strategies = [('multiann',-1,-1)]
    #labeling_strategies = [
    #    # (strategy, labeledDocWeight, unlabeledDocWeight)
    #    #("baseline",-1,-1),
    #    #("ubaseline",-1,-1),
    #    #("multiann",'binary_classifier','binary_classifier'),
    #    #("multiann",-1,0), 
    #    ("multiann",-1,-1),
    #    #("itemresp",-1,-1),
    #    #("neutered",-1,-1)
    #]


    ######################################################
    #                   Experiments
    #####################################################
    single_eval_point = 0 # used in one-off experiments
    num_eval_points = 100 #parabolic_points(300000,25)
    random_runs = [1]
    annotation_strategy = "grr"
    krange = [5]

    corpora = [
        #("../../data/newsgroups","NEWSGROUPS","full_set",400000),
        ("../../data/enron","ENRON","ldc_split",100000),
        #("../../data/reuters21578","REUTERS","full_set",200000), 
    ]
    accuracy_levels = ["CROWD"]

    #corpora = [
    #    ("../../data/naivebayes-2","NB2","full_set",4000),
    #]
    #accuracy_levels = ["VERY_NOISY"]

    ### ICML top row (#1)
    ##annealing_schedule = "1000,500,200,100,50,20,10,5,2,1,1,1"
    ##num_samples = [250]
    ##feature_normalization_constant = -1

    # ICML top row (#2)
    annealing_schedule = "1000,500,200,100,50,20,10,5,2,1,1,1"
    num_samples = [250]
    feature_normalization_constant = 123

    #### ICML bottom row (#3)
    ###annealing_schedule = "1"
    ###num_samples = [0]
    ###feature_normalization_constant = -1

    #### ICML bottom row (#4)
    ###annealing_schedule = "1"
    ###num_samples = [0]
    ###feature_normalization_constant = 123

    ######################################################
    #                   Driver code
    #####################################################

    # Main
    num_eval_points = num_eval_points
    for random_run in random_runs:
        for labeling_strategy,labeled_doc_weight,unlabeled_doc_weight in labeling_strategies:
            for accuracy_level in accuracy_levels:
                for samples in num_samples:
                    for k in krange:
                        for corpus in corpora:
                            max_eval_point = corpus[3]
                            baseline_eval_points = parabolic_points(max_eval_point,num_eval_points)
                            eval_points = [single_eval_point] if single_eval_point>-1 else baseline_eval_points
                            for eval_point in eval_points:

                                # set run time
                                if "baseline" in labeling_strategy:
                                    hours,mins = 0,30
                                elif stage==2:
                                    hours,mins = 0,30
                                else:
                                    hours,mins = 0,30

                                for next_job in create_jobs(random_run, samples, k, eval_point, labeled_doc_weight, unlabeled_doc_weight, annotation_strategy, labeling_strategy, hours, mins, corpus, accuracy_level, stage, annealing_schedule, baseline_results_dir, samples_dir, baseline_eval_points, feature_normalization_constant):
                                    if next_job is not None:
                                        print next_job
                                        yield next_job
