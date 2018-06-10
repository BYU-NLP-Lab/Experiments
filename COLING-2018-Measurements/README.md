This directory contains the experiments used to generate learning curves data and plots

get log joint values:
    cat $1 | grep LogJoint | cut -d= -f4
pom.xml
    defines job dependencies. Do mvn clean dependency:copy-dependencies to get them all into target/dependency/*
submit.py
    Submits jobs to fsl and writes output to results/
aggregate.sh
    usage ./aggregate results > results.csv
    Takes a header from one file, and concatenates the body of each csv file in results into a single csv file
learningcurves.R
    Plots the resulting concatenate csv
ssh_submit.py
    Runs jobs on the potatos using the jobfarm.py script:
    ./ssh_submit.py -n potatos -o out -r results/tst --stage=2
    Dry run with simulated errors:
    ./ssh_submit.py --test -n potatos -o out -r results/tst --stage=2 -c


To prepare your machine(s):
1) get java libraries in place (target/dependency) simply by cloning the Experiments repo onto place on all the machines on which jobs will be run (the 'potatos' file). In the past we'd use maven to do this, but now that maven server has gone away, and we've simply committed a working version of all dependency jars into place. If you need to change the code, you'll have to do a maven install in eclipse, and then copy the resulting jar manually into place in target/dependency
2) pip install paramiko
3) set up keyless ssh between your machine and all the hosts in the file 'potatos'. Add more hostnames machines there if you wish to run on more machines.
4) Optional: To get and edit source code, run ./

--------To generate simulated newsgroups results-----------
# link multiann.py to multiann-newsgroups-simulation.py 
  `rm multiann.py; ln -s multiann-newsgroups-simulation.py multiann.py`
# submit jobs to a job cluster defined in the file 'potatos', one line per host. By default it only points to localhost, but add other machines to farm the jobs out further. For this to work, keyless ssh must be set up on the machines (this includes the default localhost)
  `PYTHONPATH=$PYTHONPATH:../../broom-py ./ssh_submit.py -n potatos -o /tmp/multiann-newsgroups-output -r /tmp/multiann-newsgroups-results`
# gather up the results from the job cluster and write them to a single csv file. This will generate some ignorable errors
  `./gather.sh potatos /tmp/multiann-newsgroups-results; ./cleanup.sh /tmp/multiann-newsgroups-results; ./:aggregate.sh /tmp/multiann-newsgroups-results > /tmp/multiann-newsgroups.csv`

---------To generate the weather-preds results with real annotation order--------------
# link multiann.py to multiann-weather-preds.py 
  `rm multiann.py; ln -s multiann-weather-preds.py multiann.py`
# submit jobs to a job cluster defined in the file 'potatos', one line per host. By default it only points to localhost, but add other machines to farm the jobs out further. For this to work, keyless ssh must be set up on the machines (this includes the default localhost)
  `PYTHONPATH=$PYTHONPATH:../../broom-py ./ssh_submit.py  -n potatos -o /tmp/multiann-weather-output -r /tmp/multiann-weather-results`
# gather up the results from the job cluster and write them to a single csv file. This will generate some ignorable errors
  `./gather.sh potatos /tmp/multiann-weather-results; ./cleanup.sh /tmp/multiann-weather-results; ./aggregate.sh /tmp/multiann-weather-results > /tmp/multiann-weather.csv`

----------To run weather active learning experiment: you'll need to run-----------
java -Xmx4g -cp config:"target/dependency/*" edu.byu.nlp.al.app.CrowdsourcingActiveMeasurement \
--min-candidates=25 \
--batch-size=10 \
--num-samples=1 \
--dataset-type=WEATHER \
--dataset=/home/plfelt/git/crowdflower-weather/dataset/weather-preds.json \
--eval-point=200 \
--meas-eval-point=200 \
--annotation-strategy=real \
--labeling-strategy=PAN \
--initialization-strategy=BASELINE \
--training=maximize-all \
--training-percent=100 \
--data-seed=1 \
--algorithm-seed=1 \
--results-file=/tmp/multiann-weather-AL.csv \
--annotations-file=/tmp/multiann-weather-AL-anns \
--tabular-file=/tmp/multiann-weather-AL-tabular \

Then look in /tmp/multiann-weather-AL.csv for results to be printed out. This is not parallelizable and just runs on one machine. Takes a very long time. 
Params of interest include 
  --min-candidates (how many candidates are scored each round)
  --batch-size (how many candidates are chosen each round)
  --num-samples (how many samples are taken for each candidate each round)
  --eval-point (how many random annotations to start AL with)
  --meas-eval-point (how many random non-annotation measurements to start AL with)
  --annotations-file (lists the annotations in the order they are chosen)
  --results-file (tracks labeled_accuracy and other statistics as AL proceeds)