#!/bin/sh

pushd ..
mvn clean dependency:copy-dependencies
popd

# 5 annotators
java -cp "../target/dependency/*" edu.byu.nlp.crowdsourcing.app.CrowdflowerAnnotatorParameterEstimator --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --smooth=0.01 --aggregate=KMEANS -k 5 --output=kmeans-5

# 10 annotators
java -cp "../target/dependency/*" edu.byu.nlp.crowdsourcing.app.CrowdflowerAnnotatorParameterEstimator --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --smooth=0.01 --aggregate=KMEANS -k 10 --output=kmeans-10

# 20 annotators
java -cp "../target/dependency/*" edu.byu.nlp.crowdsourcing.app.CrowdflowerAnnotatorParameterEstimator --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --smooth=0.01 --aggregate=KMEANS -k 20 --output=kmeans-20

# 50 annotators
java -cp "../target/dependency/*" edu.byu.nlp.crowdsourcing.app.CrowdflowerAnnotatorParameterEstimator --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --smooth=0.01 --aggregate=KMEANS -k 50 --output=kmeans-50

# all annotators
java -cp "../target/dependency/*" edu.byu.nlp.crowdsourcing.app.CrowdflowerAnnotatorParameterEstimator --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --smooth=0.01 --aggregate=NONE --output=all

