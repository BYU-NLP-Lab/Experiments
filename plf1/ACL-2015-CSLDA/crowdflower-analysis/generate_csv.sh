#!/bin/sh

pushd ..
mvn clean dependency:copy-dependencies
popd

# convert json file into csv with 1 row per annotation
java -cp "../target/dependency/*" edu.byu.nlp.data.app.AnnotationStream2Csv --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --row=ANNOTATION --out=cfgroups1000-annotations.csv
# convert json file into csv with 1 row per instance
java -cp "../target/dependency/*" edu.byu.nlp.data.app.AnnotationStream2Csv --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --row=INSTANCE --out=cfgroups1000-instances.csv
# convert json file into csv with 1 row per annotator
java -cp "../target/dependency/*" edu.byu.nlp.data.app.AnnotationStream2Csv --json-stream=/aml/data/plf1/cfgroups/cfgroups1000.json --row=ANNOTATOR --out=cfgroups1000-annotators.csv

#java -cp "../target/dependency/*" edu.byu.nlp.data.app.AnnotationStream2Csv --json-stream=/aml/data/plf1/cfgroups/cfsimplegroups1000a.json --row=ANNOTATION --out=cfsimplegroups1000a-annotations.csv
