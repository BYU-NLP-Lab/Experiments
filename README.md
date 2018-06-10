This repo archives the driver scripts used to produce results for the following papers:

# NAACL-2015

Paper: "Early Gains Matter: A Case for Preferring Generative over Discriminative Crowdsourcing Models

Link: http://www.aclweb.org/anthology/N15-1089

Notes: unfortunately, due to maven servers being retired running this code may require some manual dependency resolution (see source code section below).

# CONLL-2015-CSLDA

Paper: "Making the Most of Crowdsourced Document Annotations: Confused Supervised LDA"

Link: http://www.aclweb.org/anthology/K15-1020

Notes: unfortunately, due to maven servers being retired running this code may require some manual dependency resolution (see source code section below).

# COLING-2018-Measurement

Paper: "Learning from Measurements in Crowdsourcing Models:Inferring Ground Truth from Diverse Annotation Types"

Link: in press

Notes: This project should be operational out of the box. 

# Source Code:

To get and run the code ecosystem and data associated with all of these projects, run
`./byunlp-clone.sh`
and then import the resulting projects with pom files as maven projects in Spring Tools Suite (https://spring.io/tools/sts), which is basically eclipse with lots of nice addons including maven integration. You could try using eclipse with maven plugins, but it works much more smoothly with STS.

Note: this code base is no longer being supported or maintained in any way. Feel free to borrow anything you like, but without a warrantee. 


## Background

This repo is a place for developing and archiving 
the code necessary to use archived jars 
to reproduce previous results. The intent is that anybody in 
the future at any time can clone this repo, find the 
folder corresponding to the paper they are interested in, 
and follow the directions there to exactly reproduce the 
results reported in that paper. 

folder structure:
  [username]/: a folder for ongoing development by username 
    (a good place for work in progress, failed paper submissions, etc.)
  [papername]/: an archival set of experiments/scripts corresponding
    to papers that have been published. 

Good to commit:
* READMEs explaining a manual pipeline of scripts to run
* experiment driver scripts
* result collections scripts
* plotting scripts
* parsimonious csv files summarizing important published results

Bad to commit:
* large files
* dependencies on jars or projects that are not archived 
  in our maven repo. 
* raw results or log files.
