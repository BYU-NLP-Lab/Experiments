#!/bin/sh

#################################
# The following are interdependent java maven projects, 
# typically imported into eclipse via m2e.
#################################

# Utility libraries
git clone https://github.com/BYU-NLP-Lab/AnnotationInterface.git
git clone https://github.com/BYU-NLP-Lab/Utilities.git
git clone https://github.com/BYU-NLP-Lab/jargparser.git

# Application libraries
git clone https://github.com/BYU-NLP-Lab/Classification.git
git clone https://github.com/BYU-NLP-Lab/ActiveLearning.git
git clone https://github.com/BYU-NLP-Lab/Crowdsourcing.git

# crowdsourced data
git clone https://github.com/BYU-NLP-Lab/crowdflower-newsgroups.git
git clone https://github.com/BYU-NLP-Lab/crowdflower-weather.git

#################################
# The following is a maven project typically 
# worked with via the command line. 
#################################

# Concrete experiments organized by the paper they contributed to
git clone https://github.com/BYU-NLP-Lab/Experiments.git

# utility scripts used by Experiments.git 
# (you may need to update sym links in Experiments to point here)
git clone https://github.com/paul-felt/broom-py.git
