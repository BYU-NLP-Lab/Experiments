# untar newsgroups (included here because it's in the format that the scripts expect)
tar -xzf newsgroups.tar.gz
# symlink other projects (crowdflower-newsgroups, crowdflower-weather), assuming they are siblings to the Experiments project
ln -s ../../../crowdflower-newsgroups cfgroups
ln -s ../../../crowdflower-weather weather
