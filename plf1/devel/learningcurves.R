## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   dataframe: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(dataframe=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(dataframe, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # pfelt modification: all instances with N=1 will have NAs in the sd
#   datac <- datac[which(datac$N>1),] # remove rows
#   datac[which(datac$N==1),]$sd <- 1 # set sd to 0
#   datac[which(datac$N==1),]$se <- 1
#   datac[which(datac$N==1),]$ci <- 1
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

isOptimization <- function(dat){
  return(grepl("maximize", dat$training))
}

hasHyperTuning <- function(tuningMethod){
  function(dat){
    return(grepl(tuningMethod, dat$hyperparam_training))
  }
}

usesVectors <- function(dat){
  grepl(".*(-lda|-w2v|-d2v).*",dat$basedir)
}

usesLdaVectors <- function(dat){
  grepl(".*-lda.*",dat$basedir)
}

usesWord2VecVectors <- function(dat){
  grepl(".*-w2v.*",dat$basedir)
}

usesDoc2VecVectors <- function(dat){
  grepl(".*-d2v.*",dat$basedir)
}

isLabelingStrategy <- function(labeling_strategy){
  function(dat){
    desiredLabelingStrategy <- if(is.null(labeling_strategy)) dat$labeling_strategy else labeling_strategy
    return(dat$labeling_strategy==desiredLabelingStrategy)
  }
}

not <- function(f){
  function(dat){
    return(!f(dat))
  } 
}

and <- function(...){
  function(dat){
    criteria <- list(...)
    # start with everything matched
    matchedRows <- rep(TRUE,dim(dat)[1])
    # intersection of rows that match each criterion
    for (criterion in criteria){
      matchedRows <- matchedRows & criterion(dat)
    }
    return(matchedRows)
  }
}

nameRows <- function(dat,name,criterion){
  matchedRows <- criterion(dat)
  # add algorithm name to matched rows
  dat$algorithm[which(matchedRows)] <- name
  return(dat)
}


massageData <- function(dat){
  dat$labeled_acc = as.numeric(as.character(dat$labeled_acc))
  dat$heldout_acc = as.numeric(as.character(dat$heldout_acc))
  dat$top3_labeled_acc = as.numeric(as.character(dat$top3_labeled_acc))
  dat$top3_heldout_acc = as.numeric(as.character(dat$top3_heldout_acc))
  if (is.null(dat$dataset_type)){
    dat$dataset_type <- dat$corpus
    dat$corpus <- NULL
  }
  
  num_rows = dim(dat)[1]
  
  # add truncate_unannotated_data=false if it doesn't exist
  dat$truncate_unannotated_data <- if (is.null(dat$truncate_unannotated_data)) rep('false',num_rows) else dat$truncate_unannotated_data
  
  # add basedir<-dataset_source if basedir doesn't exist
  dat$basedir <- as.character(dat$basedir)
  emptyrows <- which(is.na(dat$basedir))
  dat$basedir[emptyrows] <- as.character(dat$dataset_source[emptyrows])
  dat$basedir <- as.factor(dat$basedir)
  
  ########## derive 'algorithm' factor ##################
  dat$algorithm <- rep("invalid",num_rows)
  
  # baselines
  dat <- nameRows(dat, 'baseline', and(isLabelingStrategy('UBASELINE')))
  
  # itemresp variants
  dat <- nameRows(dat, 'itemresp_s', and(isLabelingStrategy('ITEMRESP'), not(isOptimization), not(usesVectors)))
  dat <- nameRows(dat, 'itemresp_m', and(isLabelingStrategy('ITEMRESP'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'varitemresp', and(isLabelingStrategy('VARITEMRESP'), isOptimization, not(usesVectors))) 
  dat <- nameRows(dat, 'varitemresp_w2v', and(isLabelingStrategy('VARITEMRESP'), isOptimization, usesWord2VecVectors))
  dat <- nameRows(dat, 'varitemresp_d2v', and(isLabelingStrategy('VARITEMRESP'), isOptimization, usesDoc2VecVectors))
  
  # neutered variants
  dat <- nameRows(dat, 'momresp_s', and(isLabelingStrategy('MOMRESP'), not(isOptimization), not(usesVectors)))
  dat <- nameRows(dat, 'momresp_m', and(isLabelingStrategy('MOMRESP'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'varmomresp', and(isLabelingStrategy('VARMOMRESP'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'varmomresp_w2v', and(isLabelingStrategy('VARMOMRESP'), isOptimization, usesWord2VecVectors))
  dat <- nameRows(dat, 'varmomresp_d2v', and(isLabelingStrategy('VARMOMRESP'), isOptimization, usesDoc2VecVectors))
  
  # multiresp variants
  dat <- nameRows(dat, 'multiresp_s', and(isLabelingStrategy('MULTIRESP'), not(isOptimization), not(usesVectors)))
  dat <- nameRows(dat, 'multiresp_m', and(isLabelingStrategy('MULTIRESP'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'varmultiresp', and(isLabelingStrategy('VARMULTIRESP'), isOptimization, not(usesVectors)))
  
  # logresp variants
  dat <- nameRows(dat, 'logresp_st', and(isLabelingStrategy('LOGRESP_ST'), isOptimization, not(usesVectors))) # self training
  dat <- nameRows(dat, 'logresp_m', and(isLabelingStrategy('LOGRESP'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'varlogresp', and(isLabelingStrategy('VARLOGRESP'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'varlogresp_w2v', and(isLabelingStrategy('VARLOGRESP'), isOptimization, usesWord2VecVectors))
  dat <- nameRows(dat, 'varlogresp_d2v', and(isLabelingStrategy('VARLOGRESP'), isOptimization, usesDoc2VecVectors))
  dat <- nameRows(dat, 'varlogresp_lda', and(isLabelingStrategy('VARLOGRESP'), isOptimization, usesLdaVectors))
  
  # cslda
  dat <- nameRows(dat, 'cslda_s', and(isLabelingStrategy('CSLDA'), not(isOptimization), not(usesVectors)))
  dat <- nameRows(dat, 'cslda', and(isLabelingStrategy('CSLDA'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'cslda_lex_s', and(isLabelingStrategy('CSLDALEX'), not(isOptimization), not(usesVectors)))
  dat <- nameRows(dat, 'cslda_lex', and(isLabelingStrategy('CSLDALEX'), isOptimization, not(usesVectors)))
  dat <- nameRows(dat, 'cslda_p', and(isLabelingStrategy('CSLDAP'), isOptimization, not(usesVectors))) # pipelined
  dat <- nameRows(dat, 'cslda_s_p', and(isLabelingStrategy('CSLDAP'), not(isOptimization))) # pipelined w sampler
  
  # fully discriminative 
  dat <- nameRows(dat, 'discrim', and(isLabelingStrategy('DISCRIM'), isOptimization, not(usesLdaVectors)))
  dat <- nameRows(dat, 'discrim_lda', and(isLabelingStrategy('DISCRIM'), isOptimization, usesLdaVectors))
  dat <- nameRows(dat, 'discrim_d2v', and(isLabelingStrategy('DISCRIM'), isOptimization, usesDoc2VecVectors))
  dat <- nameRows(dat, 'discrim_w2v', and(isLabelingStrategy('DISCRIM'), isOptimization, usesWord2VecVectors))
  
  # make 'algorithm' into factor 
  dat$algorithm <- factor(dat$algorithm)
  
  
  ########## derive 'corpus' factor ##################
  dat$corpus <- rep("OTHER",num_rows)
  
  # combine weather,weather-w2v,weather-d2v
  if (any(grepl("weather",dat$basedir))){
    dat[which(grepl("weather",dat$basedir)),]$corpus <- "WEATHER"
  }
  
  # combine newsgroups,newsgroups-w2v,newsgroups-d2v
  if (any(grepl("newsgroups",dat$basedir))){
    dat[which(grepl("newsgroups",dat$basedir)),]$corpus <- "NEWSGROUPS"
  }
  
  # combine cfgroups1000,cfgroups1000-w2v,cfgroups1000-d2v
  if (any(grepl("cfgroups1000",dat$basedir))){
    dat[which(grepl("cfgroups1000",dat$basedir)),]$corpus <- "CFGROUPS1000"
  }
  
  # combine twitterparaphrase,twitterparaphrase-w2v,twitterparaphrase-d2v
  if (any(grepl("twitterparaphrase",dat$basedir))){
    dat[which(grepl("twitterparaphrase",dat$basedir)),]$corpus <- "TWITTER_PARA"
  }
  
  # combine twittersentiment,twittersentiment-w2v,twittersentiment-d2v
  if (any(grepl("twittersentiment",dat$basedir))){
    dat[which(grepl("twittersentiment",dat$basedir)),]$corpus <- "TWITTER_SENT"
  }
  
  # combine compatibility experiments
  if (any(grepl("twittersentiment",dat$basedir))){
    dat[which(grepl("compatibility",dat$basedir)),]$corpus <- "COMPATIBILITY"
  }
  
  # treat simplified cfgroups as its own corpus
  dat$corpus[which(dat$dataset=="cfsimplegroups1000a.json")] <- "CFSIMPLEGROUPS"
  
  # make 'corpus' into factor
  dat$corpus <- factor(dat$corpus)
  
  
  ########## miscellaneous ##################
  
  # name num_annotators into a factor (so it can be used as a plotting facet)
  if (!is.null(dat$num_annotators)){
    dat$num_annotators <- factor(dat$num_annotators)
  }
  
  # rename k to 'd' and re-order
  require(plyr)
  dat$d <- sprintf("d = %g",dat$k)
  dat$d <- factor(dat$d, levels = c('d = 1','d = 2','d = 3','d = 5','d = 10'))
  
  # prettify factor names
  if (!is.null(dat$inline_hyperparam_tuning)){
    dat$tuning <- sprintf("Hyperparam Tuning = %s",dat$inline_hyperparam_tuning)
  }
  
  # eta variance -> factor
  if (!is.null(dat$eta_variance)){
    dat$eta_variance <- factor(dat$eta_variance)
  }
  
  # report invalid rows (weren't selected as part of a cohesive algorithm)
  #valid_rows = which(dat$algorithm!='invalid')
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat)
}

plotAlgorithms <- function(dat, yvarname, title, xvarname="num_documents_with_annotations", ymin=min(dat[[yvarname]]), ymax=max(dat[[yvarname]]), ylabel="Accuracy", xlabel="Number of annotated instances x %s",
                           shapesize=1, xlim=NULL, divisor=1000, hide_legend=FALSE, algorithm_colors=NULL, algorithm_shapes=NULL, facets="~corpus~num_annotators~annotator_accuracy",
                           other_ggplot_elements=NULL, xbreaks=NULL){
  # a modified colorblind-friendly pallette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  
  # x tick label formatter
  xformatter <- function(x){
#     sci <- format(x,scientific=TRUE)
#     sci <- gsub('^[0\\.]+e.*','0',sci)
#     sci <- gsub('\\.\\de\\+0','e',sci)
#     gsub('e\\+0','e',sci)
    gsub('\\.0','',format(x))
  }

  if (is.null(dat$num_documents_with_annotations)){
    dat$num_documents_with_annotations <- round(dat$num_annotations / d$k)
  }

  # what are the variables we should group by when calculating std dev?
  groupvars <- strsplit(facets,'~')[[1]][] # use those we are using for for facets
  groupvars <- c(groupvars, xvarname, "algorithm") # include the x axis and line identities (algorithm)
  groupvars <- groupvars[lapply(groupvars,nchar)>0] # remove empty entries
  dfc <- summarySE(dat, measurevar=yvarname, groupvars=groupvars)
  if (!is.null(divisor)){
    dfc[[xvarname]] <- dfc[[xvarname]]/divisor
    if (!is.null(xbreaks)){
      xbreaks <- xbreaks/divisor
    }
  }

  # base plot
  plt <- ggplot(dat=dfc, aes_string(x=xvarname, y=yvarname, color="algorithm", group="algorithm")) +
    ggtitle(title) +
    geom_errorbar(aes_string(ymin=sprintf("%s-sd",yvarname), ymax=sprintf("%s+sd",yvarname))) +
    geom_line(size=0.8) +
    geom_point(aes(shape=algorithm),size=shapesize,color='black') +
    ylim(ymin,ymax) +
    ylab(ylabel) +
    xlab(sprintf(xlabel,format(divisor,big.mark=',',big.interval=3))) +
    scale_x_continuous(labels=xformatter) +
    theme(plot.title = element_text(lineheight=1.8,face='bold')) +
    theme_bw()
  # line shapes
  if (!is.null(algorithm_colors)){
    plt <- plt + scale_colour_manual(values=algorithm_colors)
  }
  # x breaks
  if (!is.null(xbreaks)){
    plt <- plt + scale_x_continuous(labels=xformatter, breaks = xbreaks) 
  }
  # line colors
  if (!is.null(algorithm_shapes)){
    plt <- plt + scale_shape_manual(values=algorithm_shapes)
  }
  # facets
  if (nchar(facets)>0){
    plt <- plt + facet_grid(facets)
  }
  # hide legend
  if (hide_legend){
      plt <- plt + theme(legend.position='none') 
  }
  # xlim
  if (!is.null(xlim)){
    plt <- plt + scale_x_continuous(limits=xlim,labels=xformatter) 
  }
  # other
  if (!is.null(other_ggplot_elements)){
    for (el in other_ggplot_elements){
      plt <- plt + el
    }
  }
  return(plt)
}

# setup paths and packages
#install.packages("ggplot2")
require(ggplot2)
setwd('/aml/home/plf1/git/Experiments/plf1/cslda-paper/csv')


# stop execution --- proceed manually
stop()

# hopefully final gamut of algorithms
data = read.csv("2015-02-25-acl.csv")
# test showing (for paper) that hyper optimization reduces algorithm effect for itemresp
data = read.csv("2015-02-25-acl-hypertuning.csv")
# uses grr to test up to 20x labeled. This is so we can 
# say something like "how much earlier did cslda achieve 95% 
# accuracy before the nearest competitor (showcasing the 
# high annotation information scenario).
data = read.csv("2015-02-26-grr-extended.csv")
# newsgroups experiments with empirical annotator contribution rates
data = read.csv("2015-03-04.csv")
# reran previous and added experiments with two simplified cfgroups
# cfsimplegroups1000b randomly chooses a single label per annotator/instance
# cfsimplegroups1000b chooses a single label per annotator/instance; the correct when available
# also added newsgroups with annotate-top-k-choices=3 set to see the effect of this 
# weird annotation scheme in simulation
data = read.csv("2015-03-05.csv")
# ran with some new crowdflower data (weather)
data = read.csv("2015-03-24-tweets.csv")
# removed "validation set" annotations (which were always being thrown in before, 
# causing learning curves to start with a bunch of deep annotations then expand shallowly
# resulting in a big dip before climbing again)
data = read.csv("2015-03-25-tweets.csv")
# added extra unannotated/unlabeled tweets re weather
data = read.csv("2015-03-25-extratweets.csv")
# added big unlabeled tweets
data = read.csv("2015-03-27-extratweets.csv")
# added companies and changed simulation order 
# to be random rather than in order of arrival
# also changed cslda to be initialized by baseline
# rather than momresp
data = read.csv("2015-04-03-extra.csv")
# changed cslda to be initialized by momresp again
data = read.csv("2015-04-03-initmomresp.csv")
# added logresp_lda (pipelined lda + logresp)
# changed initialization back to majority vote
# changed labeling-strategy to be all upper case (and 
# changed 'raykar' references to 'logresp'). BEFORE this 
# point you must use massageDataLegacy() to import data.
data = read.csv("2015-04-09-logresplda.csv")
# code refactoring (to allow intermediate confusion matrices 
# to be printed during training)
# also added 2 cfgroups datasets. One with consensus
# (majority vote) labels. One with no test questions.
data = read.csv("2015-04-13.csv")
# removed derived cfgroups from above (not interesting)
# rerunning cfgroups and newsgroups to attempt to reproduce 
# results from 04-09
data = read.csv("2015-04-15.csv")
# added word2vec feature generation for momresp 
# and logresp. trying it out on newsgroups
data = read.csv("2015-04-22.csv")
# rerunning ACL experiments for a potential 
# ICML crowdsourcing workshop submission
data = read.csv("2015-04-29.csv")
# added vector newsgroups docs pre-processed with 
# lda, word2vec, and doc2vec
# also added csldalex (with lexical features)
data = read.csv("2015-05-18.csv")
# added fully discriminative model
data = read.csv("2015-05-20.csv")
# added cfgroups + embedded words and annotations
data = read.csv("2015-05-28.csv")
# changed word2vec embeddings to use pre-calculated google 
# vectors, and to aggregate docs via averaging rather than summing.
data = read.csv("2015-06-06.csv")
# did 5-repeats
data = read.csv("2015-06-10.csv")
# added 2 new datasets: twitterparaphrase and twittersentiment
# and also vectorized them, weather, and cfgroups using both 
# word2vec and doc2vec
data = read.csv("2015-06-17.csv")
# added another dataset: compatibility
data = read.csv("2015-06-25.csv")


#########################################################
#             Prototyping
#########################################################
mdata <- massageData(data); d <- mdata
# choose a dataset
d <- mdata; d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d <- mdata; d = mdata[which(grepl("cfgroups",mdata$dataset)),]
d <- mdata; d = mdata[which(mdata$corpus=="NG"),]
d <- mdata; d = mdata[which(mdata$corpus=="DREDZE"),]
d <- mdata; d = mdata[which(mdata$corpus=="R8"),]
d <- mdata; d = mdata[which(mdata$corpus=="R52"),]

d <- mdata; d = d[which(d$algorithm=="baseline" | d$algorithm=="varmomresp" | d$algorithm=="varlogresp" | d$algorithm=="cslda"),]
d <- mdata; d = d[which(grepl("w2v",d$algorithm) | d$algorithm=="baseline" | d$algorithm=="varmomresp" | d$algorithm=="varlogresp" | d$algorithm=="cslda"),]
d <- mdata; d = d[which(grepl("cslda",d$algorithm)),]
d <- mdata; d = d[which(grepl("d2v",d$algorithm) | d$algorithm=="baseline" | d$algorithm=="varmomresp" | d$algorithm=="varlogresp" | d$algorithm=="cslda"),]
d <- mdata; d = d[which(grepl("lda",d$algorithm) | d$algorithm=="baseline" | d$algorithm=="varmomresp" | d$algorithm=="varlogresp" | d$algorithm=="cslda"),]
d <- mdata; d = d[which(grepl("discrim",d$algorithm)),]
d <- mdata; d = d[which(d$algorithm=="varlogresp" | d$algorithm=="logresp_m" | d$algorithm=="baseline"),]
d <- mdata; d = d[which(grepl("logresp",d$algorithm)),]
d <- mdata; d = d[which(grepl("discrim",d$algorithm)),]
d <- mdata; d = d[which(d$algorithm=="logresp"),]
d <- mdata; d = d[which(d$algorithm=="cslda_s" | grepl("w2v",d$algorithm)),]
d <- mdata; d = d[which(d$algorithm=="varlogresp_w2v" | d$algorithm=="varlogresp_d2v" | d$algorithm=="cslda_s" | d$algorithm=="baseline" | d$algorithm=="varlogresp"),]

facets <- "~annotator_accuracy~corpus~vary_annotator_rates"
xvarname <- "num_annotations"
plotAlgorithms(d,"labeled_acc","Inferred Label Accuracy",ymin=0.,ymax=1,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"unlabeled_acc","Unlabeled Label Accuracy",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"heldout_acc","Test Label Accuracy",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"log_joint","Inferred Label Accuracy",ymin=min(d$log_joint),ymax=max(d$log_joint),facets=facets,xvarname=xvarname)
plotAlgorithms(d,"overall_acc","Overall Accuracy",facets=facets,xvarname=xvarname)
plotAlgorithms(d,"btheta","BTheta",facets=facets,xvarname=xvarname)
plotAlgorithms(d,"bgamma","BGamma",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"cgamma","CGamma",ymin=0,ymax=50,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"bphi","BPhi",ymin=0,ymax=2,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"top3_labeled_acc","Top 3 Labeled Accuracy",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"annacc_rmse","Annotator RMSE",ymin=0,ylabel="Annotator RMSE",facets=facets,xvarname=xvarname)
plotAlgorithms(d,"annacc_mat_rmse","Annotator Matrix RMSE",ymin=0,ymax=.2,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"num_annotations","Inferred Label Accuracy",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"num_documents_with_annotations","Docs with Annotations",ymin=0,facets=facets,xvarname=xvarname)

plot(d$log_joint, d$labeled_acc)
j = d[which(d$algorithm!='itemresp' & d$algorithm!='momresp'),]
plotAlgorithms(j,"machacc_rmse","Machine RMSE",ymin=0)
plotAlgorithms(j,"machacc_mat_rmse","Machine MAT RMSE")






#############################################################################
#         Enabling Crowdsourcing with document representations 2015
#############################################################################


######################### newsgroups ###############################
data = read.csv("2015-06-25.csv")

# levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
# alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
# alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data);
# mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
# mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, # algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),])
ggsave("../images/newsgroups.eps",width=width,height=height,units='cm')


######################### cfgroups1000 ###############################
data = read.csv("2015-06-25.csv")

# levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
# alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
# alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.0
ymax = 0.75
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data);
mdata <- mdata[which(mdata$algorithm=="varlogresp" | mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varmomresp" | mdata$algorithm=="cslda_s" | mdata$algorithm=="baseline"),]
# mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
# mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus~dataset", shapesize=shapesize, # algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="CFGROUPS1000"),])
ggsave("../images/cfgroups1000.eps",width=width,height=height,units='cm')


##################################### general tweet sentiment ###################################
# levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
# alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
# alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.55
ymax = 0.8
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data); mdata <- mdata[which(!grepl("itemresp_",mdata$algorithm)),] # strip useless itemresp_w2v
# mdata <- mdata[which(grepl("discrim",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("itemresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("momresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("logresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("cslda",mdata$algorithm) | mdata$algorithm=="baseline"),]
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="cslda_s" | mdata$algorithm=="varmomresp" | mdata$algorithm=="varitemresp" |  mdata$algorithm=="baseline"),]
# mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
# mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, # algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="TWITTER_SENT"),])
ggsave("../images/twittersent.eps",width=width,height=height,units='cm')


######################################## weather tweet sentiment #######################################
# levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
# alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
# alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data); mdata <- mdata[which(!grepl("itemresp_",mdata$algorithm)),] # strip useless itemresp_w2v
# mdata <- mdata[which(grepl("discrim",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("itemresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("momresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("logresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("cslda",mdata$algorithm) | mdata$algorithm=="baseline"),]
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="cslda_s" | mdata$algorithm=="varmomresp" | mdata$algorithm=="varitemresp" |  mdata$algorithm=="baseline"),]
# mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
# mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, # algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="WEATHER"),])
ggsave("../images/weather.eps",width=width,height=height,units='cm')


######################################## compatibility #######################################
# levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
# alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
# alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data); mdata$algorithm[which(mdata$algorithm=="varitemresp_w2v")] <- "varitemresp" # strip useless itemresp_w2v
# mdata <- mdata[which(grepl("discrim",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("itemresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("momresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("logresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("cslda",mdata$algorithm) | mdata$algorithm=="baseline"),]
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varitemresp" | mdata$algorithm=="baseline"),]
# mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
# mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, # algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="COMPATIBILITY"),])
ggsave("../images/compatibility.eps",width=width,height=height,units='cm')


############################################# tweet paraphrase dataset ##############################################################
# levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
# alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
# alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data); mdata <- mdata[which(!grepl("itemresp_",mdata$algorithm)),] # strip useless itemresp_w2v
# mdata <- mdata[which(grepl("discrim",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("itemresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("momresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("logresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("cslda",mdata$algorithm) | mdata$algorithm=="baseline"),]
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varitemresp" | mdata$algorithm=="baseline"),]
# mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
# mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, # algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="TWITTER_PARA"),])
ggsave("../images/twitterparaphrase.eps",width=width,height=height,units='cm')


















#########################################################
#             CONNL 2015 Submission
#########################################################

#scale_colour_manual(values=c('Majority'="#619Cff", 'MomResp'="#F8766D",'MomResp+MF'="#F8766D", 'LogResp'="#00BA38",'LogResp+MF'="#00BA38", 'MomResp+Gibbs'="#777777",'LogResp+EM'="#777777")) +
#scale_shape_manual(values=c('Majority'=1, 'MomResp'=17,'MomResp+MF'=17, 'LogResp'=18,'LogResp+MF'=18,'LogResp+EM'=3, 'MomResp+Gibbs'=3)) 
#scale_colour_manual(values=c('baseline'="#555555", 'itemresp'="#009E73", 'momresp'="#56B4E9", 'multiresp'="#D5005E",'multiresp_m'="#5ED500", 'itemresp_s'="#0072B2", 'momresp_s'="#E69F00", 'multiresp_s'="#CC79A7", 'multiresp_sm'="#79CCA7")) +
#scale_shape_manual(values=c('baseline'=1, 'itemresp'=17, 'momresp'=18, 'multiresp'=3, 'multiresp_m'=4, 'itemresp_s'=5, 'momresp_s'=6, 'multiresp_s'=0, 'multiresp_sm'=0)) 

# shared plotting params
alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 )
width = 15
height = 9
ymin = 0.57
ymax = 1
shapesize = 2
xvarname = "num_annotations"


######################## Sentiment-annotated Weather Tweets #######################
data = read.csv("2015-04-29-connl.csv")

# custom plotting params
levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data);
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", algorithm_colors=alg_colors, algorithm_shapes=alg_shapes, shapesize=shapesize,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
# Weather
plotty(mdata[which(mdata$corpus=="WEATHER"),])
ggsave("../images/weather.eps",width=width,height=height,units='cm')

######################## Crowdflower-annotated Newsgroups Data #######################
data = read.csv("2015-04-29-connl.csv")
data = data[which(data$num_annotations>500),] # first data point is really noisy and distracting

# custom plotting params
levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 7.5
shapesize = 3
# data
mdata <- massageData(data);
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,ymin,ymax,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", algorithm_colors=alg_colors, algorithm_shapes=alg_shapes, shapesize=shapesize,
                 hide_legend=hide_legend)
}
# Newsgroups RAW
plotty(mdata[which(mdata$dataset=="cfgroups1000.json"),],ymin=0.4,ymax=0.7)
ggsave("../images/cfgroups.eps",width=width,height=height,units='cm')
# Newsgroups RAW
plotty(mdata[which(mdata$dataset=="cfsimplegroups1000a.json"),],ymin=0.75,ymax=0.95)
ggsave("../images/cfsimplegroupsa.eps",width=width,height=height,units='cm')
# Newsgroups RAW
plotty(mdata[which(mdata$dataset=="cfsimplegroups1000d.json"),],ymin=0.9,ymax=0.96)
ggsave("../images/cfsimplegroupsd.eps",width=width,height=height,units='cm')



######################## 7-deep algorithm comparison #######################
data = read.csv("2015-04-29-connl.csv")
data <- data[which(data$num_annotations>200 & data$annotator_accuracy!="FILE"),] # ignore FILE newsgroups results

# shared plotting params
levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 13
height = 8
shapesize = 3
# data
mdata <- massageData(data);
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,ymin,ymax,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", algorithm_colors=alg_colors, algorithm_shapes=alg_shapes, shapesize=shapesize,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
# Newsgroups
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),],ymin=0.78,ymax=1)
ggsave("../images/newsgroups.eps",width=width,height=height,units='cm')
# Cade12
plotty(mdata[which(mdata$corpus=="CADE12"),],ymin=0.55,ymax=0.95, hide_legend=TRUE)
ggsave("../images/cade12.eps",width=width,height=height,units='cm')
# Enron
plotty(mdata[which(mdata$corpus=="ENRON"),],ymin=0.78,ymax=0.96, hide_legend=TRUE)
ggsave("../images/enron.eps",width=width,height=height,units='cm')
# R8
plotty(mdata[which(mdata$corpus=="R8"),],ymin=0.67,ymax=1, hide_legend=TRUE)
ggsave("../images/r8.eps",width=width,height=height,units='cm')
# R52
plotty(mdata[which(mdata$corpus=="R52"),],ymin=0.8,ymax=1, hide_legend=TRUE)
ggsave("../images/r52.eps",width=width,height=height,units='cm')
# Webkb
plotty(mdata[which(mdata$corpus=="WEBKB"),],ymin=0.55,ymax=0.95, hide_legend=TRUE)
ggsave("../images/webkb.eps",width=width,height=height,units='cm')





######################## testing the hyperparam optimization hypothesis #######################
data = read.csv("2015-02-26-hypers.csv")
# shared plotting params
alg_colors=c('ICM'='#777777', 'Gibbs'='#555555', 'Var'="#619Cff")
alg_shapes=c('ICM'=5,         'Gibbs-M'=3,       'Var'=1) 
width = 13
height = 6
ymin = 0.2
ymax = 0.8
shapesize = 3
#### without hyper optimization
mdata <- massageDataLegacy(data); d <- mdata
# data
d <- d[which(d$inline_hyperparam_tuning=="false"),]
d <- d[which(d$algorithm=="itemresp" | d$algorithm=="varitemresp" | d$algorithm=="itemresp_s"),]
d$algorithm <- mapvalues(d$algorithm, from=c('itemresp','itemresp_s','varitemresp','itemresp_grid','itemresp_s_grid','varitemresp_grid'), to=c('ICM','Gibbs','Var','ICM','Gibbs','Var')) # rename
d$algorithm <- factor(d$algorithm, levels=c('Gibbs','Var','ICM')) # reorder
# plot
plotAlgorithms(d,"labeled_acc","",ymin=ymin,ymax=ymax,facets="~corpus",shapesize=shapesize)
ggsave("../images/without-hypers.eps",width=width,height=height,units='cm')

#### with hyper optimization
mdata <- massageDataLegacy(data); d <- mdata
# data
d <- d[which(d$inline_hyperparam_tuning=="false"),]
d <- d[which(d$algorithm=="itemresp_grid" | d$algorithm=="varitemresp_grid" | d$algorithm=="itemresp_s_grid"),]
d$algorithm <- mapvalues(d$algorithm, from=c('itemresp','itemresp_s','varitemresp','itemresp_grid','itemresp_s_grid','varitemresp_grid'), to=c('ICM','Gibbs','Var','ICM','Gibbs','Var')) # rename
d$algorithm <- factor(d$algorithm, levels=c('Gibbs','Var','ICM')) # reorder
# plot
plotAlgorithms(d,"labeled_acc","",ymin=ymin,ymax=ymax,facets="~corpus",shapesize=shapesize)
ggsave("../images/with-hypers.eps",width=width,height=height,units='cm')

# # with hyper tuning (NOT VALID!!! Minka updates NOT IMPLEMENTED for Itemresp (only varitemresp))
# mdata <- massageData(data); d <- mdata
# d <- d[which(d$inline_hyperparam_tuning=="true"),]
# d <- d[which(d$algorithm=="itemresp" | d$algorithm=="varitemresp" | d$algorithm=="itemresp_s"),]
# plotAlgorithms(d,"labeled_acc","With Fixed-Point Hyperparameter Tuning",ymin=0,facets="~corpus")

############################ charting extended annotation ranges with grr #############################
data = read.csv("2015-04-30-connl-table.csv")
# shared plotting params
levels=c('csLDA','MomResp','LogResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
ymin = 0.4
ymax = 1.0
shapesize = 2
# data
mdata <- massageData(data);
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,xticksize){
  plotAlgorithms(d,"labeled_acc","",ymin=ymin,ymax=ymax,facets="~corpus", algorithm_colors=alg_colors, algorithm_shapes=alg_shapes, shapesize=shapesize, 
                 xvarname="num_annotations", xbreaks = round(seq(min(d$num_annotations), max(d$num_annotations), by = xticksize),1),
                 other_ggplot_elements=c(
                   geom_hline(aes(yintercept=0.95))
                   #         scale_x_continuous(breaks = )
                 )
  )
}
# Cade12
plotty(mdata[which(mdata$corpus=="CADE12"),], 20000)
plotty(mdata[which(mdata$corpus=="CADE12" & mdata$algorithm=="csLDA"),], 20000) # csLDA gets buried in the previous graph
# Newsgroups
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),], 10000)
# Enron
plotty(mdata[which(mdata$corpus=="ENRON"),], 2000)
# R52
plotty(mdata[which(mdata$corpus=="R52"),], 3000)
# R8
plotty(mdata[which(mdata$corpus=="R8"),], 3000)
# Webkb
plotty(mdata[which(mdata$corpus=="WEBKB"),], 2000)



######################## Joint vs Pipeline inference for cslda #######################
data = read.csv("2015-04-30-connl-table.csv")
data <- data[which(data$num_annotations>15000 & data$num_annotation<60000 & data$annotator_accuracy!="FILE"),] # ignore FILE newsgroups results
alg_colors=c('csLDA'='#F563E3', 'csLDA-P'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-P'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
levels=c('csLDA','csLDA-P') # determined line order in legend

# shared plotting params
width = 13
height = 6
shapesize = 3
# data
mdata <- massageData(data);
xvarname = "num_annotations"
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','logresp_lda_s','cslda_s','varitemresp','varmomresp','varlogresp'), to=c('Majority','csLDA-P','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,ymin,ymax,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", algorithm_colors=alg_colors, algorithm_shapes=alg_shapes, shapesize=shapesize,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
# Newsgroups
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),],ymin=0.68,ymax=.9)
ggsave("../images/joint-vs-pipeline.eps",width=width,height=height,units='cm')