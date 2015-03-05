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

nameRows <- function(dat,name,...){
  criteria <- list(...)
  # start with everything matched
  matchedRows <- rep(TRUE,dim(dat)[1])
  # intersection of rows that match each criterion
  for (criterion in criteria){
    matchedRows <- matchedRows & criterion(dat)
  }
  # add algorithm name to matched rows
  dat$algorithm[which(matchedRows)] <- name
  return(dat)
}


massageData <- function(dat){
  dat$labeled_acc = as.numeric(as.character(dat$labeled_acc))
  dat$heldout_acc = as.numeric(as.character(dat$heldout_acc))
  dat$top3_labeled_acc = as.numeric(as.character(dat$top3_labeled_acc))
  dat$top3_heldout_acc = as.numeric(as.character(dat$top3_heldout_acc))
  
  num_rows = dim(dat)[1]
  dat$algorithm <- rep("invalid",num_rows)

  # add truncate_unannotated_data=false if it doesn't exist
  dat$truncate_unannotated_data <- if (is.null(dat$truncate_unannotated_data)) rep('false',num_rows) else dat$truncate_unannotated_data
  
  # baselines
  dat <- nameRows(dat, 'baseline', isLabelingStrategy('ubaseline'))
  
  # itemresp variants
  dat <- nameRows(dat, 'itemresp_s', isLabelingStrategy('itemresp'), not(isOptimization), hasHyperTuning("none"))
  dat <- nameRows(dat, 'itemresp_s_grid', isLabelingStrategy('itemresp'), not(isOptimization), hasHyperTuning("GRID"))
  dat <- nameRows(dat, 'itemresp_s_bob', isLabelingStrategy('itemresp'), not(isOptimization), hasHyperTuning("BOBYQA"))
  dat <- nameRows(dat, 'itemresp', isLabelingStrategy('itemresp'), isOptimization, hasHyperTuning("none"))
  dat <- nameRows(dat, 'itemresp_grid', isLabelingStrategy('itemresp'), isOptimization, hasHyperTuning("GRID"))
  dat <- nameRows(dat, 'itemresp_bob', isLabelingStrategy('itemresp'), isOptimization, hasHyperTuning("BOBYQA"))
  dat <- nameRows(dat, 'varitemresp', isLabelingStrategy('varitemresp'), isOptimization, hasHyperTuning("none"))  
  dat <- nameRows(dat, 'varitemresp_grid', isLabelingStrategy('varitemresp'), isOptimization, hasHyperTuning("GRID"))  
  dat <- nameRows(dat, 'varitemresp_bob', isLabelingStrategy('varitemresp'), isOptimization, hasHyperTuning("BOBYQA"))  
  
  # neutered variants
  dat <- nameRows(dat, 'momresp_s', isLabelingStrategy('momresp'), not(isOptimization))
  dat <- nameRows(dat, 'momresp', isLabelingStrategy('momresp'), isOptimization)
  dat <- nameRows(dat, 'varmomresp', isLabelingStrategy('varmomresp'), isOptimization)
  
  # multiresp variants
  dat <- nameRows(dat, 'multiresp_s', isLabelingStrategy('multiresp'), not(isOptimization))
  dat <- nameRows(dat, 'multiresp', isLabelingStrategy('multiresp'), isOptimization)
  dat <- nameRows(dat, 'varmultiresp', isLabelingStrategy('varmultiresp'), isOptimization)
  
  # raykar varians
  dat <- nameRows(dat, 'raykar_st', isLabelingStrategy('raykar'), isOptimization)
  dat <- nameRows(dat, 'raykar', isLabelingStrategy('rayktrunc'), isOptimization)
  dat <- nameRows(dat, 'varraykar', isLabelingStrategy('varrayk'), isOptimization)
  
  # cslda
  dat <- nameRows(dat, 'cslda_s', isLabelingStrategy('cslda'), not(isOptimization), hasHyperTuning("none"))
  dat <- nameRows(dat, 'cslda_s_grid', isLabelingStrategy('cslda'), not(isOptimization), hasHyperTuning("GRID"))
  dat <- nameRows(dat, 'cslda', isLabelingStrategy('cslda'), isOptimization, hasHyperTuning("none"))
  dat <- nameRows(dat, 'cslda_grid', isLabelingStrategy('cslda'), isOptimization, hasHyperTuning("GRID"))
  
  #dat <- nameRows(dat,'multiresp_du0_s',    'multiresp',0,      FALSE, TRUE)  
  #dat <- nameRows(dat,'multiresp_du0','multiresp',0,       TRUE, TRUE)   
  #   dat <- nameRows(dat,'multiresp_bin_s',    'multiresp','binary_classifier',FALSE,TRUE)
  #   dat <- nameRows(dat,'multiresp_bin','multiresp','binary_classifier',TRUE,TRUE)

  # make 'algorithm' into factor 
  dat$algorithm <- factor(dat$algorithm)

  # name num_annotators into a factor (so it can be used as a plotting facet)
  dat$num_annotators <- factor(dat$num_annotators)

  # rename k to 'd' and re-order
  require(plyr)
  dat$d <- sprintf("d = %g",dat$k)
  dat$d <- factor(dat$d, levels = c('d = 1','d = 2','d = 3','d = 5','d = 10'))
  
  # prettify factor names
  dat$tuning <- sprintf("Hyperparam Tuning = %s",dat$inline_hyperparam_tuning)

  # eta variance -> factor
  dat$eta_variance <- factor(dat$eta_variance)

  # treat simplified cfgroups as its own corpus
  dat$corpus <- as.character(dat$corpus)
  dat$corpus[which(dat$dataset=="cfsimplegroups1000a.json")] <- "CFSIMPLEGROUPS"
  dat$corpus <- factor(dat$corpus)

  # report invalid rows (weren't selected as part of a cohesive algorithm)
  #valid_rows = which(dat$algorithm!='invalid')
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat[valid_rows,])
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
    theme(plot.title = element_text(lineheight=1.8,face='bold')) 
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

#########################################################
#             Prototyping
#########################################################
mdata <- massageData(data); d <- mdata
# choose a dataset
# d = mdata[which(mdata$corpus=="REUTERS"),]
d = mdata[which(mdata$corpus=="ENRON"),]
d = mdata[which(mdata$corpus=="NB20"),]
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = mdata[which(mdata$corpus=="NG"),]
d = mdata[which(mdata$corpus=="DREDZE"),]
d = mdata[which(mdata$corpus=="R8"),]
d = mdata[which(mdata$corpus=="R52"),]
d = mdata[which(mdata$corpus=="CADE12"),]
d = mdata[which(mdata$corpus=="WEBKB"),]
d = mdata[which(mdata$corpus=="CFGROUPS1000"),]
d = mdata[which(mdata$corpus=="CFSIMPLEGROUPS"),]
d = mdata[which(mdata$corpus=="CFSIMPLEGROUPS" | mdata$corpus=="CFGROUPS1000"),]

d = mdata[which(mdata$annotator_accuracy=="CFBETA"),]

facets <- "~annotator_accuracy~corpus~num_topics~vary_annotator_rates"
plotAlgorithms(d,"labeled_acc","Inferred Label Accuracy",ymin=0,facets=facets)
plotAlgorithms(d,"unlabeled_acc","Unlabeled Label Accuracy",ymin=0,facets=facets)
plotAlgorithms(d,"heldout_acc","Test Label Accuracy",ymin=0,facets=facets)
plotAlgorithms(d,"log_joint","Inferred Label Accuracy",ymin=min(d$log_joint),ymax=max(d$log_joint),facets=facets)
plotAlgorithms(d,"overall_acc","Overall Accuracy",facets=facets)
plotAlgorithms(d,"btheta","BTheta",facets=facets)
plotAlgorithms(d,"bgamma","BGamma",ymin=0,facets=facets)
plotAlgorithms(d,"cgamma","CGamma",ymin=0,ymax=50,facets=facets)
plotAlgorithms(d,"bphi","BPhi",ymin=0,ymax=2,facets=facets)
plotAlgorithms(d,"top3_labeled_acc","Top 3 Labeled Accuracy",ymin=0,facets=facets)
plotAlgorithms(d,"annacc_rmse","Annotator RMSE",ymin=0,ylabel="Annotator RMSE",facets=facets)
plotAlgorithms(d,"annacc_mat_rmse","Annotator Matrix RMSE",ymin=0,ymax=.2,facets=facets)

plot(d$log_joint, d$labeled_acc)
j = d[which(d$algorithm!='itemresp' & d$algorithm!='momresp'),]
plotAlgorithms(j,"machacc_rmse","Machine RMSE",ymin=0)
plotAlgorithms(j,"machacc_mat_rmse","Machine MAT RMSE")

#########################################################
#             ACL 2015 Submission
#########################################################

#scale_colour_manual(values=c('Majority'="#619Cff", 'MomResp'="#F8766D",'MomResp+MF'="#F8766D", 'LogResp'="#00BA38",'LogResp+MF'="#00BA38", 'MomResp+Gibbs'="#777777",'LogResp+EM'="#777777")) +
#scale_shape_manual(values=c('Majority'=1, 'MomResp'=17,'MomResp+MF'=17, 'LogResp'=18,'LogResp+MF'=18,'LogResp+EM'=3, 'MomResp+Gibbs'=3)) 
#scale_colour_manual(values=c('baseline'="#555555", 'itemresp'="#009E73", 'momresp'="#56B4E9", 'multiresp'="#D5005E",'multiresp_m'="#5ED500", 'itemresp_s'="#0072B2", 'momresp_s'="#E69F00", 'multiresp_s'="#CC79A7", 'multiresp_sm'="#79CCA7")) +
#scale_shape_manual(values=c('baseline'=1, 'itemresp'=17, 'momresp'=18, 'multiresp'=3, 'multiresp_m'=4, 'itemresp_s'=5, 'momresp_s'=6, 'multiresp_s'=0, 'multiresp_sm'=0)) 


######################## 7-deep algorithm comparison #######################
data = read.csv("2015-02-26-acl.csv")
data <- data[which(data$annotator_accuracy=="CFBETA"),] # ignore FILE newsgroups results

# shared plotting params
alg_colors=c('csLDA'='#00BEC4', 'csLDA-M'='#F563E3', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-M'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 15
height = 9
ymin = 0.4
ymax = 1.0
shapesize = 2
# data
mdata <- massageData(data);
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','cslda','cslda_s','varitemresp','varmomresp','varraykar'), to=c('Majority','csLDA-M','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=c('csLDA-M','csLDA','MomResp','LogResp','ItemResp','Majority')) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",ymin=ymin,ymax=ymax,facets="~corpus", algorithm_colors=alg_colors, algorithm_shapes=alg_shapes, shapesize=shapesize,
                 hide_legend=hide_legend)
}
# Newsgroups
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),])
ggsave("../images/newsgroups.eps",width=width,height=height,units='cm')
# Cade12
plotty(mdata[which(mdata$corpus=="CADE12"),], hide_legend=TRUE)
ggsave("../images/cade12.eps",width=width,height=height,units='cm')
# Enron
plotty(mdata[which(mdata$corpus=="ENRON"),], hide_legend=TRUE)
ggsave("../images/enron.eps",width=width,height=height,units='cm')
# R8
plotty(mdata[which(mdata$corpus=="R8"),], hide_legend=TRUE)
ggsave("../images/r8.eps",width=width,height=height,units='cm')
# R52
plotty(mdata[which(mdata$corpus=="R52"),], hide_legend=TRUE)
ggsave("../images/r52.eps",width=width,height=height,units='cm')
# Webkb
plotty(mdata[which(mdata$corpus=="WEBKB"),], hide_legend=TRUE)
ggsave("../images/webkb.eps",width=width,height=height,units='cm')


######################## testing the hyperparam optimization hypothesis #######################
data = read.csv("2015-02-26-hypers.csv")
# shared plotting params
alg_colors=c('ICM'='#777777', 'Gibbs'='#555555', 'Var'="#619Cff")
alg_shapes=c('ICM'=5,         'Gibbs-M'=3,       'Var'=1) 
width = 10
height = 6
ymin = 0.2
ymax = 0.8
shapesize = 2
#### without hyper optimization
mdata <- massageData(data); d <- mdata
# data
d <- d[which(d$inline_hyperparam_tuning=="false"),]
d <- d[which(d$algorithm=="itemresp" | d$algorithm=="varitemresp" | d$algorithm=="itemresp_s"),]
d$algorithm <- mapvalues(d$algorithm, from=c('itemresp','itemresp_s','varitemresp','itemresp_grid','itemresp_s_grid','varitemresp_grid'), to=c('ICM','Gibbs','Var','ICM','Gibbs','Var')) # rename
d$algorithm <- factor(d$algorithm, levels=c('Gibbs','Var','ICM')) # reorder
# plot
plotAlgorithms(d,"labeled_acc","",ymin=ymin,ymax=ymax,facets="~corpus",shapesize=shapesize)
ggsave("../images/without-hypers.eps",width=width,height=height,units='cm')

#### with hyper optimization
mdata <- massageData(data); d <- mdata
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
data = read.csv("2015-02-26-grr-extended.csv")
data <- data[which(data$annotator_accuracy=="CFBETA"),] # ignore FILE newsgroups results
# shared plotting params
alg_colors=c('csLDA'='#00BEC4', 'csLDA-M'='#F563E3', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=5,         'csLDA-M'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'ItemResp'=6 ) 
width = 15
height = 8
ymin = 0.4
ymax = 1.0
shapesize = 2
# data
mdata <- massageData(data);
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','cslda','cslda_s','varitemresp','varmomresp','varraykar'), to=c('Majority','csLDA-M','csLDA','ItemResp','MomResp','LogResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=c('csLDA-M','csLDA','MomResp','LogResp','ItemResp','Majority')) # reorder
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

#########################################################
#             NIPS 2014 workshop Poster
#########################################################

# momresp vs logresp on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations<=90000),]
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"labeled_acc","20 Newsgroups",xlabel="Number of Simulated Annotations x 1,000",ymin=0.25,divisor=1000,shapesize=2,facets="~d~annotator_accuracy")
ggsave("newsgroups-labeled.eps",width=20,height=6,units='cm')

# momresp vs logresp *heldout* on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations<=90000),] 
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"heldout_acc","20 Newsgroups",ylabel="Test Accuracy",ymin=0.25,divisor=1000,shapesize=2,facets="~d~annotator_accuracy")
ggsave("newsgroups-heldout.eps",width=20,height=6,units='cm')

# momresp vs logresp *annacc* on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations>200 & d$num_annotations<=90000),] 
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"annacc_rmse","20 Newsgroups",ylabel="Annotator Accuracy RMSE",ymin=0.,divisor=1000,shapesize=2,facets="~d~annotator_accuracy")
ggsave("newsgroups-annacc.eps",width=20,height=6,units='cm')


#########################################################
#             NIPS 2014 workshop
#########################################################
require(ggplot2)
setwd('/aml/home/plf1/altgit/statnlp/scripts/learningcurve/csv')
#data = read.csv("2014-10-09-nips.csv")
data = read.csv("2014-12-04-naacl-withchains.csv")
#data = read.csv("2014-12-04-naacl.csv")
mdata <- massageData(data)
# rename 'varmomresp' -> 'momresp', 'varraykar' -> 'logresp'
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('varmomresp','varraykar','baseline','raykar','momresp_s'), to=c('MomResp','LogResp','Majority','LogResp+EM',"MomResp+Gibbs"))

# momresp vs logresp on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations<=60000),] # only report up til everything has 3 anns
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 3"),]
plotAlgorithms(d,"labeled_acc","20 Newsgroups",ymin=0.25,divisor=1000,shapesize=2,facets="~d~annotator_accuracy")
ggsave("newsgroups-labeled.eps",width=20,height=6,units='cm')

# old vs new (variational vs EM for logresp) on r52 + med
d = mdata
d$algorithm <- mapvalues(d$algorithm, from=c('LogResp'), to=c('LogResp+MF'))
d = d[which(d$corpus=="R52" & d$num_annotations<=30000),]
d = d[which(d$algorithm=="LogResp+EM" | d$algorithm=="LogResp+MF"),]
d = d[which(d$d=="d = 3"),]
d = d[which(d$annotator_accuracy=="LOW"),]
plotAlgorithms(d,"labeled_acc","LogResp Inference",ymin=0.,ymax=0.6,divisor=1000,facets="~d~annotator_accuracy",shapesize=2)
ggsave("var-versus-em.eps",width=10,height=5,units='cm')


# old vs new (variational vs Gibbs for momresp) on webkb + med
# needs 2014-10-09-chains-nips.csv
d = mdata
d$algorithm <- mapvalues(d$algorithm, from=c('MomResp'), to=c('MomResp+MF'))
d = d[which(d$corpus=="WEBKB" & d$num_annotations<=13000),]
d = d[which(d$algorithm=="MomResp+Gibbs" | d$algorithm=="MomResp+MF"),]
d = d[which(d$d=="d = 1"),]
d$k <- 3
d = d[which(d$annotator_accuracy=="MED"),]
plotAlgorithms(d,"labeled_acc","MomResp Inference",ymin=0.25,divisor=1000,facets="~d~annotator_accuracy~corpus",shapesize=2)
ggsave("var-versus-gibbs.eps",width=10,height=5,units='cm')

# real (cfgroups1000)
d = mdata
d = d[which(d$corpus=="CFGROUPS1000"),]
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
#plotAlgorithms(d,"top3_labeled_acc","Crowdflower Annotations",ymin=0.5,divisor=1000,corpusFacet=TRUE,shapesize=2)
plotAlgorithms(d,"labeled_acc","Crowdflower Annotations",xlab="Number of human annotations x %s",ymin=0.35,ymax=0.7,divisor=1000,facets="~d",shapesize=2)
ggsave("cfgroups1000.eps",width=10,height=5,units='cm')

# big grid for estimating crossover points (TODO: calculate this automatically)
d = mdata
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp" | d$algorithm=="Majority"),]
#d = d[which(d$corpus=="WEBKB" & d$annotator_accuracy=="HIGH"),]
#d = d[which(d$corpus=="WEBKB"),]
#d = d[which(d$corpus=="R52"),]
#d = d[which(d$corpus=="CADE12"),]
#d = d[which(d$corpus=="ENRON"),]
#d = d[which(d$corpus=="NEWSGROUPS" & d$annotator_accuracy=="CONFLICT"),]
#d = d[which(d$d=="d = 1"),]
d = d[which(d$d=="d = 3"),]
plotAlgorithms(d,"labeled_acc","Crossover Grid",ymin=0.0,divisor=1000,shapesize=2,facets="~d~annotator_accuracy~corpus")
ggsave("crossover-grid.png",width=20,height=20,units='cm')


#########################################################
#             EMNLP Main Paper
#########################################################
mdata <- massageData(data)

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
plotAlgorithms(d,"labeled_acc","Inferred Label Accuracy",ymin=.27,shapesize=1)
ggsave("newsgroups-labeled.eps",width=20,height=12,units='cm')

# d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
# d = d[which(d$num_annotations<=60000 & d$k<=3 & (d$annotator_accuracy=='LOW' | d$annotator_accuracy=='CONFLICT')),]
# plotAlgorithms(d,"labeled_acc","",ymin=.25,shapesize=1)
# ggsave("newsgroups-labeled-zoom.eps",width=14,height=21,units='cm')

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which((d$k==10 | d$k==1) & (d$annotator_accuracy=='LOW') & (d$algorithm=='baseline' | d$algorithm=='multiresp')),]
plotAlgorithms(d,"heldout_acc","",xlim=c(0,5.2), ymin=.2,shapesize=2)
ggsave("newsgroups-heldout-1.eps",width=14,height=10,units='cm')

# d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
# d = d[which((d$k==10 | d$k==1) & (d$annotator_accuracy=='LOW') & (d$algorithm=='baseline' | d$algorithm=='multiresp')),]
# plotAlgorithms(d,"heldout_acc","", ymin=.2,shapesize=2)
# ggsave("newsgroups-heldout-2.eps",width=8,height=10,units='cm')

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
#d = d[which(d$k==1 & d$annotator_accuracy=='LOW'),]
d = d[which(d$k==1 & (d$annotator_accuracy=='LOW' | d$annotator_accuracy=='CONFLICT')),]
plotAlgorithms(d,"annacc_rmse","",ymin=0,ymax=.35,ylabel="Annotator RMSE",shapesize=2)
ggsave("newsgroups-annacc.eps",width=14,height=7,units='cm')

d = mdata[which(mdata$corpus=="ENRON"),]
plotAlgorithms(d,"labeled_acc","",ymin=.2,shapesize=1)
ggsave("enron-labeled.eps",width=20,height=12,units='cm')

# d = mdata[which(mdata$corpus=="ENRON"),]
# d = d[which(d$num_annotations<=30000 & d$k<=3 & (d$annotator_accuracy=='LOW' | d$annotator_accuracy=='CONFLICT')),]
# plotAlgorithms(d,"labeled_acc","",ymin=.20,shapesize=1)
# ggsave("enron-labeled-zoom.eps",width=14,height=12,units='cm')

# bi-corpus zoomed-in graph
d = mdata
d = d[which((d$k<=3) & (d$annotator_accuracy=='CONFLICT')),]
plotAlgorithms(d,"labeled_acc","",xlim=c(0,6.2), ymin=.2,shapesize=2,corpusFacet=TRUE)
ggsave("zoomed-labeled.eps",width=14,height=14,units='cm')

#########################################################
#             ICML Supplementary Material
#########################################################
width <- 8
height <- 9
units <- 'in'
mdata <- massageData(data) 

# -----newsgroups------
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
plotAlgorithms(d,"labeled_acc","Newsgroups Labeled",ymin=.27)
ggsave("supplement-newsgroups-labeled.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
plotAlgorithms(d,"unlabeled_acc","Newsgroups Unlabeled",ymin=.20,xlim=c(0,1.7))
ggsave("supplement-newsgroups-unlabeled.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which(!grepl('itemresp',d$algorithm)),]
plotAlgorithms(d,"overall_acc","Newsgroups Overall",ymin=.20)
ggsave("supplement-newsgroups-overall.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which(d$algorithm=='baseline' | d$algorithm=='multiresp_s' | d$algorithm=='multiresp'),]
plotAlgorithms(d,"heldout_acc","Newsgroups Heldout",ymin=.20)
ggsave("supplement-newsgroups-heldout.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
plotAlgorithms(d,"annacc_rmse","Newsgroups Annotator RMSE",ymin=0,ymax=.35)
ggsave("supplement-newsgroups-annacc.eps",width=width,height=height,units=units)

# -------enron-------
d = mdata[which(mdata$corpus=="ENRON"),]
plotAlgorithms(d,"labeled_acc","Enron Labeled",ymin=.20)
ggsave("supplement-enron-labeled.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="ENRON"),]
plotAlgorithms(d,"unlabeled_acc","Enron Unlabeled",ymin=.20,xlim=c(0,4))
ggsave("supplement-enron-unlabeled.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="ENRON"),]
d = d[which(!grepl('itemresp',d$algorithm)),]
plotAlgorithms(d,"overall_acc","Enron Overall",ymin=.20)
ggsave("supplement-enron-overall.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="ENRON"),]
d = d[which(d$algorithm=='baseline' | d$algorithm=='multiresp_s' | d$algorithm=='multiresp'),]
plotAlgorithms(d,"heldout_acc","Enron Heldout",ymin=.20)
ggsave("supplement-enron-heldout.eps",width=width,height=height,units=units)

d = mdata[which(mdata$corpus=="ENRON"),]
plotAlgorithms(d,"annacc_rmse","Enron Annotator RMSE",ymin=0,ymax=.35)
ggsave("supplement-enron-annacc.eps",width=width,height=height,units=units)

