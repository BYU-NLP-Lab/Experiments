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

truncatesData <- function(dat){
  return(dat$truncate_unannotated_data=='true')
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
  
  # neutered variants
  dat <- nameRows(dat, 'momresp_s', isLabelingStrategy('momresp'), not(isOptimization))
  dat <- nameRows(dat, 'momresp', isLabelingStrategy('momresp'), isOptimization)
  dat <- nameRows(dat, 'varmomresp', isLabelingStrategy('varmomresp'), isOptimization, not(truncatesData))
  dat <- nameRows(dat, 'varmomresp_t', isLabelingStrategy('varmomresp'), isOptimization, truncatesData)
  
  # multiresp variants
  dat <- nameRows(dat, 'multiresp_s', isLabelingStrategy('multiresp'), not(isOptimization))
  dat <- nameRows(dat, 'multiresp', isLabelingStrategy('multiresp'), isOptimization)
  dat <- nameRows(dat, 'varmultiresp', isLabelingStrategy('varmultiresp'), isOptimization)
  
  # raykar varians
  dat <- nameRows(dat, 'raykar_st', isLabelingStrategy('raykar'), isOptimization)
  dat <- nameRows(dat, 'raykar', isLabelingStrategy('rayktrunc'), isOptimization)
  dat <- nameRows(dat, 'varraykar', isLabelingStrategy('varrayk'), isOptimization)
  
  # order algorithms
  dat$algorithm <- factor(dat$algorithm, levels = c('varmomresp','varmomresp_t','momresp_s','momresp','varraykar','raykar','raykar_st','baseline'))

  # order annotator accuracy
  dat$annotator_accuracy <- factor(dat$annotator_accuracy, levels = c('CONFLICT','CONFLICT_MILD','LOW','MED','HIGH'))
  
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

plotAlgorithms <- function(dat, yvarname, title, xvarname="num_instances_annotated", ymin=min(dat[[yvarname]]), ymax=max(dat[[yvarname]]), ylabel="Accuracy", xlabel="Number of annotated instances x %s",
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
setwd('/aml/home/plf1/git/Experiments/NAACL-2015/csv/')


# stop execution --- proceed manually
stop()



#########################################################
#             NAACL 2015 
#########################################################
require(ggplot2)
setwd('/aml/home/plf1/git/Experiments/NAACL-2015/csv/')
data = read.csv("naacl-2015-1.csv")
mdata <- massageData(data); 
# rename 'varmomresp' -> 'momresp', 'varraykar' -> 'logresp'
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('varmomresp','varmomresp_t','varraykar','baseline','raykar','momresp_s'), to=c('MomResp','MomRespA','LogResp','Majority','LogResp+EM',"MomResp+Gibbs"))
algorithm_colors <- c('MomResp'='#F4766D', 'MomResp+MF'='#F4766D',    'LogResp'='#00BB34', 'LogResp+MF'='#00BB34',    'Majority'='#7B9AFF',   'LogResp+EM'='#777777', 'MomResp+Gibbs'='#777777',   'MomRespA'='#B69E00')
algorithm_shapes <- c('MomResp'=1, 'MomResp+MF'=1,    'LogResp'=2, 'LogResp+MF'=2,   'Majority'=3,   'LogResp+EM'=4,  'MomResp+Gibbs'=6,   'MomRespA'=7)

# momresp vs logresp on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS"),] 
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$annotator_accuracy!="CONFLICT_MILD"),]
#d = d[which(d$algorithm=="MomRespA"),]
plotAlgorithms(d,"labeled_acc","20 Newsgroups",ymin=0.25,ymax=1,divisor=1000,shapesize=2,facets="~annotator_accuracy", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
ggsave("../images/newsgroups-labeled.eps",width=20,height=6,units='cm')
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
plotAlgorithms(d,"heldout_acc","20 Newsgroups",ylabel="Test Accuracy",ymin=0.25,ymax=1,divisor=1000,shapesize=2,facets="~annotator_accuracy", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
ggsave("../images/newsgroups-heldout.eps",width=20,height=6,units='cm')

# mild conflict variant on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS"),] 
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp" | d$algorithm=="MomRespA"),]
d = d[which(d$annotator_accuracy=="CONFLICT_MILD"),]
#d = d[which(d$algorithm=="MomRespA"),]
plotAlgorithms(d,"labeled_acc","20 Newsgroups",ymin=0.25,ymax=1,divisor=1000,shapesize=2,facets="~annotator_accuracy", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
ggsave("../images/conflict-mild.eps",width=10,height=5,units='cm')

# # the effect of semi-supervision
# d = mdata
# d = d[which(d$corpus=="NEWSGROUPS"),] 
# d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp" | d$algorithm=="MomRespA"),]
# d = d[which(d$annotator_accuracy=="LOW"),]
# plotAlgorithms(d,"labeled_acc","20 Newsgroups",ymin=0.25, ymax=1 ,divisor=1000,shapesize=2,facets="~annotator_accuracy", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
# ggsave("../images/semi-supervision.eps",width=10,height=5,units='cm')

# old vs new (variational vs EM for logresp) on r52 + med
d = mdata
d$algorithm <- mapvalues(d$algorithm, from=c('LogResp'), to=c('LogResp+MF'))
d = d[which(d$corpus=="R52"),]
d = d[which(d$algorithm=="LogResp+EM" | d$algorithm=="LogResp+MF"),]
d = d[which(d$annotator_accuracy=="LOW"),]
d = d[which(d$num_annotations>200),]
plotAlgorithms(d,"labeled_acc","LogResp Inference",ymin=0,ymax=0.6,divisor=1000,facets="~annotator_accuracy~corpus",shapesize=2, algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
ggsave("../images/var-versus-em.eps",width=10,height=5,units='cm')


# old vs new (variational vs Gibbs for momresp) on webkb + med
# needs 2014-10-09-chains-nips.csv
d = mdata
d$algorithm <- mapvalues(d$algorithm, from=c('MomResp'), to=c('MomResp+MF'))
d = d[which(d$corpus=="WEBKB"),]
d = d[which(d$algorithm=="MomResp+Gibbs" | d$algorithm=="MomResp+MF"),]
d = d[which(d$annotator_accuracy=="MED"),]
d = d[which(d$num_annotations>100),]
plotAlgorithms(d,"labeled_acc","MomResp Inference",ymin=0.25,ymax=1,divisor=1000,facets="~annotator_accuracy~corpus",shapesize=2, algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
ggsave("../images/var-versus-gibbs.eps",width=10,height=5,units='cm')

# real (cfgroups1000)
d = mdata
d = d[which(d$corpus=="CFGROUPS1000"),]
#d = d[which(d$algorithm_seed==1),]
plotAlgorithms(d,"labeled_acc","Crowdflower Annotations",xlab="Number of human annotations x %s",ymin=0.35,ymax=0.7,divisor=1000,facets="",shapesize=2,xvarname="num_annotations", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes)
ggsave("../images/cfgroups1000.eps",width=10,height=5,units='cm')

# big grid for estimating crossover points 
data = read.csv("naacl-2015-2.csv")
mdata <- massageData(data)# rename 'varmomresp' -> 'momresp', 'varraykar' -> 'logresp'
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('varmomresp','varraykar','baseline','raykar','momresp_s'), to=c('MomResp','LogResp','Majority','LogResp+EM',"MomResp+Gibbs"))
d = mdata
#d = d[which(d$data_seed==106),]
#d = d[which(d$corpus=="WEBKB" & d$annotator_accuracy=="HIGH"),]
#d = d[which(d$corpus=="WEBKB"),]
#d = d[which(d$corpus=="R8"),]
#d = d[which(d$corpus=="CADE12"),]
#d = d[which(d$corpus=="ENRON"),]
#d = d[which(d$corpus=="NEWSGROUPS" & d$annotator_accuracy=="CONFLICT"),]
#d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"labeled_acc","Crossover Grid",ymin=0.0,divisor=1000,shapesize=2,facets="~annotator_accuracy~corpus", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes,
               xbreaks = round(seq(min(d$num_annotations), max(d$num_annotations), by = 100),1),)
#ggsave("../images/crossover-grid.png",width=75,height=50,units='cm')


#####################
# Presentation
#####################
# big grid for estimating crossover points 
data = read.csv("naacl-2015-2.csv")
mdata <- massageData(data)# rename 'varmomresp' -> 'momresp', 'varraykar' -> 'logresp'
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('varmomresp','varraykar','baseline','raykar','momresp_s'), to=c('MomResp','LogResp','Majority','LogResp+EM',"MomResp+Gibbs"))
#d = mdata; d = d[which(d$corpus=="WEBKB" & d$annotator_accuracy=="HIGH"),]
d = mdata; d = d[which(d$corpus=="NEWSGROUPS"),]
d = mdata; d = d[which(d$corpus=="WEBKB"),]
d = mdata; d = d[which(d$corpus=="R8"),]
d = mdata; d = d[which(d$corpus=="R52"),]
d = mdata; d = d[which(d$corpus=="CADE12"),]
d = mdata; d = d[which(d$corpus=="ENRON"),]
#d = mdata; d = d[which(d$corpus=="NEWSGROUPS" & d$annotator_accuracy=="CONFLICT"),]
#d = mdata; d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"labeled_acc","Crossover Grid",ymin=0.0,divisor=1000,shapesize=2,facets="~annotator_accuracy~corpus", algorithm_colors=algorithm_colors, algorithm_shapes=algorithm_shapes,
               xbreaks = round(seq(min(d$num_annotations), max(d$num_annotations), by = 100),1),)
#ggsave("../images/crossover-grid.png",width=75,height=50,units='cm')
