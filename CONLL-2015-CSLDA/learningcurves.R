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

usesWord2Vec <- function(dat){
  return(dat$doc_to_features_method=="WORD2VEC")
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


massageDataLegacy <- function(dat){
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
  dat <- nameRows(dat, 'itemresp_s', and(isLabelingStrategy('itemresp'), not(isOptimization), hasHyperTuning("none")))
  dat <- nameRows(dat, 'itemresp_s_grid', and(isLabelingStrategy('itemresp'), not(isOptimization), hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'itemresp_s_bob', and(isLabelingStrategy('itemresp'), not(isOptimization), hasHyperTuning("BOBYQA")))
  dat <- nameRows(dat, 'itemresp', and(isLabelingStrategy('itemresp'), isOptimization, hasHyperTuning("none")))
  dat <- nameRows(dat, 'itemresp_grid', and(isLabelingStrategy('itemresp'), isOptimization, hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'itemresp_bob', and(isLabelingStrategy('itemresp'), isOptimization, hasHyperTuning("BOBYQA")))
  dat <- nameRows(dat, 'varitemresp', and(isLabelingStrategy('varitemresp'), isOptimization, hasHyperTuning("none")) ) 
  dat <- nameRows(dat, 'varitemresp_grid', and(isLabelingStrategy('varitemresp'), isOptimization, hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'varitemresp_bob', and(isLabelingStrategy('varitemresp'), isOptimization, hasHyperTuning("BOBYQA")))  
  
  # neutered variants
  dat <- nameRows(dat, 'momresp_s', and(isLabelingStrategy('momresp'), not(isOptimization)))
  dat <- nameRows(dat, 'momresp', and(isLabelingStrategy('momresp'), isOptimization))
  dat <- nameRows(dat, 'varmomresp', and(isLabelingStrategy('varmomresp'), isOptimization))
  
  # multiresp variants
  dat <- nameRows(dat, 'multiresp_s', and(isLabelingStrategy('multiresp'), not(isOptimization)))
  dat <- nameRows(dat, 'multiresp', and(isLabelingStrategy('multiresp'), isOptimization))
  dat <- nameRows(dat, 'varmultiresp', and(isLabelingStrategy('varmultiresp'), isOptimization))
  
  # logresp variants
  dat <- nameRows(dat, 'logresp_st', and(isLabelingStrategy('raykar'), isOptimization))
  dat <- nameRows(dat, 'logresp', and(isLabelingStrategy('rayktrunc'), isOptimization))
  dat <- nameRows(dat, 'varlogresp', and(isLabelingStrategy('varrayk'), isOptimization))
  
  # cslda
  dat <- nameRows(dat, 'cslda_s', and(isLabelingStrategy('cslda'), not(isOptimization), hasHyperTuning("none")))
  dat <- nameRows(dat, 'cslda_s_grid', and(isLabelingStrategy('cslda'), not(isOptimization), hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'cslda', and(isLabelingStrategy('cslda'), isOptimization, hasHyperTuning("none")))
  dat <- nameRows(dat, 'cslda_grid', and(isLabelingStrategy('cslda'), isOptimization, hasHyperTuning("GRID")))
  
  # make 'algorithm' into factor 
  dat$algorithm <- factor(dat$algorithm)

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
  dat <- nameRows(dat, 'baseline', and(isLabelingStrategy('UBASELINE')))
  
  # itemresp variants
  dat <- nameRows(dat, 'itemresp_s', and(isLabelingStrategy('ITEMRESP'), not(isOptimization), hasHyperTuning("none")))
  dat <- nameRows(dat, 'itemresp_s_grid', and(isLabelingStrategy('ITEMRESP'), not(isOptimization), hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'itemresp_s_bob', and(isLabelingStrategy('ITEMRESP'), not(isOptimization), hasHyperTuning("BOBYQA")))
  dat <- nameRows(dat, 'itemresp', and(isLabelingStrategy('ITEMRESP'), isOptimization, hasHyperTuning("none")))
  dat <- nameRows(dat, 'itemresp_grid', and(isLabelingStrategy('ITEMRESP'), isOptimization, hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'itemresp_bob', and(isLabelingStrategy('ITEMRESP'), isOptimization, hasHyperTuning("BOBYQA")))
  dat <- nameRows(dat, 'varitemresp', and(isLabelingStrategy('VARITEMRESP'), isOptimization, hasHyperTuning("none"))) 
  dat <- nameRows(dat, 'varitemresp_grid', and(isLabelingStrategy('VARITEMRESP'), isOptimization, hasHyperTuning("GRID")))
  dat <- nameRows(dat, 'varitemresp_bob', and(isLabelingStrategy('VARITEMRESP'), isOptimization, hasHyperTuning("BOBYQA")))  
  
  # neutered variants
  dat <- nameRows(dat, 'momresp_s', and(isLabelingStrategy('MOMRESP'), not(isOptimization)))
  dat <- nameRows(dat, 'momresp', and(isLabelingStrategy('MOMRESP'), isOptimization))
  dat <- nameRows(dat, 'varmomresp', and(isLabelingStrategy('VARMOMRESP'), isOptimization))
  dat <- nameRows(dat, 'varmomresp_w2v', and(isLabelingStrategy('VARMOMRESP'), usesWord2Vec, isOptimization))
  
  # multiresp variants
  dat <- nameRows(dat, 'multiresp_s', and(isLabelingStrategy('MULTIRESP'), not(isOptimization)))
  dat <- nameRows(dat, 'multiresp', and(isLabelingStrategy('MULTIRESP'), isOptimization))
  dat <- nameRows(dat, 'varmultiresp', and(isLabelingStrategy('VARMULTIRESP'), isOptimization))
  
  # logresp variants
  dat <- nameRows(dat, 'logresp_st', and(isLabelingStrategy('LOGRESP_ST'), isOptimization))
  dat <- nameRows(dat, 'logresp', and(isLabelingStrategy('LOGRESP'), isOptimization))
  dat <- nameRows(dat, 'varlogresp', and(isLabelingStrategy('VARLOGRESP'), not(usesWord2Vec), isOptimization))
  dat <- nameRows(dat, 'varlogresp_w2v', and(isLabelingStrategy('VARLOGRESP'), usesWord2Vec, isOptimization))
  
  # cslda
  dat <- nameRows(dat, 'cslda_s', and(isLabelingStrategy('CSLDA'), not(isOptimization), hasHyperTuning("none")))
  #dat <- nameRows(dat, 'cslda_s_grid', and(isLabelingStrategy('CSLDA'), not(isOptimization), hasHyperTuning("GRID")))
  #dat <- nameRows(dat, 'cslda', and(isLabelingStrategy('CSLDA'), isOptimization, hasHyperTuning("none")))
  #dat <- nameRows(dat, 'cslda_grid', and(isLabelingStrategy('CSLDA'), isOptimization, hasHyperTuning("GRID")))
  
  # logresp_lda
  #dat <- nameRows(dat, 'logresp_lda', and(isLabelingStrategy('LOGRESP_LDA'), isOptimization, hasHyperTuning("none")))
  dat <- nameRows(dat, 'logresp_lda_s', and(isLabelingStrategy('LOGRESP_LDA'), not(isOptimization), hasHyperTuning("none")))
  
  # make 'algorithm' into factor 
  dat$algorithm <- factor(dat$algorithm)
  
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
  # y tick label formatter
  yformatter <- function(y){
    sprintf("%1.2f",y)
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
    #scale_y_continuous(labels=yformatter) +
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
setwd('/aml/home/plf1/git/Experiments/plf1/CONNL-2015-CSLDA-submission/csv')


# stop execution --- proceed manually
stop()

# rerunning all experiments for a CONNL submission
data = read.csv("2015-04-29-connl.csv")


#########################################################
#             Prototyping
#########################################################
mdata <- massageData(data); d <- mdata
mdata <- massageDataLegacy(data); d <- mdata
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
d = mdata[which(mdata$corpus=="WEATHER"),]
d = mdata[which(mdata$corpus=="AIRLINES"),]
d = mdata[which(mdata$corpus=="COMPANIES"),]

d = mdata[which(mdata$dataset=="weather.json"),]
d = mdata[which(mdata$dataset=="weather-augmented.json"),]
d = mdata[which(mdata$dataset=="weather-augmented-big.json"),]
d = mdata[which(mdata$dataset=="cfgroups1000.json"),]

d = d[which(d$algorithm=="baseline" | d$algorithm=="varmomresp" | d$algorithm=="cslda"),]

facets <- "~annotator_accuracy~corpus~dataset~num_topics~vary_annotator_rates"
xvarname <- "num_annotations"
plotAlgorithms(d,"labeled_acc","Inferred Label Accuracy",ymin=0.4,ymax=1,facets=facets,xvarname=xvarname)
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
plotAlgorithms(d,"inference_secs","Inferred Label Accuracy",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"num_annotations","Inferred Label Accuracy",ymin=0,facets=facets,xvarname=xvarname)
plotAlgorithms(d,"num_documents_with_annotations","Docs with Annotations",ymin=0,facets=facets,xvarname=xvarname)

plot(d$log_joint, d$labeled_acc)
j = d[which(d$algorithm!='itemresp' & d$algorithm!='momresp'),]
plotAlgorithms(j,"machacc_rmse","Machine RMSE",ymin=0)
plotAlgorithms(j,"machacc_mat_rmse","Machine MAT RMSE")

















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
