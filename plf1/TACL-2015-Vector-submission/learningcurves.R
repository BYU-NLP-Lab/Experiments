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
setwd('/aml/home/plf1/git/Experiments/plf1/TACL-2015-Vector-submission/csv')


# stop execution --- proceed manually
stop()

# experiments with:
# newsgroups, cfnewsgroups, weather, twittersentiment, twitterparaphrase, compatibility
data = read.csv("2015-06-25-taacl.csv")


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

height = 9

######################### newsgroups ###############################
data = read.csv("2015-06-25-taacl.csv")

levels=c('LogResp+w2v','LogResp','csLDA','MomResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'LogResp+w2v'="#F8766D", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'LogResp+w2v'=5,         'ItemResp'=6 ) 
width = 13
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data);
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varlogresp" |  mdata$algorithm=="varmomresp" | mdata$algorithm=="cslda_s" | mdata$algorithm=="baseline"),]
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varlogresp','varlogresp_w2v','cslda_s','varitemresp','varmomresp'), to=c('Majority','LogResp','LogResp+w2v','csLDA','ItemResp','MomResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),])
ggsave("../images/newsgroups.eps",width=width,height=height,units='cm')


######################### cfgroups1000 ###############################
data = read.csv("2015-06-25-taacl.csv")

levels=c('LogResp+w2v','LogResp','csLDA','MomResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'LogResp+w2v'="#F8766D", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'LogResp+w2v'=5,         'ItemResp'=6 ) 
width = 13
ymin = 0.0
ymax = 0.75
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data);
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varlogresp" |  mdata$algorithm=="varmomresp" | mdata$algorithm=="cslda_s" | mdata$algorithm=="baseline"),]
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varlogresp','varlogresp_w2v','cslda_s','varitemresp','varmomresp'), to=c('Majority','LogResp','LogResp+w2v','csLDA','ItemResp','MomResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus~dataset", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="CFGROUPS1000"),])
ggsave("../images/cfgroups1000.eps",width=width,height=height,units='cm')





##################################### general tweet sentiment ###################################
data = read.csv("2015-06-25-taacl.csv")
levels=c('LogResp+w2v','LogResp','csLDA','MomResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'LogResp+w2v'="#F8766D", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'LogResp+w2v'=5,         'ItemResp'=6 ) 
width = 13
ymin = 0.6
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
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varlogresp" |  mdata$algorithm=="cslda_s" | mdata$algorithm=="varmomresp" | mdata$algorithm=="varitemresp" |  mdata$algorithm=="baseline"),]
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varlogresp','varlogresp_w2v','cslda_s','varitemresp','varmomresp'), to=c('Majority','LogResp','LogResp+w2v','csLDA','ItemResp','MomResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="TWITTER_SENT"),])
ggsave("../images/twittersentiment.eps",width=width,height=height,units='cm')


######################################## weather tweet sentiment #######################################
data = read.csv("2015-06-25-taacl.csv")
levels=c('LogResp+w2v','LogResp','csLDA','MomResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'LogResp+w2v'="#F8766D", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'LogResp+w2v'=5,         'ItemResp'=6 ) 
width = 13
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
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" | mdata$algorithm=="varlogresp" |  mdata$algorithm=="cslda_s" | mdata$algorithm=="varmomresp" | mdata$algorithm=="varitemresp" |  mdata$algorithm=="baseline"),]
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varlogresp','varlogresp_w2v','cslda_s','varitemresp','varmomresp'), to=c('Majority','LogResp','LogResp+w2v','csLDA','ItemResp','MomResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="WEATHER"),])
ggsave("../images/weather.eps",width=width,height=height,units='cm')


######################################## compatibility #######################################
data = read.csv("2015-06-25-taacl.csv")
levels=c('LogResp+w2v','LogResp','csLDA','MomResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'LogResp+w2v'="#F8766D", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'LogResp+w2v'=5,         'ItemResp'=6 ) 
width = 13
ymin = 0.93
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
mdata <- mdata[which(mdata$num_annotations>5000),]
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varlogresp','varlogresp_w2v','cslda_s','varitemresp','varmomresp'), to=c('Majority','LogResp','LogResp+w2v','csLDA','ItemResp','MomResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="COMPATIBILITY"),])
ggsave("../images/compatibility.eps",width=width,height=height,units='cm')


############################################# tweet paraphrase dataset ##############################################################
data = read.csv("2015-06-25-taacl.csv")
levels=c('LogResp+w2v','LogResp','csLDA','MomResp','ItemResp','Majority') # determined line order in legend
alg_colors=c('csLDA'='#00BEC4', 'Majority'="#000000", 'MomResp'="#B69E00", 'LogResp'="#609BFF", 'LogResp+w2v'="#F8766D", 'ItemResp'='#00B937')
alg_shapes=c('csLDA'=3,         'Majority'=1,         'MomResp'=17,        'LogResp'=18,        'LogResp+w2v'=5,         'ItemResp'=6 ) 
width = 13
ymin = 0.75
ymax = 0.95
shapesize = 3
xvarname = "num_annotations"
# data
mdata <- massageData(data); mdata <- mdata[which(!grepl("itemresp_",mdata$algorithm)),] # strip useless itemresp_w2v
# mdata <- mdata[which(grepl("discrim",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("itemresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("momresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("logresp",mdata$algorithm) | mdata$algorithm=="baseline"),]
# mdata <- mdata[which(grepl("cslda",mdata$algorithm) | mdata$algorithm=="baseline"),]
mdata <- mdata[which(mdata$algorithm=="varlogresp_w2v" |  mdata$algorithm=="varitemresp" | mdata$algorithm=="baseline"),]
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varlogresp','varlogresp_w2v','cslda_s','varitemresp','varmomresp'), to=c('Majority','LogResp','LogResp+w2v','csLDA','ItemResp','MomResp')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of annotations x 1,000")
}
plotty(mdata[which(mdata$corpus=="TWITTER_PARA"),])
ggsave("../images/twitterparaphrase.eps",width=width,height=height,units='cm')










