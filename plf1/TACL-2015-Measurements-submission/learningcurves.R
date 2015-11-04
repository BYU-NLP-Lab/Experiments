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
  grepl("maximize", dat$training)
}

hasHyperTuning <- function(tuningMethod){
  function(dat){
    grepl(tuningMethod, dat$hyperparam_training)
  }
}


basedirMatches <- function(pattern){
  function(dat){
    grepl(pattern,dat$basedir)
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

hasNumMeasurements <- function(num_meas){
  function(dat){
    dat$meas_eval_point==num_meas
  }
}

isLabelingStrategy <- function(labeling_strategy){
  function(dat){
    desiredLabelingStrategy <- if(is.null(labeling_strategy)) dat$labeling_strategy else labeling_strategy
    dat$labeling_strategy==desiredLabelingStrategy
  }
}

not <- function(f){
  function(dat){
    !f(dat)
  } 
}

or <- function(...){
  function(dat){
    criteria <- list(...)
    # start with everything matched
    matchedRows <- rep(TRUE,dim(dat)[1])
    # intersection of rows that match each criterion
    for (criterion in criteria){
      matchedRows <- matchedRows | criterion(dat)
    }
    return(matchedRows)
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
  
  # PAN
  dat <- nameRows(dat, 'pan10', and(isLabelingStrategy('PAN'), hasNumMeasurements(10000)))
  dat <- nameRows(dat, 'pan5', and(isLabelingStrategy('PAN'), hasNumMeasurements(5000)))
  dat <- nameRows(dat, 'pan2', and(isLabelingStrategy('PAN'), hasNumMeasurements(2000)))
  dat <- nameRows(dat, 'pan', and(isLabelingStrategy('PAN'), hasNumMeasurements(0)))
  # dat <- nameRows(dat, 'pan', and(isLabelingStrategy('PAN'), or(hasNumMeasurements(0), basedirMatches(".*loclabels.*"))))
  
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
  
  # make 'corpus' into factor
  dat$corpus <- factor(dat$corpus)
  
  
  ########## miscellaneous ##################
  
  # name num_annotators into a factor (so it can be used as a plotting facet)
  if (!is.null(dat$num_annotators)){
    dat$num_annotators <- factor(dat$num_annotators)
  }

  # bucket annotations (in case of a little jitter)
  dat
  
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
setwd('/aml/home/plf1/git/Experiments/plf1/TACL-2015-Measurements-submission/csv')


# stop execution --- proceed manually
stop()

# experiments with:
# newsgroups, cfnewsgroups, weather, twittersentiment, twitterparaphrase, compatibility
data = read.csv("2015-08-20.csv")


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
#         Learning from Measuremnts in Crowdsourcing models 2015
#############################################################################



######################### newsgroups (simulated predicates and real proportions) ###############################
data = read.csv("2015-08-20-newsgroupssim.csv")
mdata <- massageData(data);
# eliminate jitter
mdata$num_annotations <- floor(mdata$num_annotations/5000)*5000

levels=c('PAN10','PAN5','PAN','IR','MV') # determined line order in legend
alg_colors=c('PAN10'='#F8766D', 'PAN5'='#00BEC4', 'PAN'='#B69E00', 'MV'="#000000", 'IR'="#609BFF")
alg_shapes=c('PAN10'=5,         'PAN5'=18,        'PAN'=3,         'MV'=1,         'IR'=17)
width = 13
height = 9
ymin = 0.57
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varitemresp','pan10','pan5','pan'), to=c('MV','IR','PAN10','PAN5','PAN')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of document labels x 1,000")
}
plotty(mdata[which(mdata$corpus=="NEWSGROUPS"),])
ggsave("../images/newsgroups.eps",width=width,height=height,units='cm')


######################### weather-preds (real predicates and manual proportions) ###############################
data = read.csv("2015-08-20-weatherreal.csv")
mdata <- massageData(data);

levels=c('PAN2','PAN','IR','MV') # determined line order in legend
alg_colors=c('PAN2'='#F8766D', 'PAN5'='#00BEC4', 'PAN'='#B69E00', 'MV'="#000000", 'IR'="#609BFF")
alg_shapes=c('PAN2'=5,         'PAN5'=18,        'PAN'=3,         'MV'=1,         'IR'=17)
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varitemresp','pan10','pan'), to=c('MV','IR','PAN2','PAN')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of document labels x 1,000")
}
plotty(mdata[which(mdata$corpus=="WEATHER"),])
ggsave("../images/weather-preds.eps",width=width,height=height,units='cm')




######################### weather-loclabels (real predicates and manual proportions) ###############################
data = read.csv("2015-08-20-weather-loclabels.csv")
mdata <- massageData(data);
# make an x axis that consist of max(eval_point,meas_eval_point) because we sweep eval_point for MV and IR and meas_eval_point for PAN
# mdata$effective_eval_point <- ifelse(mdata$algorithm=="pan", mdata$meas_eval_point, mdata$eval_point)

levels=c('PAN+LOC','PAN','IR','MV') # determined line order in legend
alg_colors=c('PAN+LOC'='#00B937', 'PAN5'='#00BEC4', 'PAN'='#B69E00', 'MV'="#000000", 'IR'="#609BFF")
alg_shapes=c('PAN+LOC'=6,         'PAN5'=18,        'PAN'=3,         'MV'=1,         'IR'=17)
width = 13
height = 8
ymin = 0.55
ymax = 1
shapesize = 3
xvarname = "num_annotations"
# data
mdata$algorithm <- mapvalues(mdata$algorithm, from=c('baseline','varitemresp','pan2'), to=c('MV','IR','PAN+LOC')) # rename
mdata$algorithm <- factor(mdata$algorithm, levels=levels) # reorder
plotty <- function(d,hide_legend=FALSE){
  plotAlgorithms(d,"labeled_acc","",xvarname=xvarname,ymin=ymin,ymax=ymax,facets="~corpus", shapesize=shapesize, algorithm_colors=alg_colors, algorithm_shapes=alg_shapes,
                 hide_legend=hide_legend,xlabel="Number of document labels x 1,000")
}
plotty(mdata[which(mdata$corpus=="WEATHER"),])
ggsave("../images/weather-loclabels.eps",width=width,height=height,units='cm')



######################### weather-activemeas ###############################
data = read.csv("2015-08-24-weather-activemeas.csv")
#mdata <- massageData(data);
# make an x axis that consist of max(eval_point,meas_eval_point) because we sweep eval_point for MV and IR and meas_eval_point for PAN
# mdata$effective_eval_point <- ifelse(mdata$algorithm=="pan", mdata$meas_eval_point, mdata$eval_point)

data$total_meas <- data$num_annotations + data$num_measurements
data = data[which(data$total_meas<=17000),]
data$total_meas <- data$total_meas/ 1000
al <- data[which(is.na(as.character(data$active_strategy))),]
rand <- data[which(as.character(data$active_strategy)=="RAND"),]


ggplot() +
  # Active measurement line
  geom_line(aes(al$total_meas, al$labeled_acc, colour="ACTIVE"), size=1, linetype=1, al) +
  # Rand line
  geom_line(aes(rand$total_meas, rand$labeled_acc, colour="RAND"), size=1, linetype=2, rand) +
  scale_colour_manual("", 
                      breaks = c("ACTIVE", "RAND"),
                      values = c("#F8766D", "#000000")) +
  ylab("Accuracy") + 
  xlab("Number of measurements x 1,000") +
  ylim(0.68,1) +
  #ggtitle("") +
  #scale_x_continuous(labels=xformatter) +
  #scale_x_continuous(labels=xformatter, breaks = xbreaks) +
  #theme(legend.position='none') +
  #scale_x_continuous(limits=xlim,labels=xformatter) +
  #theme(plot.title = element_text(lineheight=1.8,face='bold')) +
  theme_bw() 

# save plot
width = 13
height = 8
ggsave("../images/weather-activemeas.eps",width=width,height=height,units='cm')




