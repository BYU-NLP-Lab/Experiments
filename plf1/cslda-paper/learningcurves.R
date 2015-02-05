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
  
  # pfelt modification: remove all instances with N=1 (they 
  # will have NAs in the sd)
  datac = datac[which(datac$N>1),]
  
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

  # make 'algorithm' into factor (and re-order)
  dat$algorithm <- factor(dat$algorithm)
#   dat$algorithm <- factor(dat$algorithm, levels=c(
#     'cslda','cslda_grid',
#     'itemresp_s','itemresp_s_grid','itemresp_s_bob','itemresp','itemresp_grid','itemresp_bob',
#     'varitemresp','varitemresp_grid','varitemresp_bob',
#     'varmomresp','momresp_s','momresp',
#     'multiresp_sm','multiresp_s','multiresp_m','multiresp','varmultiresp',
#     'varraykar','raykar_st','raykar',
#     'baseline','invalid')) 
  
  # rename k to 'd' and re-order
  require(plyr)
  dat$d <- sprintf("d = %d",dat$k)
  dat$d <- factor(dat$d, levels = c('d = 1','d = 2','d = 3','d = 5','d = 10'))
  
  # remove first data point (too noisy)
  #print(c("dropping points where num_annotations<=30:",length(which(dat$num_annotations<=30))))
  #dat <- dat[which(dat$num_annotations>30),]
  
  # report invalid rows (weren't selected as part of a cohesive algorithm)
  #valid_rows = which(dat$algorithm!='invalid')
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat[valid_rows,])
}

plotAlgorithms <- function(dat, yvarname, title, ymin=0, ymax=1, ylabel="Accuracy", xlabel="Number of annotated instances x %s",
                           shapesize=1, xlim=NULL, divisor=1000, hideLegend=FALSE, qualityFacet=FALSE, corpusFacet=FALSE, depthFacet=FALSE
                           ){
  # a modified colorblind-friendly pallette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  
  # x tick label formatter
  xformatter <- function(x){
#     sci <- format(x,scientific=TRUE)
#     sci <- gsub('^[0\\.]+e.*','0',sci)
#     sci <- gsub('\\.\\de\\+0','e',sci)
#     gsub('e\\+0','e',sci)
    gsub('\\.0','',format(x))
  }

  if (is.null(dat$num_annotated_instances)){
    dat$num_annotated_instances <- round(dat$num_annotations / d$k)
  }

  dfc <- summarySE(dat, measurevar=yvarname, groupvars=c("algorithm","num_annotated_instances","d","accuracy_level","corpus","diagonalization_method"))
  if (!is.null(divisor)){
    dfc$num_annotated_instances <- dfc$num_annotated_instances/divisor
  }

  plt <- ggplot(dat=dfc, aes_string(x="num_annotated_instances", y=yvarname, color="algorithm", group="algorithm")) + 
    ggtitle(title) +
    geom_errorbar(aes_string(ymin=sprintf("%s-sd",yvarname), ymax=sprintf("%s+sd",yvarname))) +
    geom_line(size=0.8) +
    geom_point(aes(shape=algorithm),size=shapesize,color='black') + 
    ylim(ymin,ymax) +
    ylab(ylabel) + 
    xlab(sprintf(xlabel,format(divisor,big.mark=',',big.interval=3))) + 
    scale_x_continuous(labels=xformatter) +
    theme(plot.title = element_text(lineheight=1.8,face='bold')) 
    #scale_colour_manual(values=c('Majority'="#619Cff", 'MomResp'="#F8766D",'MomResp+MF'="#F8766D", 'LogResp'="#00BA38",'LogResp+MF'="#00BA38", 'MomResp+Gibbs'="#777777",'LogResp+EM'="#777777")) +
    #scale_shape_manual(values=c('Majority'=1, 'MomResp'=17,'MomResp+MF'=17, 'LogResp'=18,'LogResp+MF'=18,'LogResp+EM'=3, 'MomResp+Gibbs'=3)) 
    #scale_colour_manual(values=c('baseline'="#555555", 'itemresp'="#009E73", 'momresp'="#56B4E9", 'multiresp'="#D5005E",'multiresp_m'="#5ED500", 'itemresp_s'="#0072B2", 'momresp_s'="#E69F00", 'multiresp_s'="#CC79A7", 'multiresp_sm'="#79CCA7")) +
    #scale_shape_manual(values=c('baseline'=1, 'itemresp'=17, 'momresp'=18, 'multiresp'=3, 'multiresp_m'=4, 'itemresp_s'=5, 'momresp_s'=6, 'multiresp_s'=0, 'multiresp_sm'=0)) 

  # facets
  plotfacets <- ""
  if (depthFacet){
    plotfacets <- paste(plotfacets,"~d",sep="")
  }
  if(corpusFacet){
    plotfacets <- paste(plotfacets,"~corpus",sep="")
  }
  if (qualityFacet){
    plotfacets <- paste(plotfacets,"~accuracy_level",sep="")
  }
  if (nchar(plotfacets)>0){
    plt <- plt + facet_grid(plotfacets)
  }

  if (hideLegend){
      plt <- plt + theme(legend.position='none') 
  }
#   scale_shape_manual(values=c('baseline'=1, 'itemresp'='.', 'momresp'=',', 'multiresp'="x", 'itemresp_s'="*", 'momresp_s'=6, 'multiresp_s'="+"),guide=FALSE) 
    
#   palette=c("#555555", "#009E73", "#56B4E9", "#D55E00", "#0072B2", "#E69F00", "#CC79A7")
#    scale_fill_manual(values=palette)
#   if (!is.null(specialMultiannPointColor)){
#     plt <- plt + geom_point(dat=dfc[which(dfc$algorithm=='multiresp'),],aes(color='black'),shape=4)
#     plt <- plt + guides(color = guide_legend(override.aes = list(color=c('red','green','blue','orange'))));#=c(palette[1],palette[2],palette[3],'black'))))
#   }
  if (!is.null(xlim)){
    plt <- plt + scale_x_continuous(limits=xlim,labels=xformatter) 
  }
  return(plt)
}

stop()

#install.packages("ggplot2")
require(ggplot2)
setwd('/aml/home/plf1/git/Experiments/plf1/crowdsourcing/learningcurve/csv')
data = read.csv("2014-08-08.csv")
data = read.csv("2014-10-07-partial-nips.csv")
data = read.csv("2014-10-08-partial-nips.csv")
data = read.csv("2015-01-16-after-refactoring.csv")
data = read.csv("2015-01-22-hyperparameter-tuning.csv")
data = read.csv("2015-01-23-hyperparam-tuning-wrtla.csv")
data = read.csv("2015-01-27-icml.csv")
data = read.csv("2015-01-30-optimized-sampler-cslda.csv")
data = read.csv("2015-01-31-optimized-sampler-cslda.csv")
data = read.csv("2015-02-02-icml.csv")
data = read.csv("2015-02-03-icml-withoutreplacement.csv")
data = read.csv("2015-02-04-topics.csv")


# stop execution after reading data.proceed manually
stop()

#########################################################
#             Prototyping
#########################################################
mdata <- massageData(data)
# choose a dataset
d <- mdata
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

d = d[which(d$diagonalization_method=="GOLD"),]
d = d[which(d$num_annotations<1000),]
d = d[which(d$algorithm=="itemresp" | d$algorithm=="varitemresp" | d$algorithm=="itemresp_s" ),]
d = d[which(d$algorithm=="itemresp_bob" | d$algorithm=="varitemresp_bob" | d$algorithm=="itemresp_s_bob" ),]
alg <- "cslda_s_grid"
d = d[which(d$algorithm=="baseline" | d$algorithm==alg | d$algorithm==paste(alg,'bob',sep='_') | d$algorithm==paste(alg,'grid',sep='_') ),]
d = d[which(d$algorithm==alg | d$algorithm==paste(alg,'bob',sep='_') | d$algorithm==paste(alg,'grid',sep='_') ),]
d = d[which(d$algorithm==alg | d$algorithm==paste(alg,'grid',sep='_') ),]
d = d[which(d$algorithm==alg | d$algorithm==paste(alg,'bob',sep='_') ),]
d = d[which(d$algorithm==paste(alg,'bob',sep='_') ),]
d = d[which(d$algorithm==alg),]
e = d[which(d$algorithm=="itemresp_grid"),]
f = d[which(d$algorithm=="itemresp_bob"),]
f = d[which(d$algorithm=="itemresp_s"),]
d = d[which(d$algorithm=="itemresp" | d$algorithm=="varitemresp" | d$algorithm=="itemresp_s"),]
d = d[which(d$algorithm=="itemresp_grid" | d$algorithm=="varitemresp_grid" | d$algorithm=="itemresp_s_grid"),]
d = d[which(d$algorithm=="itemresp_bob" | d$algorithm=="varitemresp_bob" | d$algorithm=="itemresp_s_bob"),]

#d = d[which(d$accuracy_level!='CONFLICT'),]
#d = d[which(d$accuracy_level=='CONFLICT'),]

plotAlgorithms(d,"labeled_acc","Inferred Label Accuracy",ymin=0,corpusFacet=TRUE,qualityFacet=TRUE,depthFacet=TRUE)
plotAlgorithms(d,"btheta","BTheta",ymin=0,ymax=2.5,corpusFacet=TRUE,qualityFacet=TRUE,depthFacet=TRUE)
plotAlgorithms(d,"bgamma","BGamma",ymin=0,corpusFacet=TRUE,qualityFacet=TRUE,depthFacet=TRUE)
plotAlgorithms(d,"cgamma","CGamma",ymin=0,ymax=50,corpusFacet=TRUE,qualityFacet=TRUE,depthFacet=TRUE)
plotAlgorithms(d,"bphi","BPhi",ymin=0,ymax=2,corpusFacet=TRUE,qualityFacet=TRUE,depthFacet=TRUE)
plotAlgorithms(d,"top3_labeled_acc","Top 3 Labeled Accuracy",ymin=0)
plotAlgorithms(d,"unannotated_document_weight","Lambda",ymin=0)
plotAlgorithms(d,"num_trusted_labels","Num Trusted Labels",ymin=0,ymax=510)
plotAlgorithms(d,"unlabeled_acc","Unlabeled Accuracy")
plotAlgorithms(d,"heldout_acc","Heldout Accuracy")
plotAlgorithms(d,"overall_acc","Overall Accuracy")
plotAlgorithms(d,"annacc_rmse","Annotator RMSE",ymin=0,ylabel="Annotator RMSE")
plotAlgorithms(d,"annacc_mat_rmse","Annotator Matrix RMSE",ymin=0,ymax=.2)

j = d[which(d$algorithm!='itemresp' & d$algorithm!='momresp'),]
plotAlgorithms(j,"machacc_rmse","Machine RMSE",ymin=0)
plotAlgorithms(j,"machacc_mat_rmse","Machine MAT RMSE")
#########################################################
#             NIPS 2014 workshop Poster
#########################################################

# momresp vs logresp on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations<=90000),]
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"labeled_acc","20 Newsgroups",xlabel="Number of Simulated Annotations x 1,000",ymin=0.25,divisor=1000,shapesize=2, qualityFacet=TRUE,corpusFacet=FALSE)
ggsave("newsgroups-labeled.eps",width=20,height=6,units='cm')

# momresp vs logresp *heldout* on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations<=90000),] 
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"heldout_acc","20 Newsgroups",ylabel="Test Accuracy",ymin=0.25,divisor=1000,shapesize=2, qualityFacet=TRUE,corpusFacet=FALSE)
ggsave("newsgroups-heldout.eps",width=20,height=6,units='cm')

# momresp vs logresp *annacc* on newsgroups
d = mdata
d = d[which(d$corpus=="NEWSGROUPS" & d$num_annotations>200 & d$num_annotations<=90000),] 
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
d = d[which(d$d=="d = 1"),]
plotAlgorithms(d,"annacc_rmse","20 Newsgroups",ylabel="Annotator Accuracy RMSE",ymin=0.,divisor=1000,shapesize=2, qualityFacet=TRUE,corpusFacet=FALSE)
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
plotAlgorithms(d,"labeled_acc","20 Newsgroups",ymin=0.25,divisor=1000,shapesize=2, qualityFacet=TRUE,corpusFacet=FALSE)
ggsave("newsgroups-labeled.eps",width=20,height=6,units='cm')

# old vs new (variational vs EM for logresp) on r52 + med
d = mdata
d$algorithm <- mapvalues(d$algorithm, from=c('LogResp'), to=c('LogResp+MF'))
d = d[which(d$corpus=="R52" & d$num_annotations<=30000),]
d = d[which(d$algorithm=="LogResp+EM" | d$algorithm=="LogResp+MF"),]
d = d[which(d$d=="d = 3"),]
d = d[which(d$accuracy_level=="LOW"),]
plotAlgorithms(d,"labeled_acc","LogResp Inference",ymin=0.,ymax=0.6,divisor=1000,qualityFacet=TRUE,corpusFacet=TRUE,shapesize=2)
ggsave("var-versus-em.eps",width=10,height=5,units='cm')


# old vs new (variational vs Gibbs for momresp) on webkb + med
# needs 2014-10-09-chains-nips.csv
d = mdata
d$algorithm <- mapvalues(d$algorithm, from=c('MomResp'), to=c('MomResp+MF'))
d = d[which(d$corpus=="WEBKB" & d$num_annotations<=13000),]
d = d[which(d$algorithm=="MomResp+Gibbs" | d$algorithm=="MomResp+MF"),]
d = d[which(d$d=="d = 1"),]
d$k <- 3
d = d[which(d$accuracy_level=="MED"),]
plotAlgorithms(d,"labeled_acc","MomResp Inference",ymin=0.25,divisor=1000,qualityFacet=TRUE,corpusFacet=TRUE,shapesize=2)
ggsave("var-versus-gibbs.eps",width=10,height=5,units='cm')

# real (cfgroups1000)
d = mdata
d = d[which(d$corpus=="CFGROUPS1000"),]
d = d[which(d$algorithm=="Majority" | d$algorithm=="MomResp" | d$algorithm=="LogResp"),]
#plotAlgorithms(d,"top3_labeled_acc","Crowdflower Annotations",ymin=0.5,divisor=1000,corpusFacet=TRUE,shapesize=2)
plotAlgorithms(d,"labeled_acc","Crowdflower Annotations",xlab="Number of human annotations x %s",ymin=0.35,ymax=0.7,divisor=1000,qualityFacet=FALSE,corpusFacet=FALSE,shapesize=2)
ggsave("cfgroups1000.eps",width=10,height=5,units='cm')

# big grid for estimating crossover points (TODO: calculate this automatically)
d = mdata
d = d[which(d$algorithm=="MomResp" | d$algorithm=="LogResp" | d$algorithm=="Majority"),]
#d = d[which(d$corpus=="WEBKB" & d$accuracy_level=="HIGH"),]
#d = d[which(d$corpus=="WEBKB"),]
#d = d[which(d$corpus=="R52"),]
#d = d[which(d$corpus=="CADE12"),]
#d = d[which(d$corpus=="ENRON"),]
#d = d[which(d$corpus=="NEWSGROUPS" & d$accuracy_level=="CONFLICT"),]
#d = d[which(d$d=="d = 1"),]
d = d[which(d$d=="d = 3"),]
plotAlgorithms(d,"labeled_acc","Crossover Grid",ymin=0.0,divisor=1000,shapesize=2,qualityFacet=TRUE,corpusFacet=TRUE)
ggsave("crossover-grid.png",width=20,height=20,units='cm')


#########################################################
#             EMNLP Main Paper
#########################################################
mdata <- massageData(data)

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
plotAlgorithms(d,"labeled_acc","Inferred Label Accuracy",ymin=.27,shapesize=1)
ggsave("newsgroups-labeled.eps",width=20,height=12,units='cm')

# d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
# d = d[which(d$num_annotations<=60000 & d$k<=3 & (d$accuracy_level=='LOW' | d$accuracy_level=='CONFLICT')),]
# plotAlgorithms(d,"labeled_acc","",ymin=.25,shapesize=1)
# ggsave("newsgroups-labeled-zoom.eps",width=14,height=21,units='cm')

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which((d$k==10 | d$k==1) & (d$accuracy_level=='LOW') & (d$algorithm=='baseline' | d$algorithm=='multiresp')),]
plotAlgorithms(d,"heldout_acc","",xlim=c(0,5.2), ymin=.2,shapesize=2)
ggsave("newsgroups-heldout-1.eps",width=14,height=10,units='cm')

# d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
# d = d[which((d$k==10 | d$k==1) & (d$accuracy_level=='LOW') & (d$algorithm=='baseline' | d$algorithm=='multiresp')),]
# plotAlgorithms(d,"heldout_acc","", ymin=.2,shapesize=2)
# ggsave("newsgroups-heldout-2.eps",width=8,height=10,units='cm')

d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
#d = d[which(d$k==1 & d$accuracy_level=='LOW'),]
d = d[which(d$k==1 & (d$accuracy_level=='LOW' | d$accuracy_level=='CONFLICT')),]
plotAlgorithms(d,"annacc_rmse","",ymin=0,ymax=.35,ylabel="Annotator RMSE",shapesize=2)
ggsave("newsgroups-annacc.eps",width=14,height=7,units='cm')

d = mdata[which(mdata$corpus=="ENRON"),]
plotAlgorithms(d,"labeled_acc","",ymin=.2,shapesize=1)
ggsave("enron-labeled.eps",width=20,height=12,units='cm')

# d = mdata[which(mdata$corpus=="ENRON"),]
# d = d[which(d$num_annotations<=30000 & d$k<=3 & (d$accuracy_level=='LOW' | d$accuracy_level=='CONFLICT')),]
# plotAlgorithms(d,"labeled_acc","",ymin=.20,shapesize=1)
# ggsave("enron-labeled-zoom.eps",width=14,height=12,units='cm')

# bi-corpus zoomed-in graph
d = mdata
d = d[which((d$k<=3) & (d$accuracy_level=='CONFLICT')),]
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

