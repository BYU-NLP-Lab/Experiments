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

nameRows <- function(dat,name,
                     labeling_strategy,unlabeled_document_weight,optimization,doc_scaled,diag_method=NULL,diag_instances=NULL){
  rowDocScaled <- dat$pre_normalize_documents>-1
  desiredDocScaled <- if(is.null(doc_scaled)) rowDocScaled else doc_scaled
  desiredDiagMethod <- if(is.null(diag_method)) dat$diagonalization_method else diag_method
  desiredDiagInst <- if(is.null(diag_instances)) dat$diagonalization_gold_instances else diag_instances
  rowOptimization <- dat$samples==0
  desiredOptimization <- if(is.null(optimization)) rowOptimization else optimization
  desiredLabelingStrategy <- if(is.null(labeling_strategy)) dat$labeling_strategy else labeling_strategy
  desiredUnlabeledDocumentWeight <- if(is.null(unlabeled_document_weight)) dat$unlabeled_document_weight else unlabeled_document_weight
  rowUnlabeledDocumentWeight <- if(is.null(dat$unlabeled_document_weight)) desiredUnlabeledDocumentWeight else dat$unlabeled_document_weight
  rowUnlabeledDocumentWeight <- if(is.numeric(desiredUnlabeledDocumentWeight)) as.integer(as.character(rowUnlabeledDocumentWeight)) else rowUnlabeledDocumentWeight
  rows <- which(dat$labeling_strategy==desiredLabelingStrategy &
                rowUnlabeledDocumentWeight==desiredUnlabeledDocumentWeight &
                rowOptimization==desiredOptimization &
                rowDocScaled==desiredDocScaled &
                dat$diagonalization_method==desiredDiagMethod &
                dat$diagonalization_gold_instances==desiredDiagInst)
  dat$algorithm[rows] <- name
  return(dat)
}

diagonalizationLPVsGreedyMassageData <- function(dat,sampled_results=FALSE){
  num_rows = dim(dat)[1]
  dat$algorithm <- rep("invalid",num_rows)
  #   dat <- nameRows(dat,'ALL',        'neutered',NULL,       NULL, NULL, NULL, NULL)
  dat <- nameRows(dat,'LP',        'neutered',NULL,       NULL, TRUE, "GOLD", -1)
  dat <- nameRows(dat,'Greedy',  'neutered',NULL,       FALSE, TRUE, "GREEDY", NULL)
  # make 'algorithm' into factor (and re-order)
  dat$algorithm <- factor(dat$algorithm, levels=c('LP','Greedy','invalid')) 
  
  # rename annotator accuracies and re-order
  require(plyr)
  dat$accuracy_level <- mapvalues(dat$accuracy_level, from=c('NOISY','VERY_NOISY','CROWD'), to=c('HIGH','MED','LOW'))
  dat$accuracy_level <- factor(dat$accuracy_level, levels = c('CONFLICT','LOW','MED','HIGH'))
  
  # rename k to 'd' and re-order
  dat$d <- sprintf("d = %d",dat$k)
  dat$d <- factor(dat$d, levels = c('d = 1','d = 2','d = 3','d = 5','d = 10'))
  
  # remove first data point (too noisy)
  dat <- dat[which(dat$num_annotations>1),]
  
  # report invalid rows (weren't selected as part of a cohesive algorithm)
  #valid_rows = which(dat$algorithm!='invalid')
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat[valid_rows,])
}

diagonalizationMassageData <- function(dat,sampled_results=FALSE){
  num_rows = dim(dat)[1]
  dat$algorithm <- rep("invalid",num_rows)
#   dat <- nameRows(dat,'ALL',        'neutered',NULL,       NULL, NULL, NULL, NULL)
dat <- nameRows(dat,'CorrectCountAll',        'neutered',NULL,       NULL, TRUE, "GOLD", -1)
dat <- nameRows(dat,'CorrectCount20',        'neutered',NULL,       NULL, TRUE, "GOLD", 20)
dat <- nameRows(dat,'CorrectCount50',        'neutered',NULL,       NULL, TRUE, "GOLD", 50)
dat <- nameRows(dat,'CorrectCount100',        'neutered',NULL,       NULL, TRUE, "GOLD", 100)
dat <- nameRows(dat,'CorrectCount200',        'neutered',NULL,       NULL, TRUE, "GOLD", 200)
  dat <- nameRows(dat,'BestAnnotator',     'neutered',NULL,       NULL, TRUE, "MAX_GAMMA", -1)
  dat <- nameRows(dat,'AggregateAnn',      'neutered',NULL,       NULL, TRUE, "AVG_GAMMA", -1)
  dat <- nameRows(dat,'None',              'neutered',NULL,       NULL, TRUE, "RAND", -1)
  dat <- nameRows(dat,'GreedyCorrectCount',  'neutered',NULL,       FALSE, TRUE, "GREEDY", NULL)
  # make 'algorithm' into factor (and re-order)
  dat$algorithm <- factor(dat$algorithm, levels=c('None','CorrectCountAll','CorrectCount200','CorrectCount100','CorrectCount50','CorrectCount20','BestAnnotator','AggregateAnn','GreedyCorrectCount','invalid')) 
  
  # rename annotator accuracies and re-order
  require(plyr)
  dat$accuracy_level <- mapvalues(dat$accuracy_level, from=c('NOISY','VERY_NOISY','CROWD'), to=c('HIGH','MED','LOW'))
  dat$accuracy_level <- factor(dat$accuracy_level, levels = c('CONFLICT','LOW','MED','HIGH'))
  
  # rename k to 'd' and re-order
  dat$d <- sprintf("d = %d",dat$k)
  dat$d <- factor(dat$d, levels = c('d = 1','d = 2','d = 3','d = 5','d = 10'))

  # remove first data point (too noisy)
  dat <- dat[which(dat$num_annotations>1),]

  # report invalid rows (weren't selected as part of a cohesive algorithm)
  #valid_rows = which(dat$algorithm!='invalid')
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat[valid_rows,])
}

massageData <- function(dat,sampled_results=FALSE){
  num_rows = dim(dat)[1]
  dat$algorithm <- rep("invalid",num_rows)
  
  dat <- nameRows(dat,'majority',   'ubaseline',NULL,      NULL, TRUE)  
  dat <- nameRows(dat,'itemresp',    'itemresp',NULL,       TRUE, TRUE)    
  dat <- nameRows(dat,'momresp',      'neutered',NULL,       NULL, TRUE, "AVG_GAMMA", -1)
  
  # make 'algorithm' into factor (and re-order)
  dat$algorithm <- factor(dat$algorithm, levels=c('majority','itemresp','momresp','invalid')) 
  
  # rename annotator accuracies and re-order
  require(plyr)
  dat$accuracy_level <- mapvalues(dat$accuracy_level, from=c('NOISY','VERY_NOISY','CROWD'), to=c('HIGH','MED','LOW'))
  dat$accuracy_level <- factor(dat$accuracy_level, levels = c('CONFLICT','LOW','MED','HIGH'))
  
  # rename k to 'd' and re-order
  dat$d <- sprintf("d = %d",dat$k)
  dat$d <- factor(dat$d, levels = c('d = 1','d = 2','d = 3','d = 5','d = 10'))
  
  # remove first data point (too noisy)
  dat <- dat[which(dat$num_annotations>1),]
  
  # report invalid rows (weren't selected as part of a cohesive algorithm)
  #valid_rows = which(dat$algorithm!='invalid')
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat[valid_rows,])
}

plotAlgorithms <- function(dat, yvarname, title, ymin=0, ymax=1, ylabel="Accuracy", 
                           shapesize=1, xlim=NULL, divisor=10000, hideLegend=FALSE, corpusFacet=FALSE,
                           shapes=NULL,colors=NULL
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
  
  dfc <- summarySE(dat, measurevar=yvarname, groupvars=c("algorithm","num_annotations","d","accuracy_level","corpus"))
  if (!is.null(divisor)){
    dfc$num_annotations <- dfc$num_annotations/divisor
  }
  plt <- ggplot(dat=dfc, aes_string(x="num_annotations", y=yvarname, color="algorithm", group="algorithm")) + 
    ggtitle(title) +
    geom_errorbar(aes_string(ymin=sprintf("%s-sd",yvarname), ymax=sprintf("%s+sd",yvarname))) +
    geom_line(size=0.8) +
    geom_point(aes(shape=algorithm),size=shapesize,color='black') + 
    ylim(ymin,ymax) +
    ylab(ylabel) + 
    xlab(sprintf("Number of simulated annotations x %s",format(divisor,big.mark=',',big.interval=3))) + 
    scale_x_continuous(labels=xformatter) +
    facet_grid(ifelse(corpusFacet,"~d~accuracy_level~corpus","~d~accuracy_level")) + 
    theme(plot.title = element_text(lineheight=1.8,face='bold')) 
  if (!is.null(shapes)){
    plt <- plt + scale_shape_manual(values=shapes)
  }
  if (!is.null(colors)){
    plt <- plt + scale_colour_manual(values=colors)
  }
    
  if (hideLegend){
      plt <- plt + theme(legend.position='none') 
  }
#   scale_shape_manual(values=c('majority'=1, 'itemresp'='.', 'momresp'=',', 'multiresp'="x", 'itemresp_s'="*", 'momresp_s'=6, 'multiresp_s'="+"),guide=FALSE) 
    
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
require(ggplot2) 
setwd('/aml/home/plf1/altgit/statnlp/scripts/learningcurve')
#data = read.csv("1-jan-chains.csv")
#data = read.csv("1-jan-results.csv")
# data = read.csv("6-jan-results.csv")
# data = read.csv("chains.csv")
# data = read.csv("combined.csv")
# data = read.csv("results.csv")
# data = read.csv("diagonalized-chains.csv")
data = read.csv("lrec2014-results.csv")
#data = read.csv("24-jan-results.csv")
#data = read.csv("25-jan-results.csv")

# stop execution after reading data.proceed manually
stop()

#########################################################
#             Prototyping
#########################################################
mdata <- massageData(data)
# choose a dataset
# d = mdata[which(mdata$corpus=="REUTERS"),]
d = mdata[which(mdata$corpus=="ENRON"),]
d = mdata[which(mdata$corpus=="NB20"),]
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]

#d = d[which(d$accuracy_level!='CONFLICT'),]
#d = d[which(d$accuracy_level=='CONFLICT'),]

plotAlgorithms(d,"labeled_acc","Labeled Accuracy",ymin=.2)
plotAlgorithms(d,"unlabeled_acc","Unlabeled Accuracy")
plotAlgorithms(d,"heldout_acc","Heldout Accuracy")
plotAlgorithms(d,"overall_acc","Overall Accuracy")
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which((d$k==10 | d$k==1) & (d$accuracy_level=='LOW') & (d$algorithm=='majority' | d$algorithm=='multiresp')),]
plotAlgorithms(d,"heldout_acc","", ymin=.2,xlim=c(0,5000),shapesize=2,divisor=100,hideLegend=TRUE)
plotAlgorithms(d,"annacc_rmse","Annotator RMSE",ymin=0,ymax=.2,ylabel="Annotator RMSE")
plotAlgorithms(d,"annacc_mat_rmse","Annotator Matrix RMSE",ymin=0,ymax=.2)
plotAlgorithms(d,"machacc_rmse","Machine RMSE",ymin=0,ymax=.1)

#########################################################
#             LREC 2014 Paper
#########################################################
colors <- c(
  'None'='#555555', 
  'GreedyCorrectCount'='#E69F00', 
  'CorrectCountAll'='#009E73', 
  'BestAnnotator'='#56B4E9', 
  'AggregateAnn'='#D55E00', 
  'CorrectCount20'='#0072B2', 
  'CorrectCount50'='#E69F00', 
  'CorrectCount100'='#CC79A7', 
  'CorrectCount200'='#CC79A7', 
  
  'Greedy'='#E69F00', 
  'LP'='#009E73', 
  
  'itemresp'='#009E73',
  'majority'="#555555", 
  'momresp'="#D55E00"
  )
shapes <- c(
  'None'=1, 
  'GreedyCorrectCount'=20, 
  'CorrectCountAll'=4, 
  'BestAnnotator'=3, 
  'AggregateAnn'=20, 
  'CorrectCount20'=2,
  'CorrectCount50'=3,
  'CorrectCount100'=5,
  'CorrectCount200'=6,
  
  'Greedy'=20,
  'LP'=4,
  
  'itemresp'=6,
  'majority'=4, 
  'momresp'=20
  )

numann = 1.3e5

#### Diagonalization Methods ######
# LP vs Greedy
mdata <- diagonalizationLPVsGreedyMassageData(data)
d = mdata[which(mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & mdata$k==1 & (mdata$accuracy_level=="LOW")),]
plotAlgorithms(d,"labeled_acc","",ymin=0.6,ymax=.8,shapesize=2,colors=colors,shapes=shapes)
ggsave("lrec2014-class-correspondence1.eps",width=12,height=7,units='cm')

# CorrectCount at various levels
mdata <- diagonalizationMassageData(data)
d = mdata[which(mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & mdata$k==1 & grepl('^CorrectCount',mdata$algorithm) & (mdata$accuracy_level=="CONFLICT" | mdata$accuracy_level=="LOW")),]
plotAlgorithms(d,"labeled_acc","",ymin=0.3,ymax=.85,shapesize=2,colors=colors,shapes=shapes)
ggsave("lrec2014-class-correspondence2.eps",width=20,height=7,units='cm')

# Annotator methods
mdata <- diagonalizationMassageData(data)
d = mdata[which(mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & mdata$k==1 & mdata$algorithm!='GreedyCorrectCount' & !grepl('CorrectCount[0-9]',mdata$algorithm) & (mdata$accuracy_level=="CONFLICT" | mdata$accuracy_level=="LOW")),]
plotAlgorithms(d,"labeled_acc","",ymin=0,ymax=.85,shapesize=2.5,colors=colors,shapes=shapes)
ggsave("lrec2014-class-correspondence3.eps",width=20,height=7,units='cm')


#### Label Accuracy ######

# labeling accuracy (good results)
mdata <- massageData(data) 
d = mdata[which(mdata$algorithm!='itemresp' & mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & (mdata$accuracy_level=='CONFLICT' | mdata$accuracy_level=='LOW')),]
plotAlgorithms(d,"labeled_acc","",ymin=.15,shapesize=2,colors=colors,shapes=shapes)
ggsave("lrec2014-label-accuracy.eps",width=20,height=9,units='cm')

# labeling accuracy (bad results)
mdata <- massageData(data) 
d = mdata[which(mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & mdata$k==3 & (mdata$accuracy_level=='MED' | mdata$accuracy_level=='HIGH')),]
plotAlgorithms(d,"labeled_acc","",ymin=.27,shapesize=1.2,colors=colors,shapes=shapes)
ggsave("lrec2014-label-accuracy-bad.eps",width=12,height=5,units='cm')

# annotator error estimation (good results)
mdata <- massageData(data) 
d = mdata[which(mdata$algorithm!='itemresp' & mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & (mdata$accuracy_level=='CONFLICT' | mdata$accuracy_level=='LOW')),]
plotAlgorithms(d,"annacc_rmse","",ymax=.77,ymin=0,shapesize=2,colors=colors,shapes=shapes,ylabel="Annotator RMSE")
ggsave("lrec2014-annotator-error.eps",width=20,height=9,units='cm')

# annotator error estimation (bad results)
mdata <- massageData(data) 
d = mdata[which(mdata$k!=10 & mdata$num_annotations<numann & mdata$corpus=="NEWSGROUPS" & mdata$k==3 & (mdata$accuracy_level=='MED' | mdata$accuracy_level=='HIGH')),]
plotAlgorithms(d,"annacc_rmse","",ymax=.5,ymin=0,shapesize=1.2,colors=colors,shapes=shapes,ylabel="Annotator RMSE")
ggsave("lrec2014-annotator-error-bad.eps",width=12,height=5,units='cm')
