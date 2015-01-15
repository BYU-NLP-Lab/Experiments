
nameRows <- function(dat,name,
                     labeling_strategy,unlabeled_document_weight,optimization,doc_scaled){
  rowDocScaled <- dat$pre_normalize_documents>-1
  desiredDocScaled <- if(is.null(doc_scaled)) rowDocScaled else doc_scaled
  rowOptimization <- dat$samples==0
  desiredOptimization <- if(is.null(optimization)) rowOptimization else optimization
  desiredLabelingStrategy <- if(is.null(labeling_strategy)) dat$labeling_strategy else labeling_strategy
  desiredUnlabeledDocumentWeight <- if(is.null(unlabeled_document_weight)) dat$unlabeled_document_weight else unlabeled_document_weight
  rowUnlabeledDocumentWeight <- if(is.null(dat$unlabeled_document_weight)) desiredUnlabeledDocumentWeight else dat$unlabeled_document_weight
  rowUnlabeledDocumentWeight <- if(is.numeric(desiredUnlabeledDocumentWeight)) as.integer(as.character(rowUnlabeledDocumentWeight)) else rowUnlabeledDocumentWeight
  rows <- which(dat$labeling_strategy==desiredLabelingStrategy &
                  rowUnlabeledDocumentWeight==desiredUnlabeledDocumentWeight &
                  rowOptimization==desiredOptimization &
                  rowDocScaled==desiredDocScaled)
  dat$algorithm[rows] <- name
  return(dat)
}

massageData <- function(dat){
  num_rows = dim(dat)[1]
  dat$algorithm <- rep("invalid",num_rows)
  
  # baselines
    dat <- nameRows(dat,'baseline_n',        'baseline', NULL,   NULL, TRUE)
    dat <- nameRows(dat,'baseline',        'baseline', NULL,   NULL, FALSE)
  dat <- nameRows(dat,'soft baseline_n',   'ubaseline',NULL,      NULL, TRUE)  
  dat <- nameRows(dat,'soft baseline',   'ubaseline',NULL,      NULL, FALSE)  
  
  # multiresp variants
  dat <- nameRows(dat,'multiresp_n',        'multiresp',-1,      FALSE, TRUE)
  dat <- nameRows(dat,'multiresp',        'multiresp',-1,        FALSE, FALSE)  
  dat <- nameRows(dat,'multiresp_opt_n',    'multiresp',-1,      TRUE, TRUE)   
  dat <- nameRows(dat,'multiresp_opt',    'multiresp',-1,        TRUE, FALSE)  
#   dat <- nameRows(dat,'multiresp_du0_n',    'multiresp',0,      FALSE, TRUE)  
#   dat <- nameRows(dat,'multiresp_du0',    'multiresp',0,         FALSE, FALSE)  
#   dat <- nameRows(dat,'multiresp_du0_opt_n','multiresp',0,       TRUE, TRUE)   
#   dat <- nameRows(dat,'multiresp_du0_opt','multiresp',0,         TRUE, FALSE)  
  #   dat <- nameRows(dat,'multiresp_bin_n',    'multiresp','binary_classifier',FALSE,TRUE)
  #   dat <- nameRows(dat,'multiresp_bin',    'multiresp','binary_classifier',FALSE,FALSE)
  #   dat <- nameRows(dat,'multiresp_bin_opt_n','multiresp','binary_classifier',TRUE,TRUE)
  #   dat <- nameRows(dat,'multiresp_bin_opt','multiresp','binary_classifier',TRUE,FALSE)
  
  # neutered variants
  dat <- nameRows(dat,'neutered_n',        'neutered',NULL,     FALSE, TRUE)  
  dat <- nameRows(dat,'neutered',        'neutered',NULL,     FALSE, FALSE)  
  dat <- nameRows(dat,'neutered_opt_n',    'neutered',NULL,       TRUE, TRUE)
  dat <- nameRows(dat,'neutered_opt',    'neutered',NULL,       TRUE, FALSE)  
  
  # itemresp variants
  #   dat <- nameRows(dat,'itemresp_n',        'itemresp',NULL,  FALSE, TRUE) 
  #   dat <- nameRows(dat,'itemresp',        'itemresp',NULL,    FALSE, FALSE)  
  dat <- nameRows(dat,'itemresp_opt_n',    'itemresp',NULL,       TRUE, TRUE)    
  dat <- nameRows(dat,'itemresp_opt',    'itemresp',NULL,        TRUE, FALSE)  
  
  # make into factor
  dat$algorithm <- as.factor(dat$algorithm)
  
  # report invalid rows (weren't selected as part of a cohesive algorithm)
  valid_rows = which(dat$algorithm!='invalid')
  print(c("total rows:",num_rows))
  print(c("valid rows:",length(valid_rows)))
  print(c("invalid rows:",length(which(dat$algorithm=='invalid'))))
  return(dat[valid_rows,])
}


#install.packages("ggplot2")
require(ggplot2)
setwd('/aml/home/plf1/altgit/statnlp/scripts/correlation')
data = read.csv("results.csv")
#data = read.csv("baseline.csv")

# identify plottable algorithms
mdata <- massageData(data)

# break execution
stop()

#########################################################
#            Prototyping
#########################################################

# choose a dataset
# d = data[which(mdata$corpus=="REUTERS"),]
d = mdata[which(mdata$corpus=="ENRON"),]
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = mdata[which(mdata$corpus=="NB20"),]

# choose document length
d = d[which(d$pre_normalize_documents==-1),]  # no doc scaling
#d = d[which(d$pre_normalize_documents==1),]
d = d[which(d$pre_normalize_documents==123),] # doc scaling

# choose eval point
d = d[which(d$num_annotations==0),] # eval_point==0
d = d[which(d$num_annotations==15000),] # eval_point==15,000

# choose inference style
d = d[which(d$annealing_schedule=='1'),] # no annealing
d = d[which(d$annealing_schedule!='1'),] # annealing


# heldout
cor(d$heldout_acc,d$log_joint)
print(ggplot(d, aes(x=log_joint,y=heldout_acc,color=annealing_schedule)) + geom_point(shape=1) + 
        facet_grid(~k~accuracy_level)+ ggtitle("heldout_acc") )

# unlabeled
cor(d$unlabeled_acc,d$log_joint)
print(ggplot(d, aes(x=log_joint,y=unlabeled_acc,color=annealing_schedule)) + geom_point(shape=1) + 
        facet_grid(~k~accuracy_level)+ ggtitle("unlabeled_acc") )

# labeled
cor(d$labeled_acc,d$log_joint)
print(ggplot(d, aes(x=log_joint,y=labeled_acc,color=annealing_schedule)) + geom_point(shape=1) + 
        facet_grid(~k~accuracy_level)+ ggtitle("labeled_acc") )

#########################################################
#            Production: ICML 2014 Newsgroups correlation plots
#########################################################

corplot <- function(d, xlabel=NULL, ylabel=NULL){
  plt <- ggplot(d, aes(x=log_joint,y=heldout_acc))
  plt <- plt + geom_point(shape=1)
  plt <- plt + theme(legend.position="none"
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          #panel.grid=element_blank()
        ) 
  
  # xlabel
  if (!is.null(xlabel)){
    plt <- plt + xlab(xlabel)
  }
  else{
    plt <- plt + theme(axis.title.x=element_blank())
  }
  plt <- plt + scale_x_continuous(label=function(x){
    n <- format(x/1e7,scientific=FALSE,digits=4)
    gsub('e\\+0','e',n)
    })
  
  # ylabel 
  if (!is.null(ylabel)){
    plt <- plt + ylab(ylabel)
  }
  else{
    plt <- plt + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
  }
  plt <- plt + scale_y_continuous(limits=c(0,1),breaks=c(0,1),labels=c('0','1'))
  #plt <- plt + ylim(0,1)
  
  print(plt)
}

width <- 8
height <- 7

# newsgroups-correlation-1
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which(d$pre_normalize_documents==-1),]  # no doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$annealing_schedule!='1'),] # annealing
corplot(d,ylab="Accuracy")
ggsave(file="newsgroups-correlation-1.eps",width=width,height=height,units='cm')

# newsgroups-correlation-2
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which(d$pre_normalize_documents==123),] # doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$annealing_schedule!='1'),] # annealing
corplot(d)
ggsave(file="newsgroups-correlation-2.eps",width=width,height=height,units='cm')

# newsgroups-correlation-3
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which(d$pre_normalize_documents==-1),]  # no doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$samples=='0'),] # no sampling
corplot(d,ylab="Accuracy",xlab="log Model Probability")
ggsave(file="newsgroups-correlation-3.eps",width=width,height=height,units='cm')

# newsgroups-correlation-4
d = mdata[which(mdata$corpus=="NEWSGROUPS"),]
d = d[which(d$pre_normalize_documents==123),] # doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$samples=='0'),] # no sampling
corplot(d,xlab="log Model Probability")
ggsave(file="newsgroups-correlation-4.eps",width=width,height=height,units='cm')


#######################################################
# Supplementary material ICML 2014
#######################################################
width <- 14
height <- 14

# enron-correlation-1
d = mdata[which(mdata$corpus=="ENRON"),]
d = d[which(d$pre_normalize_documents==-1),]  # no doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$annealing_schedule!='1'),] # annealing
corplot(d,ylab="Accuracy")
ggsave(file="enron-correlation-1.eps",width=width,height=height,units='cm')

# enron-correlation-2
d = mdata[which(mdata$corpus=="ENRON"),]
d = d[which(d$pre_normalize_documents==123),] # doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$annealing_schedule!='1'),] # annealing
corplot(d)
ggsave(file="enron-correlation-2.eps",width=width,height=height,units='cm')

# enron-correlation-3
d = mdata[which(mdata$corpus=="ENRON"),]
d = d[which(d$pre_normalize_documents==-1),]  # no doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$samples=='0'),] # no sampling
corplot(d,ylab="Accuracy",xlab="log Model Probability")
ggsave(file="enron-correlation-3.eps",width=width,height=height,units='cm')

# enron-correlation-4
d = mdata[which(mdata$corpus=="ENRON"),]
d = d[which(d$pre_normalize_documents==123),] # doc scaling
d = d[which(d$num_annotations==0),] # eval_point=0
d = d[which(d$samples=='0'),] # no sampling
corplot(d,xlab="log Model Probability")
ggsave(file="enron-correlation-4.eps",width=width,height=height,units='cm')
