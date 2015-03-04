

massageData <- function(d){
  d$annotator <- factor(d$annotator)
  d$annotation <- factor(d$annotation)
  d$instance_id <- factor(d$instance_id)
  d
}

sortByHistogram <- function(d,y){
  ## set the levels in order we want
  d[[y]] <- factor(d[[y]], levels=names( sort(table(d[[y]]),decreasing=TRUE) ) ) 
  d
}

plotAccuracyOverTime <- function(d, annotator){
  ann1 <- d[which(d$annotator==annotator,),]
  x = seq(1:dim(ann1)[1])
#   x = ann1$start
  
  y = ann1$num_correct_annotations
  plot(y=y, x=x, main=sprintf("Annotator accuracy over time  id=%s ",annotator))
  lines(predict(loess(y~x), x=seq(min(x):max(x),by=) ) )
}


require(ggplot2)
require(MASS)
require(poweRlaw)
setwd('/aml/home/plf1/git/Experiments/plf1/cslda-paper/crowdflower-analysis')
stop()

###########################################################################
#d <- read.csv("cfsimplegroups1000a-annotations.csv")
d <- read.csv("cfgroups1000-annotations.csv")
d <- massageData(d)

# #annotators per cumulative #annotations
qplot(d$cum_num_annotations,d$cum_num_annotators,main="Annotator Arrival")

d <- sortByHistogram(d,"annotator")
qplot(d$annotator,main="Annotator Productivity (Annotations per Annotator)")
hh<- hist(as.integer(d$annotator),breaks=seq(0,136,by=1))
# fit powerlaw (http://cran.r-project.org/web/packages/poweRlaw/vignettes/examples.pdf and referenced paper)
pl <- displ(hh$counts)
pl$setXmin(estimate_xmin(pl))
ln <- dislnorm$new(hh$counts)
ln$setXmin(estimate_xmin(ln))
pois <- dispois$new(hh$counts)
pois$setXmin(estimate_xmin(pois))
plot(ln)
lines(ln, col=2)
lines(pl, col=3)
lines(pois, col=4)
print(pl)
f <- function(x){((pl$pars-1)/pl$xmin)*(x/pl$xmin)^-pl$pars}
x <- seq(pl$xmin,2000)
qplot(x,1*f(x))

# Accuracy per Annotator
annacc <- by(d$num_correct_annotations,d$annotator,sum)/by(d$num_annotations,d$annotator,sum)
accdf <- data.frame(acc=c(annacc)) # create data frame for ggplot
plot(annacc, main="Annotator Accuracy Scatter Plot")
#plot(density(annacc),main="Annotator Accuracy")
hist(annacc,main="Annotator Accuracy",xlim=c(0,1),xlab="Annotator Accuracy")
#m <- ggplot(accdf, aes(x=acc))
#m + geom_histogram(binwidth=0.05) + geom_density()
# Overlay Beta distribution with MLE-fit
loglik <- function(alpha,x){sum(-dbeta(x,alpha[1],alpha[2],log=TRUE))}
annacc_beta_fit <- fitdistr(x=annacc[which(annacc<1)],densfun="beta",start=list(shape1=1,shape2=1)) 
x <- seq(0,1,by=0.01)
lines(x,15*dbeta(x,annacc_beta_fit$estimate[1],annacc_beta_fit$estimate[2]))

# Average Annotator Accuracy
print(c("average annotator accuracy: ",mean(annacc)))

# Accuracy for Annotator over time
plotAccuracyOverTime(d,levels(d$annotator)[1])
plotAccuracyOverTime(d,levels(d$annotator)[2])
plotAccuracyOverTime(d,levels(d$annotator)[3])
plotAccuracyOverTime(d,levels(d$annotator)[4])
plotAccuracyOverTime(d,levels(d$annotator)[5])
plotAccuracyOverTime(d,levels(d$annotator)[6])
plotAccuracyOverTime(d,levels(d$annotator)[7])
plotAccuracyOverTime(d,levels(d$annotator)[8])
plotAccuracyOverTime(d,levels(d$annotator)[9])
plotAccuracyOverTime(d,levels(d$annotator)[10])
plotAccuracyOverTime(d,levels(d$annotator)[11])
plotAccuracyOverTime(d,levels(d$annotator)[12])

# Accuracy per Instance
annacc <- by(d$num_correct_annotations,d$instance_id,sum)/by(d$num_annotations,d$instance_id,sum)
plot(annacc, main="Accuracy per Instance")
plot(density(annacc))


d <- read.csv("cfgroups1000-instances.csv")
d <- d[which(d$num_annotations>0),]
# annotations per instance
hist(d$num_annotations)
plot(density(d$num_annotations))
print(c("average number of annotations per instance",mean(d$num_annotations)))
median(d$num_annotations)
hist(d$num_correct_annotations/d$num_annotations)


# Annotations per Annotator
#d <- read.csv("cfgroups1000-annotators.csv")




#####################################################
# ACL 2015 (saved plots manually via rstudio's export feature)
#####################################################
d <- read.csv("cfgroups1000-annotations.csv")
d <- massageData(d)

# Accuracy per Annotator
annacc <- by(d$num_correct_annotations,d$annotator,sum)/by(d$num_annotations,d$annotator,sum)
accdf <- data.frame(acc=c(annacc)) # create data frame for ggplot
plot(annacc, main="Annotator Accuracy Scatter Plot")
#plot(density(annacc),main="Annotator Accuracy")
hist(annacc,main="Histogram",xlim=c(0,1),xlab="Annotator Accuracy")
#m <- ggplot(accdf, aes(x=acc))
#m + geom_histogram(binwidth=0.05) + geom_density()
# Overlay Beta distribution with MLE-fit
loglik <- function(alpha,x){sum(-dbeta(x,alpha[1],alpha[2],log=TRUE))}
annacc_beta_fit <- fitdistr(x=annacc[which(annacc<1)],densfun="beta",start=list(shape1=1,shape2=1)) 
x <- seq(0,1,by=0.01)
lines(x,15*dbeta(x,annacc_beta_fit$estimate[1],annacc_beta_fit$estimate[2]))
