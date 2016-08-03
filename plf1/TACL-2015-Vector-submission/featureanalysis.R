
setwd('/aml/home/plf1/git/Experiments/plf1/TACL-2015-Vector-submission/csv')

d = read.csv('2015-08-25-featureanalysis.csv')
m <- colMeans(d)
v <- cov(d) 
r <- cor(d)

# boxplots
d = read.csv('2015-08-25-featureanalysis.csv')
# prune columns with abs(mean) < K
drops <- c(colnames(d)[which(abs(m)<1.39)],"X153") # remove bias
d <- d[,!(names(d) %in% drops)]
print(length(colnames(d)))
boxplot(d)
p<-boxplot(d,names=c("COS","V1-4","V1-11","V1-30","V1-32","V2-4","V2-19","V2-28","V2-30","V2-32"))
