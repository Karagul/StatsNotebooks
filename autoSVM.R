library(ISLR)
medmpg<-median(mpg)
attach(Auto)
Auto$highmpg<-ifelse(mpg>medmpg,1,0)

set.seed(1)
tune.out<-tune(svm,highmpg~.-mpg,data=Auto,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod<-tune.out$best.model
summary(bestmod)

set.seed(1)
tune.out<-tune(svm,highmpg~.-mpg,data=Auto,kernel="radial",ranges=list(cost=c(0.1,1,10,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)

set.seed(1)
tune.out<-tune(svm,highmpg~.-mpg,data=Auto,kernel="polynomial",ranges=list(cost=c(0.1,1,10,1000),degree=c(2,3,4,5)))
summary(tune.out)