library(ggplot2)

ages<-read.csv('longevity.csv')
ggplot(ages, aes(x=AgeAtDeath, fill=factor(Smokes)))+geom_density()+facet_grid(Smokes~.)

guess<-73
with(ages, mean((AgeAtDeath-guess)^2))

guess.accuracy<-data.frame()

for (guess in seq(63, 83, by=1)){
    prediction.error<-mean((ages$AgeAtDeath-guess)^2)
    guess.accuracy<-rbind(guess.accuracy, data.frame(Guess=guess, Error=prediction.error))
}

ggplot(guess.accuracy, aes(x=Guess, y=Error))+geom_point()+geom_line()

constant.guess<-mean(ages$AgeAtDeath)
with(ages, sqrt(mean((AgeAtDeath-constant.guess)^2)))

smokers.guess<-with(subset(ages, Smokes==1), mean(AgeAtDeath))
non.smokers.guess<-with(subset(ages, Smokes==0), mean(AgeAtDeath))
ages$NewPrediction<-ifelse(ages$Smokes==0, non.smokers.guess, smokers.guess)
with(ages, sqrt(mean((AgeAtDeath-NewPrediction)^2)))

hw<-read.csv('01_heights_weights_genders.csv', header=TRUE, sep=',')
ggplot(hw, aes(x=Height, y=Weight)) + geom_point() + geom_smooth(method='lm')

fitr<-lm(Weight~Height, data=hw)
coef(fitr)
intercept<-coef(fitr)[1]
slope<-coef(fitr)[2]

#residual plot
plot(fitr, which=1)

x<-1:10
y<-x^2
fit<-lm(y~x)
plot(fit, which=1)
errors<-residuals(fit)
squared.errors<-errors^2
#mean squared error
mean(squared.errors)
#root mean squared error
rmse_mod<-sqrt(mean(squared.errors))
mean_y<-mean(y)
rmse_mean<-sqrt(mean((y-mean_y)^2))
r2<-1-rmse_mod/rmse_mean
r2
ssres<-sum(residuals(fit)^2)
sstot<-sum((y-mean_y)^2)
1- ssres/sstot

top<-read.csv('top_1000_sites.tsv', sep='\t', stringsAsFactors=FALSE)
ggplot(top, aes(x=PageViews, y=UniqueVisitors)) + geom_point()

top$vper<-top$PageViews/top$UniqueVisitors
topsort<-top[order(top$vper, decreasing=TRUE), ]
head(topsort, n=20)

ggplot(top, aes(x=PageViews)) + geom_density()
ggplot(top, aes(x=log(PageViews))) + geom_density()
ggplot(top, aes(x=log(PageViews), y=log(UniqueVisitors)))+geom_point()+geom_smooth(method='lm', se=FALSE)

lmfit<-lm(log(PageViews)~log(UniqueVisitors), data=top)
summary(lmfit)
plot(lmfit, which=1)
#model rmse
sqrt(mean(residuals(lmfit)^2))

lmf2<-lm(log(PageViews)~HasAdvertising+log(UniqueVisitors)+InEnglish, data=top)
summary(lmf2)