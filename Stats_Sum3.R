###Lecture 4
library(ggplot2)
Advertising=read.table("Advertising.txt", header = TRUE,  sep ="\t" )
model<-lm(sale~tv+radio+tv*radio, data=Advertising)
summary(model)
ggplot(model, aes(x=.fitted, y=.resid)) + geom_point() + geom_smooth()+ geom_hline(yintercept = 0) 

#optional graph from plot()
plot(fitted(model), residuals(model),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("Residual vs Fitted")

library(ggplot2)
library(GGally)
Advertising=read.table("Advertising.txt", header = TRUE,sep ="\t" )
ggpairs(Advertising,lower = list(continuous = "smooth_loess", combo = "facethist", discrete = "facetbar", na = "na"))

###Problem 1: 
library(GGally)
waterdata=read.csv("E:/PHD/Courses/Data 603/Datasets/Assignment1/water.csv",header = TRUE )
Advertising=read.table(filename,header = TRUE,  sep ="\t")
head(waterdata,5)

model=lm(USAGE~PROD+TEMP+HOUR+DAYS, data=waterdata)
summary(model)

intermodel=lm(USAGE~(PROD+TEMP+HOUR)^2, data=waterdata)
summary(intermodel)

finalintermodel=lm(USAGE~PROD+TEMP+HOUR+PROD*TEMP+PROD*HOUR, data=waterdata)
summary(finalintermodel)

ggpairs(waterdata,lower = list(continuous = "smooth_loess", combo ="facethist", discrete = "facetbar", na = "na"))

modelnointer<-lm(USAGE~PROD+TEMP+HOUR,data=waterdata)
library(car)
vif(modelnointer) 
#The VIF of a predictor is a measure for how easily it is predicted from a linear regression using the other predictors.
#A general guideline is that a VIF larger than 5 or 10 is large, indicating that the model has problems estimating the coefficient. 


library(lmtest)
#Adding higher order terms into the model
finalintermodel=lm(USAGE~PROD+TEMP+HOUR+PROD*TEMP+PROD*HOUR, data=waterdata)
#Heteroscedasticity test
#H0:heteroscedasticity is not present, constant variance
#Ha:heteroscedasticity is present, not constant variance
bptest(finalintermodel)

par(mfrow=c(2,2))
plot(finalintermodel)

#Testing for Normality
#Ho:the residuals are significantly normally distributed
#HA:the residuals are not significantly normally distributed

shapiro.test(residuals(finalintermodel))

#Histogram of residuals
hist(residuals(finalintermodel))
plot(finalintermodel,which=c(2))

#Plot the residuals vs predicted value Y^ plot
#From the output, by checking the regression residuals for evidence of patterns or trends, we found that the residual plot show no discernible pattern or trend at all. 
plot(finalintermodel,which=c(1))

#Do you detect any outliers by using Cook’s distance measure (using cooks.distance()>1 ) and Residual vs Leverage plot?
waterdata[cooks.distance(finalintermodel)>1,]  #rows presented in waterdata

#Cook's distance
plot(finalintermodel,pch=18,col="red",which=c(4))

#Residual VS Leverage
plot(finalintermodel,which=c(5))

##From the output, we did not detect any outliers by using both Cook’s distance function and plot. The highest Cook’s distance value was 0.30(163rd observation) which is still lower than 1. Likewise the Residual vs Leverage plot, no potential outliers were detected.



###Problem 2 
#####a: Check normality, homoscedasticity, and linearity assumptions.
model=lm(BURDEN~(CGDUR+ MEM +SOCIALSU) , data=KBI)
summary(model) 
par(mfrow=c(2,2))
plot(model)
bptest(model) #Testing for Homoscedasticity
shapiro.test(residuals(model)) #Testing for Normality

#####b: Do you detect any outliers by using leverage values greater that 3p/n? If yes, create a new dataset that removes these outliers.
modfinal=lm(BURDEN~CGDUR+ MEM +SOCIALSU , data=KBI)
##leverage points
lev=hatvalues(modfinal)
p = length(coef(modfinal))
n = nrow(KBI)
outlier = lev[lev>(3*p/n)]
print(outlier)

plot(rownames(KBI),lev, main ="Leverage in Advertising Dataset", xlab="observation",ylab ="Leverage Value")
abline(h =3*p/n, lty =1)

KBI2=KBI[lev<=(3*p/n),]
head(KBI2)

#####c. Fit the model
modfinal1=lm(BURDEN~CGDUR+ MEM +SOCIALSU , data=KBI)
summary(modfinal1)

modfinal2=lm(BURDEN~CGDUR+ MEM +SOCIALSU , data=KBI2)
summary(modfinal2)

#After removing those 2 points, we fitted again the model and variable CGDUR now has a smaller p-value of 0.0349, and then becomes significant with α=0.05
#This model has the largest R2adjusted (=0.43) and smaller RMSE. MOreover, all independent variables have the p-values less than 0.05.


###Problem 3
#####a: Check normality, homoscedasticity, and linearity assumptions.
model=lm(y~x,data=)
par(mfrow=c(2,2))
plot(model)

#check normality using QQ plot or shapiro.test()
#check homoscedasticity using Residual VS Fitted values plots or bptest()
#linearity can also be checked using Residual VS Fitted values plot
#If normality test and homoscedasticity test do not hold, apply log-transformation/Box-Cox/ or use weighted least squared regression as alternatives. 

library(Ecdat)#for dataset University
reg=lm(nassets~stfees,data=University)
summary(reg)

library(MASS) #for the boxcox() function
bc=boxcox(reg,lambda=seq(-1,1))
bestlambda=bc$x[which(bc$y==max(bc$y))]
bestlambda

