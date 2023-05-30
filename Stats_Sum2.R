###Lecture 3
#####Option 1
 
library(olsrr)
salary=read.csv("c:/Users/thunt/OneDrive - University of Calgary/dataset603/EXECSAL2.csv", header = TRUE)
firstordermodel<-lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data= salary)
#Select the subset of predictors that do the best at meeting some well-defined objective criterion, such as having the largest R2 value or the smallest MSE, Mallow's Cp or AIC.
'''ols_step_best_subset: Best subsets regression, 
select the subset of predictors that do the best at meeting some well-defined objective criterion, such as having the largest adjR2 value or the smallest MSE, Mallow’s Cp or AIC.
BIC values are not provided'''
ks=ols_step_best_subset(firstordermodel, details=TRUE)
# for the output interpretation
rsquare<-c(ks$rsquare)
AdjustedR<-c(ks$adjr)
cp<-c(ks$cp)
AIC<-c(ks$aic)
cbind(rsquare,AdjustedR,cp,AIC)

par(mfrow=c(2,2)) # split the plotting panel into a 2 x 2 grid
plot(ks$cp,type = "o",pch=10, xlab="Number of Variables",ylab= "Cp")
plot(ks$rsq,type = "o",pch=10, xlab="Number of Variables",ylab= "R^2")

plot(ks$aic,type = "o",pch=10, xlab="Number of Variables",ylab= "AIC")
plot(ks$adjr,type = "o",pch=10, xlab="Number of Variables",ylab= "Adjusted R^2")


#####Option 2: 
library(olsrr) 
firstordermodel<-lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data= salary) 
library(leaps) #need to install the package leaps for regsubsets() function

best.subset<-regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data= salary, nv=10 ) 
#by default, regsubsets() only reports results up to the best 8-variable model 
#Model selection by exhaustive search, forward or backward stepwise, or sequential replacement 
#The summary() command outputs the best set of variables for each model size using RMSE. 
summary(best.subset)

reg.summary<-summary(best.subset) 
# for the output interpretation 
rsquare<-c(reg.summary$rsq) 
cp<-c(reg.summary$cp) 
AdjustedR<-c(reg.summary$adjr2) 
RMSE<-c(reg.summary$rss) 
BIC<-c(reg.summary$bic) 
cbind(rsquare,cp,BIC,RMSE,AdjustedR)


###Evaluate the reliablity of model chosen (involve interaction terms)
#considering high order model between Xs and Y to improve the model
library(GGally)
salarydata <-data.frame(salary$Y,salary$X1,salary$X2,salary$X3,salary$X4,salary$X5)
#ggpairs(salarydata)
#LOESS or LOWESS: LOcally WEighted Scatter-plot Smoother
#ggpairs(salarydata,lower = list(continuous = "smooth_loess", combo =
# "facethist", discrete = "facetbar", na = "na"))
#option2: using function pairs()
#pairs(~Y+X1+X2+factor(X3)+X4+X5,data=salary,panel = panel.smooth) 

bestmodel<-lm(Y~X1+I(X1^2)+X2+factor(X3)+X4+X5+factor(X3)*X4,data=salary)
summary(bestmodel)

bestmodel1<-lm(Y~X1+I(X1^2)+I(X1^3)+X2+factor(X3)+X4+X5+factor(X3)*X4,data=salary)
summary(bestmodel1)


###Problem1
tires=read.csv("E:/PHD/Courses/Data 603/Datasets/Assignment2/tires.csv", header = TRUE)
str(tires)

additivemodel<-lm(wear~factor(type)+ave,data=tires)
summary(additivemodel)

library(mosaic)
fav_stats(tires$ave)

interacmodel<-lm(wear~factor(type)+ave+factor(type)*ave,data=tires)
newdata = data.frame(type="A", ave=100)
predict(interacmodel,newdata,interval="predict")

###Problem2
library(ggplot2)
Healthdata=read.csv("E:/PHD/Courses/Data 603/Datasets/Assignment2/MentalHealth.CSV", header = TRUE)
head(Healthdata,5)
ggplot(data=Healthdata,mapping=aes(x=AGE,y=EFFECT,colour=METHOD))+geom_point()
intermodel=lm(EFFECT~AGE+factor(METHOD)+AGE*factor(METHOD),data = Healthdata)
summary(intermodel)
ggplot(data=Healthdata,mapping= aes(x=AGE,y=EFFECT,colour=METHOD))+geom_point()+geom_smooth(method='lm')

###Problem 3. Collusive bidding in road construction.
library(olsrr)
flag=read.table("E:/PHD/Courses/Data 603/Datasets/Assignment2/FLAG2.txt", header = TRUE)
str(flag)

fullmodel<-lm(LOWBID~DOTEST+factor(STATUS)+factor(DISTRICT)+NUMIDS+DAYSEST+RDLNGTH+PCTASPH+PCTBASE+PCTEXCAV+PCTMOBIL+PCTSTRUC+PCTTRAF , data =flag)
summary(fullmodel)
step=olsrr::ols_step_both_p(fullmodel, pent =0.05, prem =0.1, details=FALSE)
# final model
summary(step$model)

forw=ols_step_forward_p(fullmodel,pent=0.05, details=FALSE)
# final model
summary(forw$model)

backw=ols_step_backward_p(fullmodel, prem =0.05, details=FALSE)
# final model
summary(backw$model)