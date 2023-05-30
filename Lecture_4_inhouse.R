library(ggplot2)  
library(gridExtra)

setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

#10.a Backward Elimination
salary=read.csv("EXECSAL2.csv", header = TRUE)
head(salary)

full_model<-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9)+X10, data = salary)

##Determine a threshold named p_th, we remove predictors which p>p_th, e.g. p_th=0.3

###Step1: Start with the full model 
current_optimal <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9)+X10, data = salary)

###Step2: remove predictor with largest p
current_optimal_coefficients <- data.frame(summary(current_optimal)$coefficients)
current_optimal_coefficients_sorted <- current_optimal_coefficients[order(current_optimal_coefficients$Pr...t..,decreasing = TRUE), ]
current_optimal_coefficients_sorted[1,]
mode_test <-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9), data = salary)  #testing X10
anova(mode_test, current_optimal)  ##p=0.72. p>p_th OK,remove X10

###Step2 repeat1
current_optimal <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9), data = salary)
current_optimal_coefficients <- data.frame(summary(current_optimal)$coefficients)
current_optimal_coefficients_sorted <- current_optimal_coefficients[order(current_optimal_coefficients$Pr...t..,decreasing = TRUE), ]
current_optimal_coefficients_sorted[1,]
mode_test <-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X8+factor(X9), data = salary)  #testing X7
anova(mode_test, current_optimal)  ##p=0.7683, p>p_th, remove X7

###Step2 repeat2
current_optimal <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X8+factor(X9), data = salary)
current_optimal_coefficients <- data.frame(summary(current_optimal)$coefficients)
current_optimal_coefficients_sorted <- current_optimal_coefficients[order(current_optimal_coefficients$Pr...t..,decreasing = TRUE), ]
current_optimal_coefficients_sorted[1,]
mode_test <-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+factor(X9), data = salary) #testing X8
anova(mode_test, current_optimal)  ##p=0.6117, p>p_th, remove X8

###Step2 repeat3
current_optimal <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+factor(X9), data = salary)
current_optimal_coefficients <- data.frame(summary(current_optimal)$coefficients)
current_optimal_coefficients_sorted <- current_optimal_coefficients[order(current_optimal_coefficients$Pr...t..,decreasing = TRUE), ]
current_optimal_coefficients_sorted[1,]
mode_test <-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X9), data = salary) #testing factor(X6)
anova(mode_test, current_optimal)  ##p=0.4444, p>p_th, remove factor(X6)

###Step2 repeat4
current_optimal <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X9), data = salary)
current_optimal_coefficients <- data.frame(summary(current_optimal)$coefficients)
current_optimal_coefficients_sorted <- current_optimal_coefficients[order(current_optimal_coefficients$Pr...t..,decreasing = TRUE), ]
current_optimal_coefficients_sorted[1,] 
mode_test <-lm(Y~X1+X2+factor(X3)+X4+X5, data = salary) #testing factor(X9)
anova(mode_test, current_optimal)  ##p=0.2011, p<p_th, keep factor(X9)


#10.b. Forward selection 
###Step1: Start with the null model
null_model <- lm(Y~1, data = salary)

###Step2: Add predictor with smallest p
F1_model1<-lm(Y~X1, data = salary)  ###most sig
summary(F1_model1)$coefficients

F1_model2<-lm(Y~X2, data = salary)
summary(F1_model2)$coefficients

F1_model3<-lm(Y~factor(X3), data = salary)
summary(F1_model3)$coefficients

F1_model4<-lm(Y~X4, data = salary)
summary(F1_model4)$coefficients

F1_model5<-lm(Y~X5, data = salary)
summary(F1_model5)$coefficients

F1_model6<-lm(Y~factor(X6), data = salary)
summary(F1_model6)$coefficients

F1_model7<-lm(Y~X7, data = salary)
summary(F1_model7)$coefficients

F1_model8<-lm(Y~X8, data = salary)
summary(F1_model8)$coefficients

F1_model9<-lm(Y~factor(X9), data = salary)
summary(F1_model9)$coefficients

F1_model10<-lm(Y~X10, data = salary)
summary(F1_model10)$coefficients

#X1 is the predictor with smallest pvalue

##Step2 repeat1
f2_model1 <- lm(Y ~ X1+X2, data=salary)
summary(f2_model1)$coefficients

f2_model2 <- lm(Y ~ X1+factor(X3), data=salary)  ###most sig
summary(f2_model2)$coefficients

f2_model3 <- lm(Y ~ X1+X4, data=salary)
summary(f2_model3)$coefficients

f2_model4 <- lm(Y ~ X1+X5, data=salary)
summary(f2_model4)$coefficients

f2_model5 <- lm(Y ~ X1+factor(X6), data=salary)
summary(f2_model5)$coefficients

f2_model6 <- lm(Y ~ X1+X7, data=salary)
summary(f2_model6)$coefficients

f2_model7 <- lm(Y ~ X1+X8, data=salary)
summary(f2_model7)$coefficients

f2_model8 <- lm(Y ~ X1+factor(X9), data=salary)
summary(f2_model8)$coefficients

f2_model9 <- lm(Y ~ X1+X10, data=salary)
summary(f2_model9)$coefficients

##Step2 repeat3
### Too much work~~! I quit!


#10.c. Stepwise regression 
#Same two steps as forward selection, but keep an eye on predictors before adding the new predictor.


###10.Bonus Simple way! Save your day!
library(olsrr) #need to install the package olsrr
salary=read.csv("EXECSAL2.csv", header = TRUE)
fullmodel<-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9)+X10, data = salary)

#p value; variables with p larger than prem will be removed from the model.
backmodel=ols_step_backward_p(fullmodel, prem = 0.3, details=TRUE) 
#p value; variables with p smaller than penter will be added from the model.
forwardmodel=ols_step_forward_p(fullmodel, penter = 0.3, details=TRUE) 
stepwisemodel=ols_step_both_p(fullmodel, penter = 0.3, details=TRUE) 


###In class practice problem 11
credit=read.csv("credit.csv",header = TRUE)
full_model=lm(Balance ~ Income+Limit+Rating+Cards+Age+Education+factor(Gender)
              +factor(Ethnicity)+factor(Married)+factor(Student), data=credit)
backward_model = ols_step_backward_p(full_model, prem=0.3) #details=TRUE
forward_model = ols_step_forward_p(full_model,penter=0.3)

###In class practice problem 12
stepwise_model = ols_step_both_p(full_model,penter=0.3)


###Construct our criteria combos 5 
library(olsrr)
salary=read.csv("EXECSAL2.csv", header = TRUE)
fullmodel<-lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9)+X10, data = salary)

backmodel=ols_step_backward_p(fullmodel, prem = 0.3, details=TRUE) 
forwardmodel=ols_step_forward_p(fullmodel, penter = 0.3, details=TRUE) 
stepwisemodel=ols_step_both_p(fullmodel, penter = 0.3, details=TRUE) 
par(mfrow=c(3,2)) # split the plotting panel into a 2 x 2 grid
plot(stepwisemodel$rsq,type = "o",pch=10, xlab="Number of Variables",ylab= "Rˆ2")
plot(stepwisemodel$adjr,type = "o",pch=10, xlab="Number of Variables",ylab= "Adjusted Rˆ2")
plot(stepwisemodel$mallows_cp,type = "o",pch=10, xlab="Number of Variables",ylab= "Cp")
plot(stepwisemodel$aic,type = "o",pch=10, xlab="Number of Variables",ylab= "AIC")
plot(stepwisemodel$rmse,type = "o",pch=10, xlab="Number of Variables",ylab= "RMSE")

backward_combos5=cbind("backward",backmodel$indvar, backmodel$rsquare, 
                       backmodel$adjr, backmodel$mallows_cp, 
                       backmodel$aic, backmodel$rmse)

forward_combos5=cbind("forward",forwardmodel$indvar, forwardmodel$rsquare, 
                      forwardmodel$adjr, forwardmodel$mallows_cp, 
                      forwardmodel$aic, forwardmodel$rmse)
stepwise_combos5=cbind("stepwise",forwardmodel$indvar, stepwisemodel$rsquare, 
                       stepwisemodel$adjr, stepwisemodel$mallows_cp, 
                       stepwisemodel$aic, stepwisemodel$rmse)
combos5=data.frame(rbind(backward_combos5, forward_combos5, stepwise_combos5))
combos5


###In class practice problem 13
credit=read.csv("credit.csv",header = TRUE)
full_model=lm(Balance ~ Income+Limit+Rating+Cards+Age+Education+factor(Gender)
              +factor(Ethnicity)+factor(Married)+factor(Student), data=credit)

forward_model = ols_step_forward_p(full_model,penter=0.3)
stepwise_model = ols_step_both_p(full_model,penter=0.3)

forward_combos5=cbind("forward",forwardmodel$indvar, 
                      forwardmodel$rsquare, forwardmodel$adjr, 
                      forwardmodel$mallows_cp, forwardmodel$aic, forwardmodel$rmse)
stepwise_combos5=cbind("stepwise",stepwisemodel$indvar, 
                       stepwisemodel$rsquare, stepwisemodel$adjr, 
                       stepwisemodel$mallows_cp, stepwisemodel$aic, stepwisemodel$rmse)
combos5=data.frame(rbind(forward_combos5, stepwise_combos5))
colnames(combos5)=c("model","Variables", "R2","AdjR2","Cp","AIC","RMSE")
combos5
write.csv(combos5,"Lecture4_credit_combos5.csv",row.names=FALSE)
