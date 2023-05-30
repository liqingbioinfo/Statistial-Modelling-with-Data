library(ggplot2)
setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

###In class Practice Problem 9+
quality = read.csv("PRODQUAL.csv",header = TRUE)
head(quality)
dim(quality)  #76, 2 
ggplot(data=quality)+aes(x=PRESSURE, y=QUALITY)+geom_point(color='red')+geom_smooth()

model1=lm(formula = QUALITY ~ PRESSURE, data=quality)
summary(model1)

model2=lm(formula = QUALITY ~ PRESSURE + I(PRESSURE^2), data=quality)
summary(model2)

model3=lm(formula = QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3), data=quality)
summary(model3)

model4=lm(formula = QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4), data=quality)
summary(model4)

model5=lm(formula = QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4)+I(PRESSURE^5), data=quality)
summary(model5)

###Cross validaiton for above models
library(DAAG)
mk=10
out1<- CVlm(data=quality, form.lm=formula(QUALITY ~ PRESSURE), m=mk)  #folder1: 7 test; Remain 76-7 = 69 training data

out2<- CVlm(data=quality, form.lm=formula(QUALITY ~ PRESSURE + I(PRESSURE^2)), m=mk)

out3<- CVlm(data=quality, form.lm=formula(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)), m=mk)

out4<- CVlm(data=quality, form.lm=formula(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4)), m=mk)

out5<- CVlm(data=quality, form.lm=formula(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4)+I(PRESSURE^5)), m=mk)

cv_error1<-mean((out1$cvpred-out1$QUALITY)^2)
cv_error2<-mean((out2$cvpred-out2$QUALITY)^2)
cv_error3<-mean((out3$cvpred-out3$QUALITY)^2)
cv_error4<-mean((out4$cvpred-out4$QUALITY)^2)
cv_error5<-mean((out5$cvpred-out5$QUALITY)^2)

print(paste(cv_error1, cv_error2, cv_error3, cv_error4, cv_error5, sep = "   "))

###One more example for practicing over fitting
overfitting_example = read.csv("Overfitting.csv",header = TRUE)
dim(overfitting_example)

###In class practice problem 10
salary<-read.csv("EXECSAL2.csv",header=TRUE)
head(salary)
dim(salary) #100, 12

###Backward Elimination
###STEP 1:
full_model = lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9)+X10, data=salary)

###sTEP 2:
coefficients_model <- data.frame(summary(full_model)$coefficients)
coefficients_model_order <- coefficients_model[order(coefficients_model$Pr...t..,decreasing = TRUE),]
coefficients_model_order[1,] #predictor with largest p-value, remove this predictor
model_removed_X10 <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9), data=salary)

###Step 2, repeat 1
coefficients_model <- data.frame(summary(model_removed_X10)$coefficients)
coefficients_model_order <- coefficients_model[order(coefficients_model$Pr...t..,decreasing = TRUE),]
coefficients_model_order[1,] #predictor with largest p-value, remove this predictor
model_removed_X10_X7 <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X8+factor(X9), data=salary)

###Step 2, repeat 2
coefficients_model <- data.frame(summary(model_removed_X10_X7)$coefficients)
coefficients_model_order <- coefficients_model[order(coefficients_model$Pr...t..,decreasing = TRUE),]
coefficients_model_order[1,] #predictor with largest p-value, remove this predictor
model_removed_X10_X7_X8 <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+factor(X9), data=salary)

###Step 2, repeat 3
coefficients_model <- data.frame(summary(model_removed_X10_X7)$coefficients)
coefficients_model_order <- coefficients_model[order(coefficients_model$Pr...t..,decreasing = TRUE),]
coefficients_model_order[1,] #predictor with largest p-value, remove this predictor
model_removed_X10_X7_X8 <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+factor(X9), data=salary)

###Step 2, repeat 4
coefficients_model <- data.frame(summary(model_removed_X10_X7_X8)$coefficients)
coefficients_model_order <- coefficients_model[order(coefficients_model$Pr...t..,decreasing = TRUE),]
coefficients_model_order[1,] #predictor with largest p-value, remove this predictor
model_removed_X10_X7_X8_X6 <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X9), data=salary)

###Step 2, repeat 5
coefficients_model <- data.frame(summary(model_removed_X10_X7_X8_X6)$coefficients)
coefficients_model_order <- coefficients_model[order(coefficients_model$Pr...t..,decreasing = TRUE),]
coefficients_model_order[1,] #predictor with largest p-value, remove this predictor 
#X9 pvalue is 0.2 < p_th, therefore we stop back elimination.

final_model=model_removed_X10_X7_X8_X6
summary(final_model)



###Forward selection 
###Start with the null model
null_model = lm(Y~1, data=salary)

###Step2: adding one predictor with smallest p-value    ---> X1
F1_model1 <- lm(Y~X1, data=salary)
summary(F1_model1)$coefficients

F1_model2 <- lm(Y~X2, data=salary)
summary(F1_model2)$coefficients

F1_model3 <- lm(Y~factor(X3), data=salary)
summary(F1_model3)$coefficients

F1_model4 <- lm(Y~X4, data=salary)
summary(F1_model4)$coefficients

F1_model5 <- lm(Y~X5, data=salary)
summary(F1_model5)$coefficients

F1_model6 <- lm(Y~factor(X6), data=salary)
summary(F1_model6)$coefficients

F1_model7 <- lm(Y~X7, data=salary)
summary(F1_model7)$coefficients

F1_model8 <- lm(Y~X8, data=salary)
summary(F1_model8)$coefficients

F1_model9 <- lm(Y~factor(X9), data=salary)
summary(F1_model9)$coefficients

F1_model10 <- lm(Y~X10, data=salary)
summary(F1_model10)$coefficients

##opimal model after conduting step 2 for Forward selection is
F1_model_final = lm(Y~X1, data=salary)


###Step2 repeat 1: adding one predictor with smallest p-value. 
F2_model1 <- lm(Y~X1+X2, data=salary)
summary(F2_model1)$coefficients

F2_model2 <- lm(Y~X1+factor(X3), data=salary)
summary(F2_model2)$coefficients

F2_model3 <- lm(Y~X1+X4, data=salary)
summary(F2_model3)$coefficients


F2_model4 <- lm(Y~X1+X5, data=salary)
summary(F2_model4)$coefficients


F2_model5 <- lm(Y~X1+factor(X6), data=salary)
summary(F2_model5)$coefficients


F2_model6 <- lm(Y~X1+X7, data=salary)
summary(F2_model6)$coefficients


F2_model7 <- lm(Y~X1+X8, data=salary)
summary(F2_model7)$coefficients


F2_model8 <- lm(Y~X1+factor(X9), data=salary)
summary(F2_model8)$coefficients

F2_model9 <- lm(Y~X1+X10, data=salary)
summary(F2_model9)$coefficients

#X3 is the new predictor which can be added to our old model

lm(Y~X1+factor(X3), data=salary)


##Skip the rest of Forward selection, stepwise regression


library(olsrr)

full_model = lm(Y~X1+X2+factor(X3)+X4+X5+factor(X6)+X7+X8+factor(X9)+X10, data=salary)

##backward elimination 
ols_step_backward_p(full_model, prem=0.3, details = FALSE)
final_model_backward <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X9), data=salary)

##forward selection
ols_step_forward_p(full_model, penter=0.3, details = FALSE)
final_model_forward <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X9), data=salary)

##stepwise regression
ols_step_both_p(full_model, pent=0.3, prem = 0.3, details = FALSE)
final_model_stepwise <- lm(Y~X1+X2+factor(X3)+X4+X5+factor(X9), data=salary)


###In class problem 11
credit<-read.csv("credit.csv",header=TRUE)
head(credit)

full_model<-lm(Balance ~ Income+Limit+Rating+Cards+Age+Education+factor(Gender)
               +factor(Student)+factor(Married)+factor(Ethnicity), data=credit)
###backward elimination results
credit_backward=ols_step_backward_p(full_model, prem=0.3)
final_model_backward = lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Gender)
                          +factor(Student), data=credit)

###forward selection results 
credit_forward=ols_step_forward_p(full_model, penter=0.3)
final_model_forward = lm(Balance ~ Ratin+Income+factor(Student)+Limit+Cards+Age+factor(Gender), data=salgary)

###Stepwise regression results
credit_stepwise = ols_step_both_p(full_model, prem=0.3, pent=0.3)


###In class practice problem 13
library(olsrr)
credit=read.csv("credit.csv",header = TRUE)
full_model=lm(Balance ~ Income+Limit+Rating+Cards+Age+Education+factor(Gender)
              +factor(Ethnicity)+factor(Married)+factor(Student), data=credit)

forward_model = ols_step_forward_p(full_model,penter=0.3)
stepwise_model = ols_step_both_p(full_model,pent=0.3,prem=0.3)

forward_combos5=cbind("forward",forward_model$indvar, 
                      forward_model$rsquare, forward_model$adjr, 
                      forward_model$mallows_cp, forward_model$aic, forward_model$rmse)
stepwise_combos5=cbind("stepwise",stepwise_model$indvar, 
                       stepwise_model$rsquare, stepwise_model$adjr, 
                       stepwise_model$mallows_cp, stepwise_model$aic, stepwise_model$rmse)

combos5=data.frame(rbind(forward_combos5, stepwise_combos5))
colnames(combos5)=c("model","Variables", "R2","AdjR2","Cp","AIC","RMSE")
combos5
write.csv(combos5,"Lecture4_credit_combos5.csv",row.names=FALSE)



