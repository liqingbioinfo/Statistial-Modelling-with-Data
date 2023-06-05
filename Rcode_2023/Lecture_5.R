#install.packages("GGally")

library(ggplot2)
setwd("D:/Github/Statistial-Modelling-with-Data/Datasets/DATA_603_L01")

###Construct criteria combos 5 for In class practice problem 13
library(olsrr)
credit=read.csv("credit.csv",header = TRUE)
full_model<- lm(Balance ~ Income+Limit+Rating+Cards+Age+Education+factor(Gender)
                +factor(Ethnicity)+factor(Married)+factor(Student), data=credit)

ks <- ols_step_best_subset(full_model)

combos5 <- data.frame(cbind(ks$n, ks$predictors,ks$rsquare, ks$adjr, ks$cp, ks$aic, ks$msep))
colnames(combos5) <- c("model numbe", "predictors","rsquare","adjustedRs","Cp","AIC","RMSE")
write.csv(combos5, "Lecture5_credit_combo5_inclassdemo.csv",row.names = FALSE)


###In class practice problem 14
credit=read.csv("credit.csv",header = TRUE)

bestmodelfirstoder <- lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student), data=credit)

model_inter <- lm(Balance ~ (Income+Limit+Rating+Cards+Age+factor(Student))^2, data=credit)

ols_step_both_p(model_inter, pret=0.05, prem=0.05, detail=FALSE)

model_inter_stepwise <- lm(Balance ~Income+Limit+Rating+Cards+Age+factor(Student)+
                            Limit:Rating+Rating:factor(Student)+Income:Rating
                           +Income:factor(Student)+Age:factor(Student)+Rating:Age
                            , data=credit)

summary(model_inter_stepwise)

model_inter_stepwise_refine1 <- lm(Balance ~Income+Limit+Rating+Cards+Age+factor(Student)+
                                     Limit:Rating+Rating:factor(Student)+Income:Rating
                                   +Income:factor(Student)+Rating:Age
                                   , data=credit)
summary(model_inter_stepwise_refine1)

###Alernatively, we can use ols_step_best_subset()
#model_inter <- lm(Balance ~ (Income+Limit+Rating+Cards+Age+factor(Student))^2, data=credit)
#ks <- ols_step_best_subset(model_inter)
#combos5 <- data.frame(cbind(ks$n, ks$predictors,ks$rsquare, ks$adjr, ks$cp, ks$aic, ks$msep))
#colnames(combos5) <- c("model numbe", "predictors","rsquare","adjustedRs","Cp","AIC","RMSE")
#write.csv(combos5, "Lecture5_credit_combo5_problem14.csv",row.names = FALSE)


###Add high order terms to the optimal model with interaction terms

model_inter_stepwise_refine1_highorder1 <- lm(Balance ~Income+Limit+Rating+Cards+Age+factor(Student)+
                                     Limit:Rating+Rating:factor(Student)+Income:Rating
                                   +Income:factor(Student)+Rating:Age+
                                     I(Income^2)+I(Limit^2)+ I(Rating^2)+I(Cards^2)+I(Age^2)
                                   , data=credit)

ols_step_both_p(model_inter_stepwise_refine1_highorder1, pret=0.05, prem=0.05)



###Get the correlations between high order predictors and response
cor((credit$Income)^2, credit$Balance, method="pearson")
cor((credit$Limit)^2, credit$Balance, method="pearson")
cor((credit$Rating)^2, credit$Balance, method="pearson")
cor((credit$Cards)^2, credit$Balance, method="pearson")
cor((credit$Age)^2, credit$Balance, method="pearson")


model_inter_stepwise_refine1_highorder2 <- lm(Balance ~Income+Limit+Rating+Cards+Age+factor(Student)+
                                                Limit:Rating+Rating:factor(Student)+Income:Rating
                                              +Income:factor(Student)+Rating:Age+
                                              I(Limit^2)+ I(Rating^2)
                                              , data=credit)

ols_step_both_p(model_inter_stepwise_refine1_highorder2, pret=0.05, prem=0.05)

###Vilulize correlation matrix between all predictors and response
library(GGally)

ggpairs(credit, list(continuous = "smooth_loess", combo ="facethist", discrete = "facetbar", na = "na"))

library(gridExtra)
p1<- ggcorr(credit) #default way to calculate correlatio is pearson
p2<- ggcorr(credit, method=c("everything","spearman"))  #change the way to calculate correlatio to spearman

grid.arrange(
  p1,
  p2,
  nrow = 1,
  top = "Correlation matrix(Left: pearson, Right: Spearman)"
)



###=================================================================================
###In class practice problem 15! Last in class question for model selection. 
#read.table("", sep="") #give right deliminator of file to read.table
#workhours = read.table("CLERICAL.csv", sep=",") 

workhours = read.csv("CLERICAL.csv",header=TRUE)
head(workhours)

###Approach 1:
###1.1 Build first order statistical model
firstodermodel <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7, data=workhours)
summary(firstodermodel)

model1 <- lm(Y ~ X2+X4+X5, data=workhours) #

anova(model1, firstodermodel) 
#null hypothesis, coeffcients for all removed predictors=0. P=0.08 > 0.05. We cannot refuse H0.
#To conclude, we can remove these predictors. 

###1.2 Add interaction terms
model1_inter <- lm(Y ~(X2+X4+X5)^2, data=workhours)
summary(model1_inter)  #non of them are sig, no interaction terms will be included into the model


###1.3 Add high order terms
ggpairs(workhours, lower=list(continuous = "smooth_loess"))
###I want to put the Y as the last column
Y<-workhours[,3]
workhours1 <- workhours[,-3]
head(workhours1,3)
workhours_ggally <- cbind(workhours1, Y)
head(workhours_ggally,3)
ggpairs(workhours_ggally, lower=list(continuous = "smooth_loess"))

#high order term fro X2
model1_high_order <- lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
summary(model1_high_order)

#Try high order term fro X5
model1_high_order2 <- lm(Y ~ X2+X4+X5+I(X2^2)+I(X5^2), data=workhours)
summary(model1_high_order2)

########Final model from approach 1
optimal_model_approach1=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)

###============================================================
###Approach 2: Use Stepwise regression
###2.1 Build first order statistical model
library(olsrr)
firstordermodel<-lm(Y~X1+X2+X3+X4+X5+X6+X7,data=workhours)
ols_step_both_p(firstordermodel,pent = 0.05, prem = 0.05, details=FALSE)

firstordermodel_stepwise<-lm(Y~X2+X4+X5,data=workhours)
summary(firstordermodel_stepwise)

##@2.2 Add interaction terms
firstordermodel_stepwise_inter <- lm(Y~(X2+X4+X5)^2,data=workhours)
stepres <- ols_step_both_p(firstordermodel_stepwise_inter, 
                pent = 0.05, prem = 0.05, details=FALSE)
summary(stepres$model)

firstordermodel_stepwise_inter_refine <- lm(Y~(X2+X4+X5+X4:X5),data=workhours)
summary(firstordermodel_stepwise_inter_refine) 
#remove X4:X5 as its p-value is not sig, no interaction term is added

###2.3 Add high order terms
firstordermodel_stepwise_highorder1 <-lm(Y~X2+X4+X5+I(X2^2)+I(X4^2)+I(X5^2),data=workhours)
stepres <- ols_step_both_p(firstordermodel_stepwise_highorder1, 
                        pent = 0.05, prem = 0.05, details=FALSE)
summary(stepres$model)

########Final model from approach 2
optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)

###============================================================
###Approach 3:Use ols_step_best_subset and criteria combos5
###3.1 Build first order statistical model
library(olsrr)
firstordermodel<-lm(Y~X1+X2+X3+X4+X5+X6+X7,data=workhours)
ks <- ols_step_best_subset(firstordermodel)
combos5 <- data.frame(cbind(ks$n, ks$predictors, ks$rsquare, ks$adjr, ks$cp, ks$aic, ks$msep))
colnames(combos5) <- c("model numbe", "predictors","rsquare","adjustedRs","Cp","AIC","RMSE")
write.csv(combos5, "Lecture5_InClassProblem15_FirstOrder_combo5.csv",row.names = FALSE)

optimal_model_from_combos5<-lm(Y~X1+X2+X3+X4+X5+X6, data=workhours)

###3.2 Add interaction terms
optimal_model_from_combos5_inter <- lm(Y~(X1+X2+X3+X4+X5+X6)^2, data=workhours)
summary(optimal_model_from_combos5_inter)

optimal_model_from_combos5_inter_refine1<-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)
summary(optimal_model_from_combos5_inter_refine1) 
##keep these two interactions predictors


###3.3 Add high order terms
optimal_model_from_combos5_inter_refine1_highorder1<-lm(Y~X1+X2+X3+X4+X5+X6
                                                       +X1:X6+X2:X6+I(X2^2), 
                                                       data=workhours)
summary(optimal_model_from_combos5_inter_refine1_highorder1)

optimal_model_from_combos5_inter_refine1_highorder2<-lm(Y~X1+X2+X3+X4+X5+X6
                                                       +X1:X6+X2:X6+I(X5^2), 
                                                       data=workhours)
summary(optimal_model_from_combos5_inter_refine1_highorder2)

###Please go ahead trying high order for X1, X3, X4, X6
###The optimal model we get from approach 3 is:
optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)


###Comparison between the optimal models from three approaches
summary(optimal_model_approach1)

summary(optimal_model_approach2)

summary(optimal_model_approach3)

