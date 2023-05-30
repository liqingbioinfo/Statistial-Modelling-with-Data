library(olsrr)  

setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

credit=read.csv("credit.csv",header = TRUE)
head(credit)

full_model=lm(Balance ~ Income+Limit+Rating+Cards+Age+Education+factor(Gender)
              +factor(Ethnicity)+factor(Married)+factor(Student), data=credit)

#Combos 5 criterion for step by step model
ks=ols_step_best_subset(full_model, details=TRUE)

#For output interpretation
rsquare<-c(ks$rsquare)
AdjustedR<-c(ks$adjr)
cp<-c(ks$cp)
AIC<-c(ks$aic)
RMSE<-c(ks$msep)
cbind(rsquare,AdjustedR,cp,AIC, RMSE)

###Plot these five panels
par(mfrow=c(3,2)) # split the plotting panel into a 3 x 2 grid
plot(ks$rsq,type = "o",pch=10, xlab="Number of Variables",ylab= "Rˆ2")
plot(ks$adjr,type = "o",pch=10, xlab="Number of Variables",ylab= "Adjusted Rˆ2")
plot(ks$cp,type = "o",pch=10, xlab="Number of Variables",ylab= "Cp")
plot(ks$aic,type = "o",pch=10, xlab="Number of Variables",ylab= "AIC")
plot(ks$msep,type = "o",pch=10, xlab="Number of Variables",ylab= "RMSE")


###save results to a table
predictors <- c(ks$predictors)
number_of_predictors <- c(ks$n)
combo5_dynamical_construction <- data.frame(cbind(predictors,number_of_predictors,rsquare,AdjustedR,cp,AIC,RMSE))
colnames(combo5_dynamical_construction) <- c("Predictors", "# of Predictors","R2","AdjR2","Cp","AIC","RMSE")
write.csv(combo5_dynamical_construction, "Lecture5_credit_combo5.csv",row.names = FALSE)


####In class practice problem 14
first_order_optimal_model<-lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student),data=credit)


inter_full_model<-lm(Balance ~ (Income+Limit+Rating+Cards+Age+factor(Student))^2,data=credit)


#Stepwise regression to identify the optimal interaction model 
ols_step_both_p(inter_full_model, pent=0.05, prem=0.05, details = FALSE)
best_inter_model_stepwise<-lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                     +Rating:factor(Student)+Limit:Rating+Income:Rating+Income:factor(Student)
                     +Age:factor(Student)+Rating:Age
                     ,data=credit)

summary(best_inter_model_stepwise)
best_inter_model_final1 <-lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                             +Rating:factor(Student)+Limit:Rating+Income:Rating+Income:factor(Student)
                             +Rating:Age
                              ,data=credit)
summary(best_inter_model_final1)


#Best subsets, still running after 20 minutes. 
#ks = ols_step_best_subset(inter_full_model)
#Alternatively, use another package named "leap"
install.packages("leaps")
library(leaps)

best.subset<-regsubsets(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                        +Rating:factor(Student)+Limit:Rating+Income:Rating+Income:factor(Student)
                        +Rating:Age, data= credit, nv=10 )
#by default, regsubsets() only reports results up to the best 8-variable model
#Model selection by exhaustive search, forward or backward stepwise, or sequential replacement
#The summary() command outputs the best set of variables for each model size using RMSE.
summary(best.subset)

reg.summary<-summary(best.subset)
# for the output interpretation
rsquare<-c(reg.summary$rsq)
AdjustedR<-c(reg.summary$adjr2)
cp<-c(reg.summary$cp)
BIC<-c(reg.summary$bic)
RMSE<-c(reg.summary$rss)
cbind(rsquare,AdjustedR,cp,BIC,RMSE)


###Add high order predictors for Income+Limit+Rating+Cards+Age
best_inter_model_final1_high_order <-lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                                           +Rating:factor(Student)+Limit:Rating+Income:Rating+Income:factor(Student)
                                           +Rating:Age+I(Income^2)+I(Limit^2)+I(Rating^2)+I(Cards^2)+I(Age^2)
                                           ,data=credit)

ols_step_both_p(best_inter_model_final1_high_order,pent=0.05, prem=0.05, details = FALSE)

###Analyze correlations
par(mfrow=c(3,2))
plot(credit$Income,credit$Balance)
plot(credit$Limit,credit$Balance)
plot(credit$Rating,credit$Balance)
plot(credit$Cards,credit$Balance)
plot(credit$Age,credit$Balance)

cor(credit$Limit^2,credit$Balance)
cor(credit$Rating^2,credit$Balance)


####Use GGally
install.packages("GGally")
#remove.packages("GGally")
library(GGally)
ggcorr(credit,label=T)

ggpairs(credit,lower = list(continuous = "smooth_loess", combo =
                           "facethist", discrete = "facetbar", na = "na"))

###It appears that rating and limit worth trying higher orders

best_inter_model_final1_limit2_rating2<-lm(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                                           +Rating:factor(Student)+Limit:Rating+Income:Rating+Income:factor(Student)
                                           +Rating:Age+I(Limit^2)+I(Rating^2)
                                           ,data=credit)
summary(best_inter_model_final1_limit2_rating2) #Not significant, let's stop here! 



####In class problem 15
workhours=read.csv("CLERICAL.csv",header = TRUE)







