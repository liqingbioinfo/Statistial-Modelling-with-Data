library(data.table)
library(density)
library(corrr)
library(dplyr)
library(AICcmodavg)

set.seed(2022-01-05)

##Experimental design 1
bloodpressure=read.csv("bloodpressure.csv")
str(bloodpressure) #Read your data set and double check that dependent and independent variables are correctly read by R
CRD<-aov(bloodpressure~treatment, data=bloodpressure) #Perform ANOVA for CRD
summary(CRD)
boxplot(bloodpressure~treatment, data=bloodpressure, main="Boxplot diagram for the different Levels") 
#a visual comparison of the data obtained at the different levels



##rep(1,40); seq(1,10,2); sample(1:2, 1, replace=TRUE)/sample(c(0,1),100,replace=TRUE); sort(x)
##define empty arraies: c1 <- c2 <- numeric(20)
##for(x in seq(20)){...}/ for(x in 1:20){...}
##par(mfrow=c(2,2))

##load data
salary_table = data.frame(fread("E:/PHD/Courses/Data 603/Datasets/salary.csv"))

##explore y 
##Distribution check: 
qqnorm(salary_table$merit, pch = 1, frame = FALSE)
qqline(salary_table$merit, col = "steelblue", lwd = 2)
qqplot(x,y)
plot(hist(salary_table$merit))
plot(density(salary_table$merit))

##explore x correlations
variable_table = salary_table[-6] #remove the 1st col, vs salary_table[c(1,2,3,4,5)]
variable_table %>% correlate() %>% focus(salary)
salary_table %>% correlate() %>% focus(merit)

##fit full model to predict merit
lm_merit_predict1 = lm(salary_table$merit ~ salary_table$salary+salary_table$gender+salary_table$rank+salary_table$year)

##summary over model
summary(lm_merit_predict1)
inter = summary(lm_merit_predict1)$coefficients[1,1]
b1=summary(lm_merit_predict1)$coefficients[2,1]
b2=summary(lm_merit_predict1)$coefficients[3,1]
b3=summary(lm_merit_predict1)$coefficients[4,1]
r2_adj = summary(lm_merit_predict1)$adj.r.squared

##prediction
predict_merit <- function(salary, gender, rank){
  return(b1*salary+b2*gender+b3*rank+inter)
}
predict_merit(40,1,2)

#>>
predict(lm_merit_predict1,newdata, interval="predict")

##fit reduced model to predict merit
lm_merit_predict2 = lm(salary_table$merit ~ salary_table$year)

##Partial F test
anova(lm_merit_predict2, lm_merit_predict1)

##Obtain a 95% confidence interval of regression coefficient for TEMP from the model in part c.
## X_AVG +/- 1.96*sample_sigma
beta_mean=summary(lm_merit_predict2)$coefficients[2,1]
beta_sd=summary(lm_merit_predict2)$coefficients[2,2]
zscores<-qt(0.95,df=29)
lowerbound = beta_mean-zscores*(beta_sd)
upperbound = beta_mean+zscores*(beta_sd)
print(paste0("95% critical interval is ",as.character(round(lowerbound,2)),as.character(round(upperbound,2))))

#>>
confint(lm_merit_predict2)


##compare two models using AIC
model_list = list(lm_merit_predict2,lm_merit_predict1)
model_names = c("reduced","full")
aictab(model_list,modnames = model_names)


######################Differernt hypothesis tests#########################
qnorm()
qf(p=0.95, df1=2, df2=8) #lower.tail=FALSE
qt(0.95,df=10)

require(graphics)
## Formula interface
t.test(extra ~ group, data = sleep)
## Formula interface to one-sample test
t.test(extra ~ 1, data = sleep)



#####################Plots########################################
#hist

#density

#QQ
bcmodel2=lm((((Butterfat^bestlambda)-1)/bestlambda)~Breed,data=butterfat)
summary(bcmodel2)
plot(bcmodel2)

#box
boxplot(vibration~brand, data=vibration, main="Boxplot diagram for the different Levels") 



