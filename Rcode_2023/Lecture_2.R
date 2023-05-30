library(ggplot2)  
library(gridExtra)

setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")
Advertising=read.table("Advertising.txt",header = TRUE,  sep ="\t" )

###Interactions
interacmodel<-lm(sale~tv+radio+tv:radio, data=Advertising)
summary(interacmodel)

interacmodel1<-lm(sale~tv*radio, data=Advertising)
summary(interacmodel1)

interacmodel2<-lm(sale~(tv + radio)^2, data=Advertising)
summary(interacmodel2)

###In class Practice Problem 4
condominium=read.csv("condominium.csv",header = TRUE)

model1 = lm(listprice ~ livingarea + floors  + baths, data = condominium)
summary(model1)

model2 = lm(listprice ~ livingarea + floors  + baths + livingarea:floors+ livingarea:baths+ floors:baths, data = condominium)
summary(model2)


###In class Practice Problem 5
sales=read.csv("sales.csv",header = TRUE)
#5.1
model1 = lm(formula = Y ~ X1 + X2 + X3, data=sales)
summary(model1)

model2 = lm(formula = Y ~ X2 + X3, data=sales)
summary(model2)

anova(model2,model1)

#5.2
inter1=lm(formula = Y ~ X2 + X3 + X2:X3, data=sales)
summary(inter1)

inter2=lm(formula = Y ~ (X1+X2+X3)^2, data=sales)
summary(inter2)

inter3=lm(formula = Y ~ X1+ X2+ X3+ X2:X3 + X1:X3, data=sales)
summary(inter3)

###Dummy coding two levels
credit=read.csv("credit.csv",header = TRUE)
head(credit)

dummymodel<-lm(Balance~factor(Gender),data=credit)
summary(dummymodel)

#forget factor(...)
dummymodel1<-lm(Balance~Gender,data=credit)
summary(dummymodel1)

###In class Practice Problem 6
dummymodel2<-lm(Balance~factor(Married),data=credit)
summary(dummymodel2)


###Dummy coding three levels
salary=read.csv("salary.csv",header = TRUE)
head(salary)

dummymodel1<-lm(salary~factor(rank),data=salary)
summary(dummymodel1)

#forget factor(...)
dummymodel2<-lm(salary~rank,data=salary)
summary(dummymodel2)


###In class Practice Problem 7
salary=read.csv("salary.csv",header = TRUE)
head(salary)

dummymodel1<-lm(salary~factor(dept),data=salary)
summary(dummymodel1)

dummymodel2<-lm(salary~factor(rank) + factor(dept),data=salary)
summary(dummymodel2)
