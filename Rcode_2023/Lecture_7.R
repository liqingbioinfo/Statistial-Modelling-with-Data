library(ggplot2)
library(gridExtra)
library(GGally)

library(lmtest)
library(mctest)
library(car)

library(MASS)
library(Ecdat)
library(faraway)

setwd("D:/Github/Statistial-Modelling-with-Data/Datasets/DATA_603_L01")


#######################################
###########Examples####################
#######################################
Advertising <- read.table("Advertising.txt",header=TRUE,sep="\t")
head(Advertising)
model<-lm(sale~tv+radio+tv:radio, data=Advertising)

library(GGally)
ggpairs(Advertising)

ggcorr(Advertising)

library(mctest)
imcdiag(model, method="VIF")

# library(car)
# vif(model)


###Drag leverage plot for advertising dataset
highorder11<- lm(sale~tv+radio+tv:radio+I(tv^2)+I(tv^3)+I(tv^4)
                 +I(tv^5)+I(tv^6)+I(tv^7)+I(tv^8)+I(tv^9)+I(tv^10)+I(tv^11)
                 , data=Advertising)
bptest(highorder11)

lev=hatvalues(highorder11)
p = length(coef(highorder11))
n = nrow(Advertising)
outlier2p = lev[lev>(2*p/n)]
outlier3p = lev[lev>(3*p/n)]

plot(rownames(Advertising),lev, main = "Leverage in Advertising Dataset", xlab="observation", ylab = "Leverage Value")
abline(h = 2 *p/n, lty = 1, col="red")
abline(h = 3 *p/n, lty = 1, col="orange")

plot(highorder11, which=4)


plot(highorder11, which=5)



###Example 1 for BoxCox transformation
library(Ecdat)
head(Ecdat::University)
dim(Ecdat::University)

model<-lm(as.numeric(nassets)~as.numeric(stfees), data=University)

plot(model, 1)

library(MASS)
bc=boxcox(model,lambda=seq(-1,1))
bestlambda=bc$x[which(bc$y==max(bc$y))]
bestlambda
nassets_boxcox_trans <- as.numeric(((University$nassets^0.11)-1)/0.11)

model_boxcox1<-lm(nassets_boxcox_trans ~ as.numeric(stfees), data=University)

#Linearity
par(mfrow=c(1,2))
plot(model, 1)
plot(model_boxcox1, 1)

#Equal variance
par(mfrow=c(1,2))
plot(model, 3)
bptest(model) 

plot(model_boxcox1, 3)
bptest(model_boxcox1) 

#Normality 
par(mfrow=c(1,2))
plot(model, 2)
#shapiro.test(model)

plot(model_boxcox1, 2)
#shapiro.test(model_boxcox1)

#Multicollinearity
imcdiag(model, method="VIF")

imcdiag(model_boxcox1, method="VIF")

#Outline
par(mfrow=c(1,2))
plot(model, 5)
plot(model_boxcox1, 5)


##Example 2
library(faraway) 
ggcorr(gala)

reg1=lm(Species ~ Endemics+Area + Elevation + Nearest + Scruz + Adjacent, data = gala)

bc=boxcox(reg1,lambda=seq(-2,2))
bestlambda=bc$x[which(bc$y==max(bc$y))]
bestlambda
species_boxcox_trans <- as.numeric(((gala$Species^0.505)-1)/0.505)
reg1_boxcox1<-lm(species_boxcox_trans ~ Endemics+Area + Elevation + Nearest + Scruz + Adjacent, data=gala)

#Linearity
par(mfrow=c(1,2))
plot(reg1, 1)
plot(reg1_boxcox1, 1)

#Equal variance
par(mfrow=c(1,2))
plot(reg1, 3)
bptest(reg1) 

plot(reg1_boxcox1, 3)
bptest(reg1_boxcox1) 

#Normality 
par(mfrow=c(1,2))
plot(reg1, 2)
#shapiro.test(model)

plot(reg1_boxcox1, 2)
#shapiro.test(model_boxcox1)

#Multicollinearity
imcdiag(reg1, method="VIF")
imcdiag(reg1_boxcox1, method="VIF")

reg2=lm(Species ~ Endemics+Area  + Nearest+ Scruz + Adjacent, data = gala)
reg2_boxcox1<-lm(species_boxcox_trans ~ Endemics+Area + Nearest + Scruz + Adjacent, data=gala)

imcdiag(reg2, method="VIF")
imcdiag(reg2_boxcox1, method="VIF")

#Outline
par(mfrow=c(1,2))
plot(reg2, 5)
plot(reg2_boxcox1, 5)



###########################################
#######In class practice problems##########
###########################################
###Problem 19
credit <- read.csv("credit.csv",header=TRUE)
head(credit)
model<-lm(Balance~Income+Limit+Rating+Cards+Age+Education, data=credit)
imcdiag(model,method="VIF")

#Drop Limit
model1<-lm(Balance~Income+Rating+Cards+Age+Education, data=credit)
imcdiag(model1,method="VIF")


###Problem 20
workhours = read.csv("CLERICAL.csv",header=TRUE)
head(workhours)

ggcorr(workhours)

optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
imcdiag(optimal_model_approach2, method="VIF")

optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)
imcdiag(optimal_model_approach3, method="VIF")

###Problem 21
plot(optimal_model_approach2, 5)

plot(optimal_model_approach3, 5)


###Problem 22
salary <- read.csv("EXECSAL2.csv",header=TRUE)
head(salary)

model_dig <- lm(Y~ X1+I(X1^2)+X2+factor(X3)+X4+X5+factor(X3):X4, data=salary)

#Linearity 
plot(model_dig, 1) #Pass

#Independence, not time-seris data, Pass

#Equal variance. #Voilate the 3rd assumption
plot(model_dig, 3) 
bptest(model_dig) #p=0.007207, <0.05, H0 is rejected. Hetero

#Normality 
plot(model_dig, 2) 

factor(salary$X3)
salary[, 5] = as.numeric(as.factor(salary$X3))
head(salary)
model_dig1 <- lm(Y~ X1+I(X1^2)+X2+X3+X4+X5+X3:X4, data=salary)
shapiro.test(model_dig1)

#Multidisciplinary, Pass
#ggcorr
imcdiag(model_dig, method="VIF")

#Outlier. No influential points. 
plot(model_dig, 5) 
