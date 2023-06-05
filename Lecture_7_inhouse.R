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
##########Examples#####################
#######################################
credit <- read.csv("credit.csv",header=TRUE)
head(credit)

ggpairs(credit)
ggcorr(credit)

#VIF for each predictor
Advertising=read.table("Advertising.txt", header = TRUE, sep ="\t" )
ggpairs(Advertising)
ggcorr(Advertising)

firstordermodel<-lm(sale~tv+radio, data=Advertising)

#install.packages("mctest")
library(mctest)
imcdiag(firstordermodel, method="VIF")

library(car)
vif(firstordermodel)

#logmodel<-lm(sale~log(tv)+radio+tv*radio, data=Advertising)
Advertising=read.table("Advertising.txt", header = TRUE, sep ="\t" )
morepower<-lm(sale~tv+I(tv^2)+I(tv^3)+I(tv^4)+I(tv^5)+I(tv^6)+I(tv^7)+I(tv^8)+
                I(tv^9)+I(tv^10)+I(tv^11)+radio+tv*radio, data=Advertising)

##leverage plot
lev=hatvalues(morepower)
p = length(coef(morepower))
n = nrow(Advertising)
outlier2p = lev[lev>(2*p/n)]
outlier3p = lev[lev>(3*p/n)]
print("h_I>2p/n, outliers are")
plot(rownames(Advertising),lev, main = "Leverage in Advertising Dataset", xlab="observation",
     ylab = "Leverage Value")
abline(h = 2 *p/n, lty = 1, col="red")
abline(h = 3 *p/n, lty = 1, col="orange")

##Cook's distance
plot(morepower,which=4)

#Leverage and Influence
plot(morepower,which=5)

####Transformation
#Example 1
#install.packages("Ecdat")
library(Ecdat)
reg=lm(nassets~stfees,data=University)
summary(reg)

#Testing for Linearity
ggplot(reg, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(nassets~stfees)")

#Testing for Homoscedasticity
library(lmtest)
bptest(reg)

#Testing for Normality
shapiro.test(residuals(reg))

#Multicollinearity, at least two variables
# library(mctest)
# imcdiag(reg, method="VIF")

#Outlier
plot(reg, which=5)

##Conduct box-cox transformation
library(MASS)
bc=boxcox(reg,lambda=seq(-1,1))

bestlambda=bc$x[which(bc$y==max(bc$y))]
bestlambda #.1111

bcmodel1=lm((((nassets^0.1111)-1)/0.1111)~stfees,data=University)
summary(bcmodel1)

ggplot(bcmodel1, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(nassets~stfees)")

#Testing for Linearity
ggplot(bcmodel1, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(nassets~stfees)")

#Testing for Homoscedasticity
bptest(bcmodel1)

#Testing for Normality
shapiro.test(residuals(bcmodel1))

#Outlier
plot(bcmodel1, which=5)

#Example 2
library(faraway)
head(faraway::gala)
reg1=lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(reg1)

reg2=lm(Species ~ Elevation + Adjacent, data = gala)
summary(reg2)

#Linearity
ggplot(reg2, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("Raw data")

#Testing for Homoscedasticity
bptest(reg2)

#Testing for Normality
shapiro.test(residuals(reg2))

#Outlier
plot(reg2, which=5)

#Box-Cox transformation
bc=boxcox(reg2,lambda=seq(-1,1))

bestlambda=bc$x[which(bc$y==max(bc$y))]
bestlambda

#We found that bestlambda=0.2525
bcmodel2=lm((((Species^0.2525)-1)/0.2525) ~ Elevation + Adjacent, data = gala)
summary(bcmodel2)

#Testing for Linearity
ggplot(bcmodel2, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("Data after Box-Cox Transformation")

#Testing for Homoscedasticity
bptest(bcmodel2)

#Testing for Normality
shapiro.test(residuals(bcmodel2))

#Multicollinearity, at least two variables
imcdiag(bcmodel2, method="VIF")

#Outlier
plot(bcmodel2, which=5)

#Remove data point
row_names_to_remove<-c("Isabela")
gala_new = gala[!(row.names(gala) %in% row_names_to_remove),]

bcmodel3=lm((((Species^0.2525)-1)/0.2525) ~ Elevation + Adjacent, data = gala_new)
plot(bcmodel3, which=5)


#########################################
#########In class practice problems######
#########################################

###In class practice problem 19
workhours = read.csv("CLERICAL.csv",header=TRUE)
head(workhours)

firstorder = lm(Y ~ X2 + X4 + X5, data=workhours)

optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)

optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)

#
ggcorr(workhours)
#VIF
imcdiag(optimal_model_approach2,method="VIF")
imcdiag(optimal_model_approach3,method="VIF")


###In class practice problem 20
credit = read.csv("credit.csv",header=TRUE)
head(credit)

firstorder=lm(Balance ~ Income + Rating + Age + Limit + Cards + factor(Student), data=credit)
imcdiag(firstorder, method="VIF")

#Remove Limit or Rating
firstorder1 <- lm(Balance ~ Income + Rating + Age + Cards + factor(Student), data=credit)
imcdiag(firstorder1, method="VIF")


###In class practice problem 21
optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
plot(optimal_model_approach2, which=5)

optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)
plot(optimal_model_approach3, which=5)

