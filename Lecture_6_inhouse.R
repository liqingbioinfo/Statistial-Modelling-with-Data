library(ggplot2)
library(gridExtra)
setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

###==========================1. Check linearity 
Advertising=read.table("Advertising.txt", header = TRUE, sep ="\t" )
head(Advertising)
model<-lm(sale~tv+radio+tv:radio, data=Advertising)
summary(model)

plot(fitted(model), residuals(model),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("Interaction model Residual vs Fitted")

#Alternative way to save your plot
png("L6_1_autosave.png")
plot(fitted(model), residuals(model),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("Interaction model Residual vs Fitted")
dev.off()

#Use GGally to construct correlation matrix
library(GGally)
ggpairs(Advertising)

#Alternative way to save your plot
png("L6_2.png",width = 800, height = 800)
g <- ggpairs(Advertising)
print(g)
dev.off()

#Add high order predictors
intermodel<-lm(sale~tv+radio+tv:radio, data=Advertising)
quadmodel<-lm(sale~tv+I(tv^2)+radio+tv:radio, data=Advertising)
cubic<-lm(sale~tv+I(tv^2)+I(tv^3)+radio+tv:radio, data=Advertising)

summary(intermodel)$adj.r.squared
summary(quadmodel)$adj.r.squared
summary(cubic)$adj.r.squared

par(mfrow=c(2,2))

plot(fitted(intermodel), residuals(intermodel),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("Interaction model Residual vs Fitted")

plot(fitted(quadmodel), residuals(quadmodel),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("Second order model Residual vs Fitted")

plot(fitted(cubic), residuals(cubic),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("Third order model Residual vs Fitted")

###====================3. Equal variance: Scale location data for Advertising data
intermodel<-lm(sale~tv+radio+tv:radio, data=Advertising)
quadmodel<-lm(sale~tv+I(tv^2)+radio+tv:radio, data=Advertising)
cubic<-lm(sale~tv+I(tv^2)+I(tv^3)+radio+tv:radio, data=Advertising)

p1<-ggplot(intermodel, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth()+
  ggtitle("lm(sale~tv+radio+tv:radio)")

p2<-ggplot(quadmodel, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth()+
  ggtitle("lm(sale~tv+I(tv^2)+radio+tv:radio")

p3<-ggplot(cubic, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth()+
  ggtitle("lm(sale~tv+I(tv^2)+I(tv^3)+radio+tv:radio)")

grid.arrange(
  p1,
  p2,
  p3,
  nrow=3,
  top="Scale-Location plot : Standardized Residual vs Fitted values"
)

####Breusch-Pagan test to check if Heteroscedasticity exists. 
library(lmtest)
bptest(intermodel)
bptest(quadmodel)
bptest(cubic)

###Add more high order terms to fight against heteroscedasticity
morepower1<-lm(sale~tv+I(tv^2)+I(tv^3)+I(tv^4)+radio+tv:radio,
               data=Advertising)
summary(morepower1)
bptest(morepower1)

morepower2<-lm(sale~tv+I(tv^2)+I(tv^3)+I(tv^4)+I(tv^5)+radio+tv:radio,
               data=Advertising)
summary(morepower2)
bptest(morepower2)

morepower3<-lm(sale~tv+I(tv^2)+I(tv^3)+I(tv^4)+I(tv^5)+I(tv^6)+radio+tv:radio,data=Advertising)
summary(morepower3)
bptest(morepower3)

morepower11<-lm(sale~tv+I(tv^2)+I(tv^3)+I(tv^4)+I(tv^5)+I(tv^6)
                +I(tv^7)+I(tv^8)+I(tv^9)+I(tv^10)+I(tv^11)
                +radio+tv:radio,
                data=Advertising)
summary(morepower11)
bptest(morepower11)


###===============================4. Check normality 
##hist plot
dim(Advertising)
morepower11<-lm(sale~tv+I(tv^2)+I(tv^3)+I(tv^4)+I(tv^5)+I(tv^6)
                +I(tv^7)+I(tv^8)+I(tv^9)+I(tv^10)+I(tv^11)
                +radio+tv:radio,
                data=Advertising)


hist(morepower11$residuals)

ggplot(data=Advertising, aes(residuals(morepower11))) +
  geom_histogram(breaks = seq(-1,1,by=0.1), col="grey", fill="lightblue") +
  labs(title="Histogram for residuals") +
  labs(x="residuals", y="Count")


##Q-Q plot
#Illustration code
x1<-rnorm(1000, mean=50, sd=10)
x2<-rnorm(100, mean=30,sd=10)
hist(x1)
hist(x2)




ggplot(Advertising, aes(sample=morepower$residuals)) +
  stat_qq() +
  stat_qq_line()





#####################################################################
#################In class practice problems##########################
#####################################################################


###In class practice problem 16
workhours = read.csv("CLERICAL.csv",header=TRUE)
head(workhours)

firstorder_model <- lm(Y ~ X2+X4+X5, data=workhours)

plot(fitted(firstorder_model), residuals(firstorder_model),xlab="Fitted Values", ylab="Residuals")
abline(h=0,lty=1)
title("First order model Residual vs Fitted") #Not quite obvious

p1<- ggplot(firstorder_model, aes(x=.fitted, y=.resid)) +
  geom_point() +geom_smooth()+
  geom_hline(yintercept = 0)+
  ggtitle("lm(Y ~ X2+X4+X5)")
print(p1)


#ggpairs(workhours)
optimal_model_approach1=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)


p2<-ggplot(optimal_model_approach2, aes(x=.fitted, y=.resid)) +
  geom_point() +geom_smooth()+
  geom_hline(yintercept = 0)+
  ggtitle("lm(Y ~ X2+X4+X5+I(X2^2)")

p3<-ggplot(optimal_model_approach3, aes(x=.fitted, y=.resid)) +
  geom_point() +geom_smooth()+
  geom_hline(yintercept = 0)+
  ggtitle("lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6)")

grid.arrange(
  p1,
  p2,
  p3,
  nrow = 3,
  top = "Lineality assumption for three models"
)




###In class practice problem 17
workhours = read.csv("CLERICAL.csv",header=TRUE)
head(workhours)

optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
optimal_model_approach3=lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)

#gglot and BP test

p1<-ggplot(optimal_model_approach2, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth()+
  ggtitle("lm(Y ~ X2+X4+X5+I(X2^2)")

p2<-ggplot(optimal_model_approach3, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth()+
  ggtitle("lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6)")

grid.arrange(
  p1,
  p2,
  nrow=2,
  top="CLERICAL data: Scale-Location plot : Standardized Residual vs Fitted values"
)

library(lmtest)
bptest(optimal_model_approach2)
bptest(optimal_model_approach3)





