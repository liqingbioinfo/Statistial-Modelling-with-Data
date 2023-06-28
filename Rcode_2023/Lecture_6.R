library(ggplot2)
library(gridExtra)

setwd("D:/Github/Statistial-Modelling-with-Data/Datasets/DATA_603_L01")
Advertising <- read.csv("Advertising.txt",header=TRUE,sep="\t")
head(Advertising)
model<-lm(sale~tv+radio+tv:radio, data=Advertising)
#plot residual - fitted values to check whether linearity assumption is hold or not
plot(fitted(model), residuals(model), xlab="Fitted values", ylab="Residual")

#high order model

par(mfrow=c(3,1))

model<-lm(sale~tv+radio+tv:radio, data=Advertising)
#plot residual - fitted values to check whether linearity assumption is hold or not
plot(fitted(model), residuals(model), xlab="Fitted values", ylab="Residual")
abline(h=0, lty=1)

secondorder <- lm(sale~tv+radio+tv:radio+I(tv^2), data=Advertising)
plot(fitted(secondorder), residuals(secondorder), xlab="Fitted values", ylab="Residual")
abline(h=0, lty=1)

thirdorder<- lm(sale~tv+radio+tv:radio+I(tv^2)+I(tv^3), data=Advertising)
plot(fitted(thirdorder), residuals(thirdorder), xlab="Fitted values", ylab="Residual")
abline(h=0, lty=1)

###Check equal variance 
thirdorder<- lm(sale~tv+radio+tv:radio+I(tv^2)+I(tv^3), data=Advertising)
plot(fitted(thirdorder), sqrt(residuals(thirdorder)), xlab="Fitted values", ylab="Square root of Residual")
abline(h=0, lty=1)


p3<-ggplot(thirdorder, aes(x=.fitted, y=sqrt(abs(.stdresid))))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("sale~tv+radio+tv:radio+I(tv^2)+I(tv^3)")

library(lmtest)
bptest(model)
bptest(secondorder)
bptest(thirdorder)

highorder4<- lm(sale~tv+radio+tv:radio+I(tv^2)+I(tv^3)+I(tv^4), data=Advertising)
bptest(highorder4)

highorder10<- lm(sale~tv+radio+tv:radio+I(tv^2)+I(tv^3)+I(tv^4)
                  +I(tv^5)+I(tv^6)+I(tv^7)+I(tv^8)+I(tv^9)+I(tv^10)
                  , data=Advertising)
bptest(highorder10)

highorder11<- lm(sale~tv+radio+tv:radio+I(tv^2)+I(tv^3)+I(tv^4)
                +I(tv^5)+I(tv^6)+I(tv^7)+I(tv^8)+I(tv^9)+I(tv^10)+I(tv^11)
                , data=Advertising)
bptest(highorder11)

#4. Normality assumption 
#histgram
hist(highorder11$residuals,breaks = seq(-1,1,by=0.1))

ggplot(data=Advertising, aes(residuals(highorder11))) +
  geom_histogram(breaks = seq(-1,1,by=0.1), col="grey", fill="lightblue") + labs(title="Histogram for residuals") +labs(x="residuals", y="Count")

#QQ plot later

#KS test
shapiro.test(residuals(highorder11))
dim(Advertising) #200
sdh11 = sigma((highorder11))

ks.test(x=residuals(highorder11), y=rnorm(200, mean=0, sd= sqrt(sdh11)))



##########################################################
###############In class problem###########################
##########################################################

###In class problem 16
workhours = read.csv("CLERICAL.csv",header=TRUE)
head(workhours)
firstorder = lm(Y ~ X2 + X4 + X5, data=workhours)

plot(fitted(firstorder), residuals(firstorder), xlab="Fitted values", ylab="Residual")
abline(h=0, lty=1)

#ggplot code
ggplot(firstoder, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(Y ~ X2 + X4 + X5)")


optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
p2<-ggplot(optimal_model_approach2, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(Y ~ X2+X4+X5+I(X2^2)")

optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)
p3<-ggplot(optimal_model_approach3, aes(x=.fitted, y=.resid))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6)")

grid.arrange(
  p2,
  p3,
  nrow=2,
  top="Check linearity assumption for optimal models"
)

###3. Euqal variance assumption for optimal models
optimal_model_approach2=lm(Y ~ X2+X4+X5+I(X2^2), data=workhours)
p2<-ggplot(optimal_model_approach2, aes(x=.fitted, y=sqrt(abs(.stdresid))))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(Y ~ X2+X4+X5+I(X2^2)")

optimal_model_approach3 <-lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6), data=workhours)
p3<-ggplot(optimal_model_approach3, aes(x=.fitted, y=sqrt(abs(.stdresid))))+
  geom_point()+geom_smooth()+
  geom_hline(yintercept=0)+
  ggtitle("lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6)")

grid.arrange(
  p2,
  p3,
  nrow=2,
  top="Check equal variance assumption for optimal models"
)

bptest(optimal_model_approach2)
bptest(optimal_model_approach3)

###4.Normality assumptions for optimal models
par(mfrow=c(1,2))
hist(residuals(optimal_model_approach2),breaks=seq(-30, 30, 2))
hist(residuals(optimal_model_approach3),breaks=seq(-30, 30, 2))

##Q-Q plot,empirical distribution is normal distribution

p2<- ggplot(workhours, aes(sample=optimal_model_approach2$residuals)) +
  stat_qq() +
  stat_qq_line()+  
  ggtitle("lm(Y ~ X2+X4+X5+I(X2^2)")

p3<- ggplot(workhours, aes(sample=optimal_model_approach3$residuals)) +
  stat_qq() +
  stat_qq_line()+  
  ggtitle("lm(Y~(X1+X2+X3+X4+X5+X6+X1:X6+X2:X6)")

grid.arrange(
  p2,
  p3,
  nrow=2,
  top="Check normality assumption for optimal models"
)

#sw test
shapiro.test(residuals(optimal_model_approach2))
shapiro.test(residuals(optimal_model_approach3))



