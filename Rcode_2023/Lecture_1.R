#install.packages(ggplot2)
#install.packages(gridExtra)
library(ggplot2)  
library(gridExtra)

setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

Advertising=read.table("Advertising.txt",header = TRUE,  sep ="\t" )
#attach(Advertising)

############General linear models
p1<-ggplot(data=Advertising,mapping= aes(x=tv,y=sale))+geom_point(color='red')+ 
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

p2<-ggplot(data=Advertising,mapping= aes(x=radio,y=sale))+geom_point(color='green')+ 
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

p3<-ggplot(data=Advertising,mapping= aes(x=newspaper,y=sale))+geom_point(color='black')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

grid.arrange(
  p1,
  p2,
  p3,
  nrow = 1,
  top = "Sales regressed on one media"
)

summary(lm(sale~tv,data=Advertising))
summary(lm(sale~radio,data=Advertising))
summary(lm(sale~newspaper,data=Advertising))

reg1<-lm(sale~tv, data=Advertising)
coefficients(reg1)

reg2<-lm(sale~radio, data=Advertising)
coefficients(reg2)

reg3<-lm(sale~newspaper, data=Advertising)
coefficients(reg3)

###Multiple variables
p4<-ggplot(data=Advertising,mapping= aes(x=tv+radio,y=sale))+geom_point(color='red')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

p5<-ggplot(data=Advertising,mapping= aes(x=tv+newspaper,y=sale))+geom_point(color='green')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

p6<-ggplot(data=Advertising,mapping= aes(x=radio+newspaper,y=sale))+geom_point(color='black')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

p7<-ggplot(data=Advertising,mapping= aes(x=tv+radio+newspaper,y=sale))+geom_point(color='blue')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,300))

grid.arrange(
  p4,
  p5,
  p6,
  p7,
  nrow = 1,
  top = "Sales regressed on multiple medias"
)

reg4<-lm(sale~tv+radio, data=Advertising)
coefficients(reg4)

reg5<-lm(sale~tv+newspaper, data=Advertising)
coefficients(reg5)

reg6<-lm(sale~radio+newspaper, data=Advertising)
coefficients(reg6)

reg7<-lm(sale~tv+radio+newspaper, data=Advertising)
coefficients(reg7)


############Generalized linear models
hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50)
pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
glm_pass=glm(pass~hours, family=binomial(link='logit'))
coefficients(glm_pass)


############Confidence Interval
reg1<-lm(sale~tv+radio+newspaper, data=Advertising)
confint(reg1)
confint(reg1, level = 0.99)


############Inclass problem 1
condominium=read.csv("condominium.csv",header = TRUE)

p1<-ggplot(data=condominium,mapping= aes(x=livingarea,y=listprice ))+geom_point(color='red')+ 
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,5))

p2<-ggplot(data=condominium,mapping= aes(x=floors,y=listprice ))+geom_point(color='green')+ 
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,5))

p3<-ggplot(data=condominium,mapping= aes(x=bedrooms,y=listprice ))+geom_point(color='black')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,5))

p4<-ggplot(data=condominium,mapping= aes(x=baths,y=listprice))+geom_point(color='black')+
  geom_smooth(method = "lm", se = FALSE)+expand_limits(x=c(0,5))

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 1,
  top = "Condominium price regresses on one factor"
)

model1 = lm(listprice ~ livingarea + floors + bedrooms + baths, data = condominium)
summary(model1)
confint(model1) #by default alpha=0.05


############Evaluating Overall Model Utility, F-test
reg1<-lm(sale~tv+radio+newspaper, data=Advertising) # (Full) model with all variables
reg2<-lm(sale~1, data=Advertising) # Model with only intercept
anova(reg2,reg1) #models with more parameters on left, models with less parameters on right


full<-lm(sale~tv+radio+newspaper, data=Advertising)
reduced<-lm(sale~tv+radio, data=Advertising) # dropping the newspaper variable
anova(reduced,full) # test if Ho: newspaper = 0


############Inclass problem 2
condominium=read.csv("condominium.csv",header = TRUE)
full<-lm(listprice ~ livingarea + floors + bedrooms + baths, data=condominium)
reduced1<-lm(listprice ~ livingarea + floors + bedrooms, data=condominium) 
anova(reduced1,full) 

reduced2<-lm(listprice ~ livingarea + floors+ baths , data=condominium) 
anova(reduced2,full) 

reduced3<-lm(listprice ~ livingarea + baths , data=condominium) 
anova(reduced3,reduced2) 

reduced4<-lm(listprice ~ floors + baths , data=condominium) 
anova(reduced4,reduced2)

reduced5<-lm(listprice ~ livingarea + floors , data=condominium) 
anova(reduced5,reduced2) 



#############Model Fitness
full<-lm(sale~tv+radio+newspaper, data=Advertising)
reduced<-lm(sale~tv+radio, data=Advertising)
#R squared
summary(full)$r.squared
summary(reduced)$r.squared
summary(full)$adj.r.squared
summary(reduced)$adj.r.squared

#MSE
full<-lm(sale~tv+radio+newspaper, data=Advertising)
reduced<-lm(sale~tv+radio, data=Advertising)
sigma(full)
sigma(reduced)


#############Model Prediction
reduced<-lm(sale~tv+radio, data=Advertising)
newdata = data.frame(tv=200, radio=20)
predict(reduced,newdata,interval="predict")


#############Inclass problem 3
condominium=read.csv("condominium.csv",header = TRUE)
full<-lm(listprice ~ livingarea + floors + bedrooms + baths, data=condominium)
fit <-lm(listprice ~ livingarea + floors+ baths , data=condominium) 

summary(full)$adj.r.squared
sigma(full)

summary(fit)$adj.r.squared
sigma(fit)

newdata = data.frame(livingarea=2, floors=3, baths=3)
predict(fit,newdata,interval="predict") #95% prediction interval



