library(ggplot2)  
library(gridExtra)

setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

###Interaction Effect in Multiple Regression with both Quantitative and Qualitative (Dummy) Variable Models
credit=read.csv("credit.csv",header = TRUE)
mixmodel<-lm(Balance~Income+factor(Student), data=credit)
summary(mixmodel)


credit=read.csv("credit.csv",header = TRUE)
mixmodel<- lm(Balance~Income+factor(Student),data=credit)
#For student y=593.8135+5.9843Income
#For nonstudent y=211.1430+5.9843 Income
nonstudent=function(x){coef(mixmodel)[2]*x+coef(mixmodel)[1]}
student=function(x){coef(mixmodel)[2]*x+coef(mixmodel)[1]+coef(mixmodel)[3]}
ggplot(data=credit,mapping= aes(x=Income,y=Balance,colour=Student))+geom_point()+
  stat_function(fun=nonstudent,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=student,geom="line",color=scales::hue_pal()(2)[2])

###Mixed model
credit=read.csv("credit.csv",header = TRUE)
mixmodel<- lm(Balance~Income+factor(Student)+Income*factor(Student),data=credit)
summary(mixmodel)


credit=read.csv("credit.csv",header = TRUE)
mixmodel<- lm(Balance~Income+factor(Student),data=credit)
#For student y=677.2992+4.219Income
#For nonstudent y=200.6232+6.2182Income
student=function(x){4.219*x+677.2992}
nonstudent=function(x){coef(mixmodel)[2]*x+coef(mixmodel)[1]}
ggplot(data=credit,mapping= aes(x=Income,y=Balance,colour=Student))+geom_point()+
  stat_function(fun=nonstudent,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=student,geom="line",color=scales::hue_pal()(2)[2])


###In class Practice Problem 8
credit=read.csv("credit.csv",header = TRUE)
head(credit)

###Dr. Thuntida Ngamkham’s approach
model1 = lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+Education
            +factor(Gender)+factor(Ethnicity)+factor(Married)+factor(Student), data=credit)
summary(model1)

model2 = lm(formula = Balance ~ (Income+Limit+Rating+Cards+Age+factor(Student))^2, data=credit)
summary(model2)

model3=lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
          +Income*Age+Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student),data=credit)
summary(model3)

model4=lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
          +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student),data=credit)
summary(model4)

###Leah's approach
#Step1: Build an interaction model with all predictors
model_inter = lm(formula = Balance ~ (Income+Limit+Rating+Cards+Age+Education
                 +factor(Gender)+factor(Ethnicity)+factor(Married)+factor(Student))^2, data=credit)
summary(model_inter)

#Step2: Select significant predictors, remove non-significant predictors
coefficeints_model_inter = data.frame(summary(model_inter)[4])
sig_coefficeints_model_inter = coefficeints_model_inter[coefficeints_model_inter$coefficients.Pr...t.. < 0.05,]
sig_coefficeints_model_inter

#Step3: Build refined model 1 with significant interaction predictors
model_inter_refine1 = lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+Education
                                             +factor(Gender)+factor(Ethnicity)+factor(Student)+
                          Income:Rating+ Income:Age+ Income:Education+ Income:factor(Gender)+
                          Income:factor(Ethnicity)+Income:factor(Student)+Limit:Rating+
                          Limit:factor(Student)+Cards:factor(Gender), data=credit)
summary(model_inter_refine1)

#Step 4: Select significant interaction predictors for refined model 1
coefficeints_model_inter_refine1=data.frame(summary(model_inter_refine1)[4])
si_coefficients_model_inter_refine1 = coefficeints_model_inter_refine1[coefficeints_model_inter_refine1$coefficients.Pr...t.. < 0.05,]
si_coefficients_model_inter_refine1

#Step 5: Build refined model 2 with significant interaction predictors from model 1
model_inter_refine2 = lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)+Education+
                           Income:Rating+Income:Education+Income:factor(Student)+Limit:Rating+
                           Limit:factor(Student), data=credit)
summary(model_inter_refine2)

#Step 6: Select significant interaction predictors for refined model 2
coefficeints_model_inter_refine2=data.frame(summary(model_inter_refine2)[4])
si_coefficients_model_inter_refine2 = coefficeints_model_inter_refine2[coefficeints_model_inter_refine2$coefficients.Pr...t.. < 0.05,]
si_coefficients_model_inter_refine2


#Step 7: Build refined model 3 with significant interaction predictors from model 2
model_inter_refine3 = lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)+
                           Income:Rating+Income:factor(Student)+Limit:Rating+
                           Limit:factor(Student), data=credit)
summary(model_inter_refine3)

#Stop! We got a final model as coefficients for all interaction terms are significant!!!



###A Quadratic (Second Order) Model
aerobicdata=read.csv("AEROBIC.csv",header = TRUE)
ggplot(data=aerobicdata,mapping= aes(x=MAXOXY,y=IGG))+geom_point(color='red')+
  geom_smooth()

simplemodel=lm(IGG~MAXOXY,data=aerobicdata)
summary(simplemodel)

quadmodel=lm(IGG~MAXOXY+I(MAXOXY^2),data=aerobicdata)
summary(quadmodel)

cubemodel=lm(IGG~MAXOXY+I(MAXOXY^2)+I(MAXOXY^3),data=aerobicdata)
summary(cubemodel)

forthmodel=lm(IGG~MAXOXY+I(MAXOXY^2)+I(MAXOXY^3)+I(MAXOXY^4),data=aerobicdata)
summary(forthmodel)# should stop at cubemodel because all variables are not significant.



###In class Practice Problem 9
quality = read.csv("PRODQUAL.csv",header = TRUE)
head(quality)
dim(quality)
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

model6=lm(formula = QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4)+I(PRESSURE^5)+I(PRESSURE^6), data=quality)
summary(model6)


#Cross validation, In class problem 8+
#install.packages(DAAG)
library(DAAG)
credit=read.csv("credit.csv",header = TRUE)
head(credit)
mk=10

model_inter_refine3=lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                       +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)
                       ,data=credit)
summary(model_inter_refine3)

model_inter_high_order1=lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                           +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)
                           +I(Income^2)
                           ,data=credit)
summary(model_inter_high_order1)

model_inter_high_order2=lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                           +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)
                           +I(Income^2)+I(Rating^2)+I(Cards^2)+I(Age^2)
                           ,data=credit)
summary(model_inter_high_order2)


out1<-CVlm(data=credit, m=mk, seed=20230525, 
           form.lm = formula(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                             +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)))

out2<-CVlm(data=credit, m=mk, seed=20230525, 
           form.lm = formula(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                             +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)
                             +I(Income^2)))

out3<-CVlm(data=credit, m=mk, seed=20230525, 
           form.lm = formula(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                             +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)
                             +I(Income^2)+I(Rating^2)+I(Cards^2)+I(Age^2)))


cv_error1<-mean((out1$cvpred-out1$Balance)^2)

cv_error2<-mean((out2$cvpred-out2$Balance)^2)

cv_error3<-mean((out3$cvpred-out3$Balance)^2)

print(paste(cv_error1, cv_error2, cv_error3))

#Plot
x=c(1,2,3)
y=c(cv_error1, cv_error2, cv_error3) #test errors
plot(x, y, col = "red", lwd = 2)


###Cross validation In Class problem 9
library(DAAG)
quality=read.csv("PRODQUAL.csv",header = TRUE)
head(quality)

quality_out1 <- CVlm(data=quality, m=mk, seed=20230526, 
                     form.lm = formula(QUALITY ~ PRESSURE))

quality_out2 <- CVlm(data=quality, m=mk, seed=20230526, 
                     form.lm = formula(QUALITY ~ PRESSURE + I(PRESSURE^2)))


quality_out3 <- CVlm(data=quality, m=mk, seed=20230526, 
                     form.lm = formula(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)))


quality_out4 <- CVlm(data=quality, m=mk, seed=20230526, 
                     form.lm = formula(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4)))

quality_out5 <- CVlm(data=quality, m=mk, seed=20230526, 
                     form.lm = formula(QUALITY ~ PRESSURE + I(PRESSURE^2) + I(PRESSURE^3)+I(PRESSURE^4)+I(PRESSURE^5)))


cv_error1<-mean((quality_out1$cvpred-quality_out1$QUALITY)^2)

cv_error2<-mean((quality_out2$cvpred-quality_out2$QUALITY)^2)

cv_error3<-mean((quality_out3$cvpred-quality_out3$QUALITY)^2)

cv_error4<-mean((quality_out4$cvpred-quality_out4$QUALITY)^2)

cv_error5<-mean((quality_out5$cvpred-quality_out5$QUALITY)^2)

print(paste(cv_error1, cv_error2, cv_error3, cv_error4, cv_error5))



###Another example of cross validation 
overfit_example=read.csv("Overfitting.csv",header = TRUE)
head(overfit_example)

model_inter=lm(Y~(X1+X2+X3)^2, data = overfit_example)
summary(model_inter)

model_inter_refine = lm(Y~ X1+X2+X3+X2:X3,data = overfit_example)
summary(model_inter_refine)

model_inter_refine_higher_order1 = lm(Y~ X1+X2+X3+X2:X3+I(X1^2),data = overfit_example)
summary(model_inter_refine_higher_order1)

model_inter_refine_higher_order2 = lm(Y~ X1+X2+X3+X2:X3+I(X2^2),data = overfit_example)
summary(model_inter_refine_higher_order2)

model_inter_refine_higher_order3 = lm(Y~ X1+X2+X3+X2:X3+I(X3^2),data = overfit_example)
summary(model_inter_refine_higher_order3)

model_inter_refine_higher_order4 = lm(Y~ X1+X2+X3+X2:X3+I(X3^2)+I(X1^3),data = overfit_example)
summary(model_inter_refine_higher_order4)

model_inter_refine_higher_order5 = lm(Y~ X1+X2+X3+X2:X3+I(X3^2)+I(X1^3)+I(X1^4),data = overfit_example)
summary(model_inter_refine_higher_order5)


#USE CVlm() function 
library(DAAG)
mk=10

out1<-CVlm(data=overfit_example, m=mk, seed=20230525, 
           form.lm = formula(Y~ X1+X2+X3+X2:X3))

out2<-CVlm(data=overfit_example, m=mk, seed=20230525, 
           form.lm = formula(Y~ X1+X2+X3+X2:X3+I(X3^2)))

out3<-CVlm(data=overfit_example, m=mk, seed=20230525, 
           form.lm = formula(Y ~ X1+X2+X3+X2:X3+I(X3^2)+I(X1^3)))

out4<-CVlm(data=overfit_example, m=mk, seed=20230525, 
           form.lm = formula(Y~ X1+X2+X3+X2:X3+I(X3^2)+I(X1^3)+I(X1^4)))

#increase print limit to 2000 values
#options(max.print=2000)

cv_error1<-mean((out1$cvpred-out1$Y)^2)

cv_error2<-mean((out2$cvpred-out2$Y)^2)

cv_error3<-mean((out3$cvpred-out3$Y)^2)

cv_error4<-mean((out4$cvpred-out4$Y)^2)

print(paste(cv_error1, cv_error2, cv_error3, cv_error4))







