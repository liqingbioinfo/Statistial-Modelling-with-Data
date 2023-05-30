
setwd("E:/PHD/Courses/Data 603/Datasets/DATA_603_L01/")

set.seed(123)  # Set a seed for reproducibility
X1 <- sample(1:5, 1000, replace = TRUE)
X2=rnorm(1000, mean=10, sd=5)
X3=rnorm(1000, mean=-10, sd=3)
error = rnorm(1000, mean=200, sd=10)

Y <- X1*X1+(X2*X3)+100*sin(X3) + error
data <- data.frame(X1, X2, X3 ,Y)

write.csv(data, "Overfitting.csv", row.names = FALSE)



library(DAAG)
credit=read.csv("credit.csv",header = TRUE)
mk=10

model_inter_refine2 = lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+Education
                         +factor(Gender)+factor(Ethnicity)+factor(Student)+
                           Income:Rating+ Income:Age+ Income:Education+ Income:factor(Gender)+
                           Income:factor(Ethnicity)+Income:factor(Student)+Limit:Rating+
                           Limit:factor(Student)+Cards:factor(Gender), data=credit)
summary(model_inter_refine2)

out1<-CVlm(data=credit, m=mk, seed=20230525, 
           form.lm = formula(Balance ~ Income+Limit+Rating+Cards+Age+Education
                             +factor(Gender)+factor(Ethnicity)+factor(Student)+
                               Income:Rating+ Income:Age+ Income:Education+ Income:factor(Gender)+
                               Income:factor(Ethnicity)+Income:factor(Student)+Limit:Rating+
                               Limit:factor(Student)+Cards:factor(Gender)))

model4=lm(formula = Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
          +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student),data=credit)
summary(model4)

out2<-CVlm(data=credit, m=mk, seed=20230525, 
           form.lm = formula(Balance ~ Income+Limit+Rating+Cards+Age+factor(Student)
                             +Income*Rating+Income*factor(Student)+Limit*Rating+Limit*factor(Student)))

error1=sqrt(mean((out1$Predicted-out1$Balance)^2))
cv_error1<-sqrt(mean((out1$cvpred-out1$Balance)^2))

error2=sqrt(mean((out2$Predicted-out2$Balance)^2))
cv_error2<-sqrt(mean((out2$cvpred-out2$Balance)^2))

print(paste(error1, error2))
print(paste(cv_error1, cv_error2))
