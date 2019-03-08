#Loading the Library 
library(caret)
library(ISLR)
library(pROC)
library(MASS)
library(caret)

#Load Data
data(Default)

summary(Default)

#fitting a Logistic Regression model 
set.seed(122)
logit <- glm(default ~ income + balance,family = "binomial",
             data= Default )
summary(logit)

#Prdicting the results 
results <- predict(logit,Default, type="response")
prob<-ifelse(results>0.5,"Yes","No")
round(mean(Default$default!=prob),4)

#creating training and test set
set.seed(199)
trainIndex <- createDataPartition(Default$default, p=.7, list=F)

d.train <- Default[trainIndex,]
d.test <- Default[-trainIndex,]

#Fitting multiple logistic regression on training set
logit.1 <- glm(default ~ income + balance,  family = "binomial",
               data= d.train)
summary(logit.1)

#Predicting using test set
results.1 <- predict(logit.1,d.test, type="response")
prob.1<-ifelse(results.1>0.5,"Yes","No")

table(d.test$default,prob.1)

round(mean(prob.1!=d.test$default),4)

#Creating split 80:20
set.seed(215)
trainIndex <- createDataPartition(Default$default, p=.8, list=F)

d.train.1 <- Default[trainIndex,]
d.test.1 <- Default[-trainIndex,]

#Fitting multiple logistic regression on training set
logit.2 <- glm(default ~ income + balance,  family = "binomial",
               data= d.train.1)
summary(logit.2)

#Predicting using test set
results.2 <- predict(logit.2,d.test.1, type="response")
prob.2<-ifelse(results.2>0.5,"Yes","No")

table(d.test.1$default,prob.2)

round(mean(prob.2!=d.test.1$default),4)

#Creating split 60:40
set.seed(220)
trainIndex <- createDataPartition(Default$default, p=.6, list=F)

d.train.2 <- Default[trainIndex,]
d.test.2 <- Default[-trainIndex,]

#Fitting multiple logistic regression on training set
logit.3 <- glm(default ~ income + balance,  family = "binomial",
               data= d.train.2)
summary(logit.3)

#Predicting using test set
results.3 <- predict(logit.3,d.test.2, type="response")
prob.3<-ifelse(results.3>0.5,"Yes","No")

table(d.test.2$default,prob.3)

round(mean(prob.3!=d.test.2$default),4)

#Creating split 90:10
set.seed(250)
trainIndex <- createDataPartition(Default$default, p=.9, list=F)

d.train.3 <- Default[trainIndex,]
d.test.3 <- Default[-trainIndex,]

#Fitting multiple logistic regression on training set
logit.4 <- glm(default ~ income + balance,  family = "binomial",
               data= d.train.3)
summary(logit.4)

#Predicting using test set
results.4 <- predict(logit.4,d.test.3, type="response")
prob.4<-ifelse(results.4>0.5,"Yes","No")

table(d.test.3$default,prob.4)

round(mean(prob.4!=d.test.3$default),4)

#Using student as the predictor split 80:20
set.seed(260)
trainIndex <- createDataPartition(Default$default, p=.8, list=F)

d.train.s <- Default[trainIndex,]
d.test.s <- Default[-trainIndex,]

#Fitting multiple logistic regression on training set
logit.s <- glm(default ~ income + balance + student,  family = "binomial",
               data= d.train.s)
summary(logit.s)

#Predicting using test set
results.s <- predict(logit.s,d.test.s, type="response")
prob.s<-ifelse(results.s>0.5,"Yes","No")

table(d.test.s$default,prob.s)

round(mean(prob.s!=d.test.s$default),4)
