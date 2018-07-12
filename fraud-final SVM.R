#setwd("~/Desktop/DS0562-Fraud Analytics")

library(readr)
library(dplyr)
library(caret)
library(e1071)

train=read_csv('training_data.csv')
test=read_csv('testing_data.csv')
oot=read_csv('oot_data.csv')
oot=oot[,-1]
train$Fraud=as.factor(train$Fraud)
test$Fraud=as.factor(test$Fraud)
oot$Fraud=as.factor(oot$Fraud)
train=as.data.frame(train)
test=as.data.frame(test)
oot=as.data.frame(oot)


## Default SVM

train_svm<-svm(Fraud~.,data=train,scale=TRUE,probability = TRUE) 

train_svm$decision.values=as.data.frame(train_svm$decision.values)
train_decision=train_svm$decision.values
train=cbind(train,train_decision$`0/1`)
colnames(train)[65]='prob'

train%>%
  arrange(prob)%>%
  slice(1:1340)%>%
  filter(Fraud==1)%>%
  summarise(caught=n())   #485, which is good, next will tune the SVM

FDR_2percent_train=485/541 # 89.6%


test_svm=predict(train_svm,test[,-1],probability = TRUE)
prob=attributes(test_svm)$probabilities[,2]
test$prob=prob


test%>%
  arrange(-prob)%>%
  slice(1:587)%>%
  filter(Fraud==1)%>%
  summarise(caught=n())  #406

FDR_2percent_test=406/473 #85.8%

big_train=rbind(train[,-65],test[-65]) # combine training and testing

big_svm<-svm(Fraud~.,data=big_train,scale=TRUE,probability = TRUE)  ## retrain on the big dataset

oot_svm=predict(big_svm,oot[,-1],probability = TRUE)

oot$prob=attributes(oot_svm)$probabilities[,2]


oot%>%
  arrange(-prob)%>%
  slice(1:252)%>%
  filter(Fraud==1)%>%
  summarise(caught=n()) ## 243

FDR_2percent_oot=243/338  ## 71.8%


sorted_oot=oot%>%
  arrange(-prob)%>%
  slice(1:252)


plot_svm=svm(Fraud~.,data=big_train,scale=TRUE) 

plot(plot_svm, sorted_oot, same_card_3 ~ same_card_1)


data(cats, package="MASS")
inputData <- data.frame(cats[, c (2,3)], response = as.factor(cats$Sex))

svmfit<-svm(response~.,data=inputData,kernel="linear",cost=10,scale=FALSE)


plot(svmfit,inputData)