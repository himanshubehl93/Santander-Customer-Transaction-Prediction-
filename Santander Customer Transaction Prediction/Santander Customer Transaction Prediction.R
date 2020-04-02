rm(list = ls())
setwd('C:/Users/hp/Desktop/assignment')
#load libraries
library (ggplot2)
library(tidyverse)
library(moments)
library(DataExplorer)
library(caret)
library(Matrix)
library(pdp)
library(mlbench)
library(caTools)
library(randomForest)
library(glmnet)
library(mlr)
library(vita)
library(rBayesianOptimization)
library(lightgbm)
library(pROC)
library(DMwR)
library(ROSE)
library(yardstick)
#load the train data
train_df<-read.csv('C:/Users/hp/Desktop/assignment/train.csv')
head(train_df)
dim(train_df)
str(train_df)
test_df<-read.csv('C:/Users/hp/Desktop/assignment/test.csv')
head(test_df)
dim(test_df)
str(test_df)
#Confusion matrix
set.seed(689)
#actual target variable
target<-valid.data$target
#convert to factor
target<-as.factor(target)
#predicted target variable
#convert to factor
cv_predict.lr<-as.factor(cv_predict.lr)
confusionMatrix(data=cv_predict.lr,reference=target)
#convert to factor
train_df$target <-as.factor(train_df$target)
#Count of target classes
table(train_df$target)
#Percenatge counts of target classes
table(train_df$target)/length(train_df$target)*100
#Bar plot for count of target classes
plot1<-ggplot(train_df,aes(target))+theme_bw()+geom_bar(stat='count',fill='lightgreen')
plot1
#Distribution of train attributes from 3 to 10
for (var in names(train_df)[c(3:10)]){
  target<-train_df$target
  plot<-ggplot(train_df, aes(x=train_df[[var]],fill=target)) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}
#Split the training data using simple random sampling
train_index<-sample(1:nrow(train_df),0.8*nrow(train_df))
#train data
train_data<-train_df[train_index,]
#validation data
valid_data<-train_df[-train_index,]
#dimension of train and validation data
dim(train_data)
dim(valid_data)

#Training the Random forest classifier
set.seed(1234)
#convert to int to factor
train_data$target<-as.factor(train_data$target)
#setting the mtry
mtry<-floor(sqrt(200))
#setting the tunegrid
tuneGrid<-expand.grid(.mtry=mtry)
#fitting the ranndom forest
rf<-randomForest(target~.,train_data[,-c(1)],mtry=mtry,ntree=10,importance=TRUE)
#Variable importance
VarImp<-importance(rf,type=2)
VarImp
#Split the data using CreateDataPartition
set.seed(789)
#train.index<-createDataPartition(train_df$target,p=0.8,list=FALSE)
train.index<-sample(1:nrow(train_df),0.8*nrow(train_df))
#train data
train.data<-train_df[train.index,]
#validation data
valid.data<-train_df[-train.index,]
#dimension of train data
dim(train.data)
#dimension of validation data
dim(valid.data)
#target classes in train data
table(train.data$target)
#target classes in validation data
table(valid.data$target)
# training set
X_t<-as.matrix(train.data[,-c(1,2)])
y_t<-as.matrix(train.data$target)
#validation dataset
X_v<-as.matrix(valid.data[,-c(1,2)])
y_v<-as.matrix(valid.data$target)
#test dataset
test<-as.matrix(test_df[,-c(1)])
#Logistic regression model
set.seed(777) # to reproduce results
lr_model <-glmnet(X_t,y_t, family = "binomial")
summary(lr_model)
#Model performance on validation dataset
set.seed(3578)
cv_predict.lr<-predict(cv_lr,X_v,s = "lambda.min", type = "class")
cv_predict.lr
#Confusion matrix
set.seed(789)
#actual target variable
target<-valid.data$target
#convert to factor
target<-as.factor(target)
#predicted target variable
#convert to factor
cv_predict.lr<-as.factor(cv_predict.lr)
confusionMatrix(data=cv_predict.lr,reference=target)
library(e1071)
NB_model=naivBayes(target~.,data=train)
#predict on test cases
NB_Predictions=predict(NB_model,test[,2:220])
conf_matrix=table(observed = test[,1],predicted=NB_Predictions)
confusionMatrix(conf_matrix)
mean(NB_Predictions==test$target)
#Accuray=91.37777
#fN rate=47.53
