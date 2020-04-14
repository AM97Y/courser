#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 14 20:03:38 2020

@author: averina
"""

setwd("D:/Downloads/R")
url_train="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url_train,destfile="training_data.csv")
download.file(url_test,destfile="testing_data.csv")
library(readr)
training=read.csv("D:/Downloads/R/training_data.csv", na.strings = c("NA", "#DIV/0!", "")) 
testing = testing[ which(colSums(na(testing)) == 0)]
training = training[c[1:7]] ##the first 7 columns are variables that has no relationship with "class"
testing = testing[c[1:7]]
library(caret)

set.seed(888)
training = data.frame(training)
inTrain = createDataPartition(training, classe, p=0.70, list=F)
train = training[inTrain, ]
validation = training[-inTrain, ]

fit1 = train(classe , method="rpart", data=train)
val1 = predict(fit1, validation)
confusionMatrix(validation, classe, val1)



fit2 = train(classe , method="rf", data=train, prox=TRUE,ntree=250)
val2 = predict(fit2, validation)
confusionMatrix(validation, classe, val2)

fit3 = train(classe , method="gbm", data=train,trControl=trainControl(method = "repeatedcv", number = 5, repeats = 1),verbose=FALSE)
val3 = predict(fit3, validation)
confusionMatrix(validation, classe, val3)

pred = predict(fit2, newdata=testing)
print(pred)
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
