rm(list=ls(all=TRUE))

install.packages("tidyverse")
library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(dplyr)
library(randomForest)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/healthcare/lungs")


#load data##

data<-read.csv(file="lung_cancer_examples.csv",header=T,sep=",")
dim(data)
str(data)
summary(data)


#drop name & surname#

data=data[,3:7]


#checking missin vaalue##
sum(is.na(data))
#0#



##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)

#html file created#


library(esquisse)
esquisse::esquisser(data)

library(ggplot2)

ggplot(data) +
 aes(x = Alkhol, y = Smokes, colour = Result, size = Result, group = Age) +
 geom_point() +
 scale_color_gradient() +
 theme_minimal()


#featur selection#
install.packages("Boruta")
library(Boruta)
oruta<-Boruta(Result~.,data<-data,doTrace=2)


library(earth)
marsModel <- earth(Result ~ ., data=data) # build model
ev <- evimp(marsModel)
plot(ev)

#alcohol is most significant attribute##

data$Result=as.factor(data$Result)
table(data$Result)


#splitdata

set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test


#build model and testing accuracy#
library(ada)

model = ada(Result~ ., iter = 20,data = train, loss="logistic")
pred = predict(model, test);
pred
confusionMatrix(test$Result,pred)

#93%#


#randomforest


library(randomForest)

model_rf <- randomForest(Result~ ., data= train, ntree=10,mtry = 5)
pred1=predict(model_rf,test)
confusionMatrix(test$Result,pred1,positive = "1")

#93%#

saveRDS(model_rf,"rfheart-model.rds")


