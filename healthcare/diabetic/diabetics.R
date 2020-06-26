rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library("dplyr")
library(randomForest)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/healthcare/diabetic")


#load data##

data<-read.csv(file="diabetes.csv",header=T,sep=",")
dim(data)
str(data)
summary(data)




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
 aes(x = Glucose, y = BloodPressure, colour = Outcome, size = Age) +
 geom_point() +
 scale_color_viridis_c(option = "plasma") +
 theme_minimal()

#featur selection#

library(earth)
marsModel <- earth(Outcome ~ ., data=data) # build model
ev <- evimp(marsModel)
plot(ev)

###dropiing insignificant variable##

data=data[c(2:3,6:9)]

#preprocessing#

data
data$Outcome=as.factor(data$Outcome)
table(data$Outcome)


#splitdata

set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




#buildmodel#

library(caret)
library(ada)

model = ada(Outcome~ ., iter = 20,data = train, loss="logistic")

pred = predict(model, test)

pred

confusionMatrix(test$Outcome,pred,positive = "1")

#76%#


#randomforest


library(randomForest)

model_rf <- randomForest(Outcome~ ., data= train, ntree=10,mtry = 5)

pred1=predict(model_rf,test)

confusionMatrix(test$Outcome,pred1,positive = "1")

#73%#