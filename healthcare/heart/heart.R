rm(list=ls(all=TRUE))

install.packages("dplyr")
library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library("dplyr")
library(randomForest)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/healthcare/heart")


#load data##

data<-read.csv(file="heart.csv",header=T,sep=",")
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
 aes(x = ï..age, y = oldpeak, colour = fbs, size = chol) +
 geom_point() +
 scale_color_viridis_c(option = "plasma") +
 theme_minimal()


#featur selection#



library(earth)
marsModel <- earth(target ~ ., data=data) # build model
ev <- evimp(marsModel)
plot(ev)


#as model shows bp is minor factor,but the priliminary diagnosis strts from bp check up so we cant avoid it##

data=data[c(2:6,10:14)]

data
data$target=as.factor(data$target)
table(data$target)


#splitdata

set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




#buildmodel#


library(ada)

model = ada(target~ ., iter = 20,data = train, loss="logistic")

pred = predict(model, test);

pred

confusionMatrix(test$target,pred,positive = "1")

#81%#


#randomforest


library(randomForest)

model_rf <- randomForest(target~ ., data= train, ntree=10,mtry = 5)

pred1=predict(model_rf,test)

confusionMatrix(test$target,pred1,positive = "1")

#76%#