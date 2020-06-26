rm(list=ls(all=TRUE))

library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)
library(dplyr)

###set working directory###
setwd("C:/Users/User/Desktop/data science project/healthcare/kidney")


#load data##

data<-read.csv(file="kidney_disease.csv",header=T,sep=",")
dim(data)
str(data)
summary(data)


table(data$classification)
data$pcv=as.numeric(data$pcv)
data$dm=as.numeric(data$dm)

data3=c(rbc,pc,pcc,ba,wc,rc,htn,cad,appet,pe)




#checking missin vaalue##
sum(is.na(data))
#470#

#delete na rows#
data=na.omit(data)

##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)

#html file created#

install.packages("Rcpp")
library(esquisse)
esquisse::esquisser(data)


data1=filter(data,classification=="ckd" )
data2=filter(data,classification=="notckd")
data=rbind(data1,data2)


library(dplyr)
library(ggplot2)

data %>%
 filter(!(rbc %in% "")) %>%
 filter(!(pc %in% "")) %>%
 filter(!(pcc %in% "")) %>%
 
    filter(!(ba %in% "")) %>%
 filter(!(pcv %in% "")) %>%
 filter(!(rc %in% "")) %>%
 
    filter(!(htn %in% "")) %>%
 filter(!(dm %in% "")) %>%
 filter(!(cad %in% "")) %>%
 
    filter(!(appet %in% "")) %>%
 filter(!(pe %in% "")) %>%
 filter(!(ane %in% "")) %>%
 ggplot() +
 aes(x = age, fill = pc) +
 geom_histogram(bins = 30L) +
 scale_fill_hue() +
 theme_minimal()

data %>%
 filter(!(rbc %in% "")) %>%
 filter(!(pc %in% "")) %>%
 filter(!(pcc %in% "")) %>%
 
    filter(!(ba %in% "")) %>%
 filter(!(pcv %in% "")) %>%
 filter(!(rc %in% "")) %>%
 
    filter(!(htn %in% "")) %>%
 filter(!(dm %in% "")) %>%
 filter(!(cad %in% "")) %>%
 
    filter(!(appet %in% "")) %>%
 filter(!(pe %in% "")) %>%
 filter(!(ane %in% "")) %>%
 ggplot() +
 aes(x = pc, y = age, fill = pc) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()


#featur selection#

library(earth)
marsModel <- earth(classification ~ ., data=data) # build model
ev <- evimp(marsModel)
plot(ev)

#preprocessing#
data$id=NULL
data<-centralImputation(data)


set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




#buildmodel#

library(caret)

library(ada)

model = ada(classification~ ., iter = 20,data = train, loss="logistic")

pred = predict(model, test);

pred

confusionMatrix(test$classification,pred)

#76%#


#randomforest


library(randomForest)

model_rf <- glm(classification~ ., data= train)

pred1=predict(model_rf,test)

confusionMatrix(test$classification,pred1)

#73%#
