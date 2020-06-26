rm(list=ls(all=TRUE))


library(tidyverse)
library(caret)
library(rpart)
library(DMwR)
library(randomForest)


###set working directory###
setwd("C:/Users/User/Desktop/data science project/healthcare/life expectancy")


#reading
#Loading data
data<-read.csv(file="Life Expectancy Data.csv",header=T,sep=",")

dim(data)
str(data)
summary(data)

#checking missin vaalue##
sum(is.na(data))      

data=na.omit(data)


##########Exploratory data analysis###########
library(DataExplorer)
create_report(data)


remove.packages("esquisse")
install.packages("esquisse")
library(esquisse)
esquisse::esquisser(data)

library(ggplot2)

ggplot(data) +
 aes(x = Life.expectancy, y = BMI) +
 geom_point(size = 1L, colour = "#0c4c8a") +
 theme_minimal()

ggplot(data) +
 aes(x = Country, weight = BMI) +
 geom_bar(fill = "#0c4c8a") +
 theme_minimal()

ggplot(data) +
 aes(x = Country, weight = Life.expectancy) +
 geom_bar(fill = "#f0f921") +
 theme_minimal()

ggplot(data) +
 aes(x = Life.expectancy, y = GDP, colour = Country, group = Year) +
 geom_point(size = 1L) +
 scale_color_hue() +
 theme_minimal()



boxplot(data$Life.expectancy)



#featureselection##
library(earth)
marsModel <- earth(Life.expectancy ~ ., data=data) # build model
ev <- evimp (marsModel)
plot(ev)

#preprocessing##
data$Country=as.factor(data$Country)
data$Status=as.factor(data$Status)


#data partiotioin###
set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test



#build  model###


lm_model <- lm(Life.expectancy~ ., data= train)
plot(lm_model)

pred1=predict(lm_model,test)
regr.eval(train$Life.expectancy,pred1)

#mae         mse        rmse        mape 
#9.6887376 154.4176072  12.4264881   0.1473918 




library(MASS)
stepAIC(lm_model, direction = "both")


#new model#

data=data[,-c(1,2,5,6,12,17,20,21,22)]
data



#data partiotioin###
set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




model <- lm(Life.expectancy~ ., data= train)
plot(model)

pred1=predict(model,test)
regr.eval(train$Life.expectancy,pred1)

#mae         mse        rmse        mape 
#8.6844988 126.4186511  11.2436049   0.1326538 


library(MASS)
stepAIC(model, direction = "both")






#for random forest#

data$Country=NULL
data$Status=NULL

library(randomForest)
model_rf <- randomForest(Life.expectancy~ ., data= train, ntree=10,mtry = 5)
plot(model_rf)
print(model_rf)
pred1=predict(model_rf,test)
regr.eval(train$Life.expectancy,pred1)

library(rpart)
model_rpart<-rpart(Life.expectancy~.,data=train,method="anova",control =rpart.control(cp = 0.001))
plot(model_rpart)
print(model_rpart)
pred1=predict(model_rpart,test)
regr.eval(train$Life.expectancy,pred1)




data$Year=NULL



data=data[,c(1:4,8,9,11,13,17,18)]


set.seed(214)
crows <- sample(x = 1:nrow(data), size = 0.75*nrow(data))
train<- data[crows, ]
train
test <- data[-crows, ]
test




model4 <- lm(Life.expectancy~ ., data= train)
plot(model4)

pred1=predict(model4,test)
regr.eval(train$Life.expectancy,pred1)

library(MASS)
stepAIC(model, direction = "both")
