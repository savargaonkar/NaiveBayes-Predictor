### Project: Predict Product Subscription
library(tidyverse)
library(e1071)
library(reshape2)

# Set Seed for reproductible results
set.seed(100)

#Read, Load, Split Data
data<-read.csv("../data.csv")
train_i<- sample(1:nrow(data), (2/3)*nrow(data))
train<- data[train_i, ]
test<- data[-train_i, ]

#Data overview
str(train)
summary(train)
table(complete.cases(train))

#Pre-processing: Removing NA's - not a large number of observations
na_index<-which(is.na(train$inc_cat))
train_nr<- train
train_nr<-train[-na_index, ]
summary(train_nr)
test<-na.omit(test) # Alt. approach to removing all instances with NA

#Visualize data
hist(train_nr$weekly_use, main="Weekly Usage (Hrs.)", ylab="Hrs")
plot(train_nr$age, train_nr$income_level, xlab="Age", main="Income by age")
plot(train_nr$education, train_nr$income_level, xlab="Age", main="Income by education")
plot(train_nr$product, train_nr$income_level, xlab="Product", main="Product subscribed to by income levels")

#Clean data
train_nr<- train_nr[which(train_nr$age > 100), ]
train_nr<- train_nr[which(train_nr$weekly_use <= 0), ]
train_nr<- train_nr[which(train_nr$product == "Unknown"), ]

#Additional visualizations
ggplot(train_nr, mapping=aes(occupation, fill=income_level))+geom_bar()+ggtitle("Income by occupation")
ggplot(train_nr, mapping=aes(sex, product))+geom_point()+ggtitle("Products use by gender")
ggplot(train_nr, mapping=aes(weekly_usage, fill=product))+geom_bar()+ggtitle("Weekly usage by product")

#Model: Naive Bayes Classification
model<-naiveBayes(subscriptionstat ~ age + income_level + education + sex + weekly_usage, data = train_nr)
predict_train<-predict(model,train[,-subscriptionstat])
predict_test<-predict(model,test[,-subscriptionstat])
cm_train<-table(predict_train, train_nr$subscriptionstat)
cm_test<-table(predict_test, test$subscriptionstat)

#Test: Model accuracy
cm_train
sum(cm_train[1,2],cm_train[2,1])/sum(cm_train)
cm_test
sum(cm_test[1,2],cm_test[2,1])/sum(cm_test)