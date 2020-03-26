data<-read.csv('D:/R/insurance.csv')
head(data)

any(is.na(data))

summary(data)
str(data)

num.cols<-sapply(data, is.numeric)
num.cols
cor.data<-cor(data[,num.cols])
cor.data

library(corrgram)
library(corrplot)
library(dplyr)
library(ggplot2)
library(caTools)

corrplot(cor.data, method = 'color')

ggplot(data, aes(x=charges)) + geom_histogram(bins = 20, alpha = 0.8, fill = 'yellow')

sample<-sample.split(data$charges, SplitRatio = 0.70)
train<-subset(data, sample==TRUE)
test<-subset(data, sample==FALSE)

#multiple linear regression model
model<-lm(charges ~.,train)
summary(model)

residuals<-residuals(model)
res<-as.data.frame(residuals)
head(res)

predict<-predict(model,test)
results<-cbind(predict,test$charges)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
head(results)

mse<-mean((results$real-results$pred)^2)
mse

rmse<-mse^0.5
rmse

sse<-sum((results$pred-results$real)^2)
sse

tss<-sum((data$charges - mean(results$real))^2)
tss

R2<-(1-sse/tss)
R2

#simple linear regression model
s.model<-lm(charges ~age,train)
summary(s.model)

residuals<-residuals(s.model)
res<-as.data.frame(residuals)
head(res)

predict<-predict(s.model,test)
results<-cbind(predict,test$charges)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
head(results)

mse<-mean((results$real-results$pred)^2)
mse

rmse<-mse^0.5
rmse

sse<-sum((results$pred-results$real)^2)
sse

tss<-sum((data$charges - mean(results$real))^2)
tss

R2<-(1-sse/tss)
R2
