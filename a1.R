data<-read.csv('D:/R/Real_estate.csv')
head(data)

any(is.na(data))

summary(data)
str(data)

num.cols<-sapply(data, is.numeric)
num.cols

cor.data<-cor(data)
cor.data

library(corrgram)
library(corrplot)
library(dplyr)
library(ggplot2)
library(caTools)

corrplot(cor.data, method = 'color')

corrgram(data, order = TRUE, panel = panel.shade, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

ggplot(data, aes(x=Y.house.price.of.unit.area)) + geom_histogram(bins = 20, alpha = 0.8, fill = 'yellow')

sample<-sample.split(data$Y.house.price.of.unit.area, SplitRatio = 0.70)
train<-subset(data, sample==TRUE)
test<-subset(data, sample==FALSE)

#multiple linear regression model
model<-lm(Y.house.price.of.unit.area ~X3.distance.to.the.nearest.MRT.station+X4.number.of.convenience.stores+X5.latitude+X6.longitude,train)
summary(model)

residuals<-residuals(model)
res<-as.data.frame(residuals)
head(res)

predict<-predict(model,test)
results<-cbind(predict,test$Y.house.price.of.unit.area)
colnames(results)<-c('Y.pred','Y.real')
results<-as.data.frame(results)
head(results)

mse<-mean((results$Y.real-results$Y.pred)^2)
mse

rmse<-mse^0.5
rmse

sse<-sum((results$Y.pred-results$Y.real)^2)
sse

tss<-sum((data$Y.house.price.of.unit.area - mean(data$Y.house.price.of.unit.area))^2)
tss

R2<-(1-sse/tss)
R2

#simple linear regression model
#no. of convenience stores
s.model<-lm(Y.house.price.of.unit.area ~X4.number.of.convenience.stores,train)
summary(s.model)

residuals<-residuals(s.model)
res<-as.data.frame(residuals)
head(res)

predict<-predict(s.model,test)
results<-cbind(predict,test$Y.house.price.of.unit.area)
colnames(results)<-c('Y.pred','Y.real')
results<-as.data.frame(results)
head(results)

mse<-mean((results$Y.real-results$Y.pred)^2)
mse

rmse<-mse^0.5
rmse

sse<-sum((results$Y.pred-results$Y.real)^2)
sse

tss<-sum((data$Y.house.price.of.unit.area - mean(data$Y.house.price.of.unit.area))^2)
tss

R2<-(1-sse/tss)
R2

#latitude
s2.model<-lm(Y.house.price.of.unit.area ~X5.latitude,train)
summary(s2.model)

residuals<-residuals(s2.model)
res<-as.data.frame(residuals)
head(res)

predict<-predict(s2.model,test)
results<-cbind(predict,test$Y.house.price.of.unit.area)
colnames(results)<-c('Y.pred','Y.real')
results<-as.data.frame(results)
head(results)

mse<-mean((results$Y.real-results$Y.pred)^2)
mse

rmse<-mse^0.5
rmse

sse<-sum((results$Y.pred-results$Y.real)^2)
sse

tss<-sum((data$Y.house.price.of.unit.area - mean(data$Y.house.price.of.unit.area))^2)
tss

R2<-(1-sse/tss)
R2

#longitude
s3.model<-lm(Y.house.price.of.unit.area ~X6.longitude,train)
summary(s3.model)

residuals<-residuals(s3.model)
res<-as.data.frame(residuals)
head(res)

predict<-predict(s3.model,test)
results<-cbind(predict,test$Y.house.price.of.unit.area)
colnames(results)<-c('Y.pred','Y.real')
results<-as.data.frame(results)
head(results)

mse<-mean((results$Y.real-results$Y.pred)^2)
mse

rmse<-mse^0.5
rmse

sse<-sum((results$Y.pred-results$Y.real)^2)
sse

tss<-sum((data$Y.house.price.of.unit.area - mean(data$Y.house.price.of.unit.area))^2)
tss

R2<-(1-sse/tss)
R2
