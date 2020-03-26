#Student Performance
#read csv, note the delimeter 
df<-read.csv('C:/Users/91727/Downloads/student/student-mat.csv', sep = ';')
head(df)

summary(df)
str(df)

#checking presence of missing value
any(is.na(df))

#packages required for exploratory data analysis
install.packages('ggthemes')
library(ggplot2)
library(ggthemes)
library(dplyr)
install.packages('corrgram')
install.packages('corrplot')
library(corrgram)
library(corrplot)
library(caTools)    #for split function
library(ggplot2)

#correlation
#grab only numeric columns
num.cols<-sapply(df, is.numeric)
num.cols
#filter to numeric columns for correlation
cor.data<-cor(df[,num.cols])
cor.data

corrplot(cor.data, method = 'color')   #expected to give only numeric data

corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)  #can be done for the entire dataset, not mandatory to do

#analysing g3 (the one we require)

ggplot(df,aes(x=G3)) + geom_histogram(bins = 20,alpha=1,fill='blue') + theme_minimal()

# absent or not really performing value, mean value

#building multiple linear regression model
sample<-sample.split(df$G3, SplitRatio = 0.70)  #splitting data into training and testing set randomly
#training data
train <- subset(df, sample==TRUE)
#testing data
test <- subset(df, sample==FALSE)
#training model
model<-lm(G3 ~ ., train)
summary(model)

res <- residuals(model)
res<-as.data.frame(res)
head(res)

G3.predictions<-predict(model,test)
results<-cbind(G3.predictions,test$G3)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
head(results)

to_zero<-function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
results$pred<-sapply(results$pred,to_zero)
head(results)

#finding parameters
mse<-mean((results$real-results$pred)^2)
mse
rmse<-mse^0.5
rmse
SSE<-sum((results$pred-results$real)^2)
SSE
TSS<-sum((mean(df$G3)-results$real)^2)
TSS
R2<-1-SSE/TSS
R2 

#simple linear regression
#G1
model<-lm(G3 ~ G1, train)
summary(model)
res <- residuals(model)
res<-as.data.frame(res)
head(res)

G3.G1.predictions<-predict(model,test)
results<-cbind(G3.G1.predictions,test$G3)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
head(results)

to_zero<-function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
results$pred<-sapply(results$pred,to_zero)
head(results)

#finding parameters
mse<-mean((results$real-results$pred)^2)
mse
rmse<-mse^0.5
rmse
SSE<-sum((results$pred-results$real)^2)
SSE
TSS<-sum((mean(df$G3)-results$real)^2)
TSS
R2<-1-SSE/TSS
R2 

#G2
model<-lm(G3 ~ G2, train)
summary(model)
res <- residuals(model)
res<-as.data.frame(res)
head(res)

G3.G2.predictions<-predict(model,test)
results<-cbind(G3.G2.predictions,test$G3)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
head(results)

to_zero<-function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
results$pred<-sapply(results$pred,to_zero)
head(results)

#finding parameters
mse<-mean((results$real-results$pred)^2)
mse
rmse<-mse^0.5
rmse
SSE<-sum((results$pred-results$real)^2)
SSE
TSS<-sum((mean(df$G3)-results$real)^2)
TSS
R2<-1-SSE/TSS
R2 

#G1+G2
model<-lm(G3 ~ G1+G2, train)
summary(model)
res <- residuals(model)
res<-as.data.frame(res)
head(res)

G3.G1.G2.predictions<-predict(model,test)
results<-cbind(G3.G2.predictions,test$G3)
colnames(results)<-c('pred','real')
results<-as.data.frame(results)
head(results)

to_zero<-function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
results$pred<-sapply(results$pred,to_zero)
head(results)

#finding parameters
mse<-mean((results$real-results$pred)^2)
mse
rmse<-mse^0.5
rmse
SSE<-sum((results$pred-results$real)^2)
SSE
TSS<-sum((mean(df$G3)-results$real)^2)
TSS
R2<-1-SSE/TSS
R2 
