data <- read.csv(file = 'parkinsons.data')

data$status <- as.factor(data$status)
na = colSums(is.na(data))
na

data.m=melt(data,id.vars = "status")
 d <- ggplot(data = data.m, aes(x=variable, y=value))+  
    geom_boxplot(aes(fill=status))
 d + facet_wrap( ~ variable, scales="free")

features <- subset(data,select = -status)
labels <- subset(data,select= status)
data.scaled <- as.data.frame(scale(features))
data.scaled_1 <- cbind(data.scaled,labels)
library(ROCR)
set.seed(100) 
#100 is used to control the sampling permutation to 100. 
index <-sample(nrow(data),0.75*nrow(data))
train<-data[index,]
test<- data[-index,]
modelblr<-glm(status~.,data = train,family = "binomial")

train$pred<-fitted(modelblr)
pred <-prediction(train$pred,train$status)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize = T,print.cutoffs.at = seq(0.1,by = 0.1))

train$pred1<-ifelse(train$pred<0.8,1,0)
 
library(caret)

train$pred1 <- as.factor(train$pred1)
 train$status <- as.factor(train$status)
 confusionMatrix(factor(train$pred1),train$status)
 
 test$pred<-predict(modelblr,test,type = "response")
 # type = "response" is used to get the outcome in the form of probability of having parkinson diseases.
 head(test)
 test$pred1 <- as.factor(test$pred1)
 test$status <- as.factor(test$status)
 test$pred1<-ifelse(test$pred<0.8,0,1)
 confusionMatrix(factor(test$pred1),test$status)
 
 auc<-performance(pred,"auc")
 auc@y.values