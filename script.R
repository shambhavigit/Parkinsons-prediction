data <- read.csv(file = 'parkinsons.data')
             
features <- subset(data,select = -status)
labels <- subset(data,select= status)
data.scaled <- as.data.frame(scale(features))
data.scaled_1 <- cbind(data.scaled,labels)


# choosing 80% of the data to be the training data

library(caTools)
split_index<- sample.split(data.scaled_1,SplitRatio = 0.80)

split_index_1<- sample.split(data,SplitRatio = 0.80)

# 80% of the observations from price column have been assigned the “true” label and the rest 35% have been assigned “false” label.

subset(data.scaled_1,split_index==T)->train
subset(data.scaled_1,split_index==F)->test

subset(data,split_index_1==T)->train
subset(data,split_index_1==F)->test


rpart(status~.,data = train)-> mod1
plot(mod1,margin = 0.1)text(mod1,pretty = T,cex=0.8)
predict(mod1,test,type = "class")->result1
confusionMatrix(table(test$status,result1))                     
#buildind model
 
glm(status~.,data = train)-> model_regress
 
#predict model
 prediction(model_regress,test)-> result_regress1

 result <- ifelse(predict(model_regress, type="response")>.5, 1, 0)
 table(result,test)

#Let’s bind the actual status values from the “test” data-set and the predicted values into a single data-set using the “cbind()” function.
#The new data-frame is stored in “Final_Data”

Final_Data <-  cbind(Actual=test$status,Predicted=result_regress)
Final_Data<- as.data.frame(Final_Data)

#Let’s find the error by subtracting the predicted values from the actual values and add this error as a new column to the “Final_Data”:
  
Error<- (Final_Data$Actual- Final_Data$Predicted)
Final_Data <- cbind(Final_Data,Error)

# Now, we’ll go ahead and calculate “Root Mean Square Error” which gives an aggregate error for all the predictions

rmse1<-sqrt(mean(Final_Data$Error^2)) 
rmse1

matrix<- table(test$status,result_regress)
library(caret)

u <- union(result_regress, test$status)
t <- table(factor(result_regress, u), factor(test$status, u))
confusionMatrix(t)

#Random Forest

data_set_size <- floor(nrow(data)/2)
library(randomForest)
rf_classifier = randomForest(labels ~ ., data=train, ntree=100, mtry=2, importance=TRUE,proximity=TRUE)
rf_classifier

library(ggplot2)
importance(rf_classifier)
varImpPlot(rf_classifier)
pred<-predict(rf_classifier,test,type='response')
summary(pred)
summary(data.scaled_1$status)
MAE <- function(actual, predicted) {
  +     mean(abs(actual - predicted))  
  + }
cor(pred,test$status)

 MAE(pred, test$status)
 rf1<- randomForest(status~ ., data =train,mtry=10)
 rf1
 importance(rf1)
 varImpPlot(rf1)
 pred1<-predict(rf1,test,type='response')
 head(pred1)
 
 data.imputed <- rfImpute(status ~ ., data = train, iter=6)
 model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
 
 data.imputed <- rfImpute(hd ~ ., data = data, iter=20)
 
 mytable <- table(data.scaled$status)
 > pct<-round(mytable/sum(mytable)*100)
 > lbls1<-paste(names(mytable),pct)
 > lbls<-paste(lbls1, "%", sep="")
 > pie(mytable, labels = lbls,col = rainbow(length(lbls)),main="Pie Chart",radius = 0.9)
 > 
   set.seed(100) 
  #100 is used to control the sampling permutation to 100. 
  index <-sample(nrow(data),0.75*nrow(data))
 train<-data[index,]
  test<- data[-index,]
  modelblr<-glm(status~.,data = train,family = "binomial")
 
 train$pred<-fitted(modelblr)
 pred<-prediction(train$pred,train$status)
 perf<-performance(pred,"tpr","fpr")
 plot(perf,colorize = T,print.cutoffs.at = seq(0.1,by = 0.1))
 )
   > set.seed(100) 
 > #100 is used to control the sampling permutation to 100. 
   > index<-sample(nrow(data),0.75*nrow(heart))
 
 test$pred<-predict(modelblr,test,type = "response")
 # type = "response" is used to get the outcome in the form of probability of having heart diseases.
 head(test)