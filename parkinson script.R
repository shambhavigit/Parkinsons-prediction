data_1 <- read.csv(file = 'parkinsons.data')

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

 install.packages("corrplot")
 library(corrplot)
 install.packages("ggplot2")
 library(ggplot2)
 #define the columns that contain your abundance data. Change the number after the ":" to subset your data
 com <- data[,2:23]
 #Now create a correlation matrix with your community composition data using the command ‘cor’:
  cc <- cor(com, method = "spearman")
  #I usually use Spearman correlation because I’m not overly concerned that my relationships fit a linear model, and Spearman captures all types of positive or negative relationships (i.e. exponential, logarithmic)
  # The easiest way to visualize this correlation matrix is using the function “corrplot” from the package corrplot:
  corrplot(cc)
  #In this figure, blue is positive and red is negative. The diagonal line of dark blue cutting across the square is due to the perfect correlation between an OTU and itself.
  #I don’t particularly like the font size or colour so I will change these parameters with ‘tl.cex’, and ‘tl.col’. I also would like to cluster my OTUs so that those with similar patterns of correlation coefficients are closer together. I can do this by defining the ‘order’ parameter with “hclust” (for heirarchical clustering), and the ‘hclust.method’ as “average”. I can also add rectangles with ‘addrect’ which will divide the OTUs into a given number of groups based on hierarchical clustering.
  corrplot(cc, tl.col ="black",order  ="hclust",hclust.method= "average", addrect = 4, tl.cex = 0.7)
  xx = ggplot(ccm, aes(x = variable, y = OTUs)) +
    + geom_tile(aes(fill = value), colour = "grey45") +
    + coord_equal() +
    + scale_fill_gradient(low = "navy", high = "darkorange") +
    + theme(axis.text.y = element_text(face = "bold", colour = "grey25"),
            + legend.title = element_text(size = 10, face = "bold"),legend.position = "bottom",
            + axis.text.x = element_text(angle = 90, face = "bold",colour = "grey25", vjust = 0.5, hjust = 0),
            + panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = NA),
            + axis.ticks = element_blank()) +
    + labs(x= "", y = "", fill = "Spearman's Correlation") +
    + scale_x_discrete(position = "top") +
    + scale_y_discrete(limits = rev(levels(ccm$OTUs)))
  
  remove MDvp jitter, jitter.abs, mdvp.ppq,mdvp shimmer
  mdvp.shimmer db, mdvp.apq, HNR, shimmer Apq3
  5,6,8,9,10,11,12,14,17
  data_1 <- subset(data, select = -c(1,5,6,8,10,11,12,14))
  
  
  
  park.m=melt(data_1,id.vars = "status")
  > p <- ggplot(data = park.m, aes(x=variable, y=value)) + 
    +     geom_boxplot(aes(fill=status))
  > p + facet_wrap( ~ variable, scales="free")
  > 
    > park$MDVP.RAP= outlier_remove(park$MDVP.RAP)
  > park$NHR= outlier_remove(park$NHR)
  > 
    > data_1$MDVP.RAP= outlier_remove(data_1$MDVP.RAP)
  > data_1$NHR= outlier_remove(data_1$NHR)
  > split=sample.split(data_1$status,SplitRatio = .80)
  > train=subset(data_1,split==T)
  > test=subset(data_1,split==F)
  > model.lg=glm(data=train,status~.,family = "binomial")
  > summary(model.lg)
  step(model.lg,direction="backward") 
  model <- glm(formula = status ~ MDVP.RAP + NHR + RPDE + spread1 + spread2, 
               +                     family = "binomial", data = train)
   pre <- as.numeric(predict(model,type="response")>0.8)
   confusionMatrix(table(pre,train$status))
  
   test$status <- as.factor(test$status)
   test$pred<-predict(final.model,test,type = "response")>0.8