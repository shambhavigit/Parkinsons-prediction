library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)




set.seed(123)
data_split <- initial_split(data, prop = .7)
data_train <- training(data_split)
data_test  <- testing(data_split)

# for reproduciblity
set.seed(123)

# default RF model
m1 <- randomForest(
  formula = status ~ .,
  data    = data_train
)
m1
plot(m1)
m1$mse
# number of trees with lowest MSE
which.min(m1$mse)

#RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])

# create training and validation data 
set.seed(123)
valid_split <- initial_split(data_train, .8)

# training data
data_train_v2 <- analysis(valid_split)

# validation data
data_valid <- assessment(valid_split)
x_test <- data_valid[setdiff(names(data_valid), "status")]
y_test <- data_valid$status

rf_oob_comp <- randomForest(
  formula = status ~ .,
  data    = data_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

features <- setdiff(names(data_train), "status")

set.seed(123)

m2 <- tuneRF(
  x          = data_train[features],
  y          = data_train$status,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

# randomForest speed
system.time(
  data_randomForest <- randomForest(
    formula = status ~ ., 
    data    = data_train, 
    ntree   = 500,
    mtry    = floor(length(features) / 3)
  )
)

# ranger speed
system.time(
  data_ranger <- ranger(
    formula   = status ~ ., 
    data      = data_train, 
    num.trees = 500,
    mtry      = floor(length(features) / 3)
  )
)

hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = status ~ ., 
    data            = data_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  
  
  
  com <- data[,2:23]
  > cc <- cor(com, method = "spearman")
  > corrplot(cc)
  > corrplot(cc, tl.col = "black", order = "hclust", hclust.method = "average", addrect = 4, tl.cex = 0.7)
  > 