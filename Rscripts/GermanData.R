## German Credit
# Parallel Processing
library(doMC)
registerDoMC(cores = 20)
## READ IN DATA
library(readr)
credit <- read_csv("./Data/credit.csv")
View(credit)
## partition the data
library(caret)
set.seed(867) # set seed for reproducibility
trainID <- createDataPartition(y = credit$default,
                               p = 0.80,
                               list = FALSE)
train <- credit[trainID, ]
test  <- credit[-trainID, ]
dim(train)
dim(test)
## create basic tree
library(rpart)
credit_model <- rpart(formula = default ~ .,
                      data = train,
                      method = "class")
## graph model
library(rpart.plot)
rpart.plot(x = credit_model, yesno = 2)
## Each node shows: 1) predicted class, 
## 2) predicted probability of default, 
# 3) % observations in that node

## Generate predictions for the test set and evaluate performance

class_pred <- predict(object = credit_model,
                      newdata = test,
                      type = "class"
                      )
## Confusion matrix

confusionMatrix(data = class_pred,
                reference = as.factor(test$default))

## Tune the model
plotcp(credit_model)
# Print the "CP Table"
print(credit_model$cptable)
# Retrieve optimal cp value based on cross-validated error
(opt_index <- which.min(credit_model$cptable[, "xerror"]))
(cp_opt <- credit_model$cptable[opt_index, "CP"])

## Prunning the tree
credit_model_opt <- prune(tree = credit_model,
                          cp = cp_opt)
## Plot optimal tree
rpart.plot(x = credit_model_opt, yesno = 2)

## Generate predictions for the test set and evaluate performance

class_pred_opt <- predict(object = credit_model_opt,
                      newdata = test,
                      type = "class"
)
## Confusion matrix

confusionMatrix(data = class_pred_opt,
                reference = as.factor(test$default))

#### Note Accuracy < no Information Rate !!!!

########### Do everything with train() now
## set up trainControl() first

myControl <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 5,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary
                          )

model_TR <- train(default ~ .,
                  data = train,
                  trControl = myControl,
                  method = "rpart",
                  tuneLength = 10)
model_TR
model_TR$bestTune
##
model_TR_opt <- rpart(default ~ ., 
                      data = train,
                      cp = model_TR$bestTune)
###
rpart.plot(model_TR_opt, yesno = 2)
###
### How good is this Tree?

## Generate predictions for the test set and evaluate performance

class_pred_opt <- predict(object = model_TR_opt,
                          newdata = test,
                          type = "class"
)
## Confusion matrix

confusionMatrix(data = class_pred_opt,
                reference = as.factor(test$default))
## Still not a very good model!!!!!!

## How about Bagging?  Lets use caret with method = 'treebag'

model_BAG <- train(default ~ .,
                  data = train,
                  trControl = myControl,
                  tuneLength = 10,
                  metric = "ROC",
                  method = "treebag")
model_BAG

## Generate predictions for the test set and evaluate performance

class_BAG <- predict(object = model_BAG,
                    newdata = test,
                    type = "raw")
## Confusion matrix

confusionMatrix(data = class_BAG,
                reference = as.factor(test$default))

# Print the CV AUC
model_BAG$results[,"ROC"]


##### How about Random Forest

model_RF <- train(default ~ .,
                  data = train,
                  trControl = myControl,
                  tuneLength = 10,
                  metric = "ROC",
                  method = "ranger")
model_RF

model_RF2 <- train(default ~ .,
                  data = train,
                  trControl = myControl,
                  tuneLength = 10,
                  metric = "ROC",
                  method = "rf")
model_RF2

######
## Generate predictions for the test set and evaluate performance

class_RF <- predict(object = model_RF,
                          newdata = test,
                          type = "raw")
## Confusion matrix

confusionMatrix(data = class_RF,
                reference = as.factor(test$default))

class_RF2 <- predict(object = model_RF2,
                    newdata = test,
                    type = "raw")
## Confusion matrix

confusionMatrix(data = class_RF2,
                reference = as.factor(test$default))

###############################################################################
## GBMs next
model_GBM <- train(default ~ .,
                  data = train,
                  trControl = myControl,
                  tuneLength = 10,
                  metric = "ROC",
                  method = "gbm")
model_GBM
summary(model_GBM)
## Evaluate Performance of GBM now

class_GBM <- predict(object = model_GBM,
                     newdata = test,
                     type = "raw")
## Confusion matrix

confusionMatrix(data = class_GBM,
                reference = as.factor(test$default))

#############################################################
## Comparing the models now

ANS <- resamples(list(TREE = model_TR, BAG = model_BAG, RF = model_RF, GBM = model_GBM))
summary(ANS)
bwplot(ANS)
dotplot(ANS)
