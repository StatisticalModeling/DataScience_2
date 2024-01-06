# Predicting Grades
library(tidyverse)
library(caret)
library(rpart)
url <- "https://assets.datacamp.com/production/repositories/710/datasets/3d720e80d1ad70a88322c2175fa0e6041761a5f9/grade.csv"
if(!file.exists("./Data/grade.csv")){ download.file(url, destfile = "./Data/grade.csv")}
grade <- read.csv("./Data/grade.csv")

str(grade)
ggplot(data = grade, aes(x = final_grade)) +
  geom_histogram(binwidth = 1) + 
  theme_bw()

hist(grade$final_grade)
summary(grade$final_grade)

grade %>%
  summarize(Mean = mean(final_grade), Median = median(final_grade),
            S = sd(final_grade), iqr = IQR(final_grade))


# Randomly assign rows to ids (1/2/3 represents train/valid/test)
# This will generate a vector of ids of length equal to the number of rows
# The train/valid/test split will be approximately 70% / 15% / 15% 
set.seed(1)
assignment <- sample(1:3, size = nrow(grade), prob = c(0.70, 0.15, 0.15), replace = TRUE)

# Create a train, validation and tests from the original data frame 
grade_train <- grade[assignment == 1, ]   # subset the grade data frame to training indices only
grade_valid <- grade[assignment == 2, ]   # subset the grade data frame to validation indices only
grade_test <- grade[assignment == 3, ]    # subset the grade data frame to test indices only


# Train the model
grade_model <- rpart(formula = final_grade ~ ., 
                     data = grade_train, 
                     method = "anova")
# Look at the model output                      
print(grade_model)

library(rpart.plot)
# Plot the tree model
rpart.plot(x = grade_model, yesno = 2)

# Generate predictions on a test set
pred <- predict(object = grade_model,   # model object 
                newdata = grade_test)   # test dataset

# Compute the RMSE using Metrics and caret rmse and RMSE functions respectively
Metrics::rmse(actual = grade_test$final_grade, predicted = pred)
caret::RMSE(pred, grade_test$final_grade)

# Tuning the model
plotcp(grade_model)
# Print the "CP Table"
print(grade_model$cptable)
# Retrieve optimal cp value based on cross-validated error
(opt_index <- which.min(grade_model$cptable[, "xerror"]))
(cp_opt <- grade_model$cptable[opt_index, "CP"])

# Prune the model (to optimized cp value)
grade_model_opt <- prune(tree = grade_model, 
                         cp = cp_opt)

# Plot the optimized model
rpart.plot(x = grade_model_opt, yesno = 2)
####################################################
## Consider the caret approach
myControlB <- trainControl(method = "cv",
                           number = 5
                           )
set.seed(23)
mod_TRB <- train(final_grade ~ .,
                 data = grade_train,
                 trControl = myControlB,
                 method = "rpart",
                 tuneLength = 10)

mod_TRB

mod_TRBG <- rpart(final_grade ~ .,
                  data = grade_train, cp = mod_TRB$bestTune)
mod_TRBG
rpart.plot(mod_TRBG, yesno = 2)

#### Evaluate
predict(mod_TRBG, newdata = grade_test) -> pred
RMSE(pred, grade_test$final_grade) # 2.307
################
## Can we do better with Random Forest?
set.seed(31)
mod_RF <- train(final_grade ~ .,
                 data = grade_train,
                 trControl = myControlB,
                 method = "ranger",
                 tuneLength = 6)

mod_RF
# Let us evaluate the mod_RF
RF_pred <- predict(mod_RF, newdata = grade_test)
RMSE(RF_pred, grade_test$final_grade) # 2.134038

####
set.seed(31)
mod_KNN <- train(final_grade ~ .,
                 data = grade_train,
                 trControl = myControlB,
                 tuneLength = 6,
                 method = "knn")

mod_KNN
# Let us evaluate the mod_KNN
KNN_pred <- predict(mod_KNN, newdata = grade_test)
RMSE(KNN_pred, grade_test$final_grade) # 2.120571



#### Compare the Tree and RF  and KNN models
ANS <- resamples(list(TREE = mod_TRB, RF = mod_RF, KNN = mod_KNN))
summary(ANS)
bwplot(ANS, scales = "free")
dotplot(ANS, scales = "free")
