# Fitting Models with `parsnip`
# Adding stuff
library(tidyverse)
library(tidymodels)
library(ISLR2)




lm_spec <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")
lm_spec

#
summary(Hitters)
Hitters <- na.omit(Hitters)
summary(Hitters)
dim(Hitters)

set.seed(444)
hitters_split <- initial_split(Hitters, prop = .80, strata = Salary)
hitters_train <- training(hitters_split)
hitters_test <- testing(hitters_split)


lm_recipe <- 
  recipe(formula = Salary ~ ., data = hitters_train)  |>  
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_factor())

lm_workflow <- 
  workflow()  |>  
  add_recipe(lm_recipe)  |>  
  add_model(lm_spec) 

set.seed(44)
folds <- vfold_cv(Hitters, v = 10, repeats = 5)

lm_tune <- 
  tune_grid(lm_workflow, resamples = folds) 

lm_tune
show_best(lm_tune, metric = "rmse")


final_lm_fit <- lm_workflow |> 
  fit(hitters_train)

hitters_test |> 
  bind_cols(predict(final_lm_fit, hitters_test)) -> stuff
stuff 

hitters_aug <- augment(final_lm_fit, hitters_test)
hitters_aug |> 
  relocate(Salary, .before = .resid) -> hitters_aug
hitters_aug

stuff |>
  select(Salary, .pred)
# Test RMSE - 341
rmse(stuff, Salary, .pred)
# Or
rmse(hitters_aug, Salary, .pred)
# R^2 = 0.385
rsq(hitters_aug, Salary, .pred)


###############################################################
glmnet_spec <- linear_reg() |> 
  set_engine("glmnet") |> 
  set_mode("regression") 
glmnet_spec


library(usemodels)
use_glmnet(Salary ~. , data = hitters_train, verbose = TRUE)

glmnet_recipe <- 
  recipe(formula = Salary ~ ., data = hitters_train)  |>  
  ## Regularization methods sum up functions of the model slope 
  ## coefficients. Because of this, the predictor variables should be on 
  ## the same scale. Before centering and scaling the numeric predictors, 
  ## any predictors with a single unique value are filtered out. 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_factor())

glmnet_spec <- 
  linear_reg(penalty = tune(), mixture = tune())  |>  
  set_mode("regression")  |>  
  set_engine("glmnet") 

glmnet_workflow <- 
  workflow()  |>  
  add_recipe(glmnet_recipe)  |>  
  add_model(glmnet_spec) 

glmnet_grid <- tidyr::crossing(penalty = 10^seq(-6, -1, length.out = 20), 
                               mixture = c(0.05, 0.2, 0.4, 0.6, 0.8, 1)) 
## Cross validation

# folds <- vfold_cv(Hitters, v = 10, repeats = 5)

# This will take ( ~ 1-2 minutes)
glmnet_tune <- 
  tune_grid(glmnet_workflow, resamples = folds, grid = glmnet_grid) 

glmnet_tune
autoplot(glmnet_tune)
show_best(glmnet_tune, metric = "rmse")

#tibble(penalty = 0.000001, mixture = 0.05)
glmnet_param <- tibble(penalty = 0.000001, mixture = 0.05)
final_glmnet_wkfl <- glmnet_workflow |> 
 finalize_workflow(glmnet_param)
final_glmnet_wkfl 
  
final_glmnet_fit <- final_glmnet_wkfl |> 
  fit(hitters_train)

hitters_test |> 
  bind_cols(predict(final_glmnet_fit, hitters_test)) -> stuff
stuff <- stuff |> 
  relocate(.pred, .after = Salary)
stuff


# Test RMSE - 339
rmse(stuff, Salary, .pred)
# R^2 = 0.393
rsq(stuff, Salary, .pred)



#######################################################################

use_ranger(Salary ~. , data = hitters_train, verbose = TRUE)

ranger_recipe <- recipe(formula = Salary ~ ., data = hitters_train) 

ranger_spec <- rand_forest(mtry = tune(), 
                           min_n = tune(), 
                           trees = 500) |>  
  set_mode("regression")  |>  
  set_engine("ranger",
             importance = "impurity")
ranger_spec

ranger_workflow <- 
  workflow()  |>  
  add_recipe(ranger_recipe)  |>  
  add_model(ranger_spec) 

### ranger_grid <- tidyr::crossing(mtry = c(3, 5, 7, 9), 
##                               min_n = c(3, 8, 13, 18)) 

# below already done above with seed
# folds <- vfold_cv(Hitters, v = 10, repeats = 5)

# This will take a few minutes (13 or so for 1000 trees....
# but does not really change much to use 500 trees and runs in 6.5 min)
set.seed(48700)
ranger_tune <-
  tune_grid(ranger_workflow, resamples = folds, grid = 32)


######
ranger_tune
autoplot(ranger_tune)
show_best(ranger_tune, metric = "rmse")

ranger_param <- tibble(mtry = 6, min_n = 5)
final_ranger_wkfl <- ranger_workflow |> 
  finalize_workflow(ranger_param)
final_ranger_wkfl 

final_ranger_fit <- final_ranger_wkfl |> 
  fit(hitters_train)


hitters_test |> 
  bind_cols(predict(final_ranger_fit, hitters_test)) -> stuff
stuff <- stuff |> 
  relocate(.pred, .after = Salary)
stuff

# Test RMSE - 247
rmse(stuff, Salary, .pred)
# R^2 = 0.676
rsq(stuff, Salary, .pred)


library(vip)
vip(final_ranger_fit)
####################################################################
### xgboost now

use_xgboost(Salary ~. , data = hitters_train, verbose = TRUE)

# Note I am getting a 0 correlation error...suggestion is that the model Salary ~ . is too complex. 
# Went back to ranger and added variable importance.  Will start with 5 most 
# important variables from Random Forest. Still results in 0 standard deviation :(
# CRuns + CRBI + CHits + CAtBat + CHmRun + CWalks + Walks + RBI + Hits +AtBat .....
# Going back to including everything....

xgboost_recipe <- 
  recipe(formula = Salary ~  . , data = hitters_train) |>  
  step_normalize(all_numeric_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) 
xgboost_recipe

xgboost_spec <- 
  boost_tree(trees = 500, min_n = tune(), tree_depth = tune(), learn_rate = tune(), 
             loss_reduction = tune(), sample_size = tune()) |>  
  set_mode("regression") |>  
  set_engine("xgboost") 
xgboost_spec

xgboost_workflow <- 
  workflow() |>  
  add_recipe(xgboost_recipe) |>  
  add_model(xgboost_spec) 
xgboost_workflow

# This will take a hot minute (Say 14 + minutes)
set.seed(28004)
xgboost_tune <-
  tune_grid(xgboost_workflow, resamples = folds, grid = 64)

autoplot(xgboost_tune)
show_best(xgboost_tune, metric = "rmse")

#####

xgboost_param <- tibble(min_n = 3, tree_depth = 4, learn_rate = 0.0203, loss_reduction = 0.00000000817, sample_size = 0.262)
final_xgboost_wkfl <- xgboost_workflow |> 
  finalize_workflow(xgboost_param)
final_xgboost_wkfl 

final_xgboost_fit <- final_xgboost_wkfl |> 
  fit(hitters_train)


hitters_test |> 
  bind_cols(predict(final_xgboost_fit, hitters_test)) -> stuff
stuff <- stuff |> 
  relocate(.pred, .after = Salary)
stuff

# Test RMSE - 224
rmse(stuff, Salary, .pred)
# Test R^2 = 0.736
rsq(stuff, Salary, .pred)

#### Graph the R^2
ggplot(data = stuff, aes(x = Salary, y = .pred)) + 
  geom_point() +
  coord_obs_pred() + 
  theme_bw() + 
  geom_abline(intercept = 0, slope = 1, lty = 2) + 
  labs(y = "Predicted Salary", 
       x= "Actual Salary",
       title = "R-square plot for xgboost model")
stuff |> 
  summarize(R2 = cor(Salary, .pred)^2)
