# Fitting Models with `parsnip`
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
hitters_split <- initial_split(Hitters, prop = .80)
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

stuff |>
  select(Salary, .pred)
# Test RMSE - 357
rmse(stuff, Salary, .pred)


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


# Test RMSE - 354
rmse(stuff, Salary, .pred)



#######################################################################

use_ranger(Salary ~. , data = hitters_train, verbose = TRUE)

ranger_recipe <- recipe(formula = Salary ~ ., data = hitters_train) 

ranger_spec <- rand_forest(mtry = tune(), 
                           min_n = tune(), 
                           trees = 500) |>  
  set_mode("regression")  |>  
  set_engine("ranger") 

ranger_workflow <- 
  workflow()  |>  
  add_recipe(ranger_recipe)  |>  
  add_model(ranger_spec) 

ranger_grid <- tidyr::crossing(mtry = c(3, 6, 9, 12), 
                               min_n = c(5, 20, 35, 50)) 

folds <- vfold_cv(Hitters, v = 10, repeats = 5)

# This will take a few minutes
set.seed(48700)
ranger_tune <-
  tune_grid(ranger_workflow, resamples = folds, grid = ranger_grid)


######
ranger_tune
autoplot(ranger_tune)
show_best(ranger_tune, metric = "rmse")

ranger_param <- tibble(mtry = 3, min_n = 5)
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

# Test RMSE - 297
rmse(stuff, Salary, .pred)
