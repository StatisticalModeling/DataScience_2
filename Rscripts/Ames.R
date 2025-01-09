# Ames data
# 1/9/2025

library(modeldata)
library(tidyverse)
library(tidymodels)
dim(ames)
str(ames)
# Description of variables is given at:
# http://jse.amstat.org/v19n3/decock/DataDocumentation.txt
# tidymodels_prefer() uses the `conflicted` package to handle 
# common conflicts with `tidymodels` and other packages.
tidymodels_prefer()
# NOTE: we want to predict `Sale_Price`
ggplot(data = ames, aes(x = Sale_Price)) + 
  geom_histogram() + 
  theme_bw()
# First histogram with defaults not optimal
ggplot(data = ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, color = "white") + 
  theme_bw()
# Skew right - use a log transformation
ggplot(data = ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, color = "white") + 
  scale_x_log10() +
  theme_bw()
####
ames <- ames |> 
  mutate(Sale_Price = log10(Sale_Price))
#####
ggplot(data = ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, color = "white") + 
  theme_bw()
#### Splitting Data
set.seed(38)
ames_split <- initial_split(ames,
                            prop = 3/4,
                            strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
dim(ames_train)
dim(ames_test)
####
# Set up different models
## List of all parsnip models: https://www.tidymodels.org/find/parsnip/
## Linear Regression
lm_model <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")
## Lasso - penalty = 1; Ridge Regression - penalty = 0...penalty = lambda
# SEE: ?`glmnet-details`
lasso_model <- linear_reg(penalty = 1) |> 
  set_engine("glmnet") |> 
  set_mode("regression")
## Decision Tree
dt_model <- decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("regression")

## Bagged Tree
library(baguette)
bag_model <- bag_tree() |> 
  set_engine("rpart") |> 
  set_mode("regression")

## Random Forest
rf_model <- rand_forest() |> 
  set_engine("ranger", importance = "impurity") |> 
  set_mode("regression")

## Boosted Model

boost_model <- boost_tree() |> 
  set_engine("xgboost") |> 
  set_mode("regression")
#################################
### RECIPES
  
ames_recipe <- recipe(Sale_Price ~ .,
                      data = ames_train) |> 
    step_corr(all_numeric_predictors(), threshold = 0.8) |> 
    step_nzv(all_nominal_predictors()) |> 
    step_nzv(all_numeric_predictors()) |> 
    step_normalize(all_numeric_predictors()) |> 
    step_other(Neighborhood, threshold = 0.02) |> 
    step_dummy(all_nominal(), - all_outcomes()) 
      

#### Workflows and metrics

## lm_model

ames_wkfl_lm <- workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_recipe)

ames_wkfl_lm_fit <- ames_wkfl_lm |> 
  last_fit(split = ames_split)

ames_wkfl_lm_fit |> 
  collect_metrics()

## lasso_model
## This has issues.....work with later.
ames_wkfl_lasso <- workflow() |> 
  add_model(lasso_model) |> 
  add_recipe(ames_recipe)

ames_wkfl_lasso_fit <- ames_wkfl_lasso |> 
  last_fit(split = ames_split)

ames_wkfl_lasso_fit |> 
  collect_metrics()

## dt_model

ames_wkfl_dt <- workflow() |> 
  add_model(dt_model) |> 
  add_recipe(ames_recipe)

ames_wkfl_dt_fit <- ames_wkfl_dt |> 
  last_fit(split = ames_split)

ames_wkfl_dt_fit |> 
  collect_metrics()

## Visualize the Tree ######################
## Note we want the data that has been preprocessed--not ames_train
ames_recipe_prep <- ames_recipe |> 
  prep(training = ames_train)
ames_train_prep <- ames_recipe_prep |> 
  bake(new_data = NULL)
ames_test_prep <- ames_recipe_prep |> 
  bake(new_data = ames_test)

fit(dt_model, 
    Sale_Price ~., 
    data = ames_train_prep) -> ames_dt_fit 
rpart.plot::rpart.plot(ames_dt_fit$fit)
############################################

## bag_model

ames_wkfl_bag <- workflow() |> 
  add_model(bag_model) |> 
  add_recipe(ames_recipe)

ames_wkfl_bag_fit <- ames_wkfl_bag |> 
  last_fit(split = ames_split)

ames_wkfl_bag_fit |> 
  collect_metrics()

## rf_model

ames_wkfl_rf <- workflow() |> 
  add_model(rf_model) |> 
  add_recipe(ames_recipe)

ames_wkfl_rf_fit <- ames_wkfl_rf |> 
  last_fit(split = ames_split)

ames_wkfl_rf_fit |> 
  collect_metrics()


## boost_model

ames_wkfl_boost <- workflow() |> 
  add_model(boost_model) |> 
  add_recipe(ames_recipe)

ames_wkfl_boost_fit <- ames_wkfl_boost |> 
  last_fit(split = ames_split)

ames_wkfl_boost_fit |> 
  collect_metrics()
