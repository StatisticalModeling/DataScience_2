# Ames data
# 1/9/2025

library(modeldata)
library(tidyverse)
library(tidymodels)
dim(ames)
str(ames)
library(janitor)
ames <- ames |> clean_names()
# Description of variables is given at:
# http://jse.amstat.org/v19n3/decock/DataDocumentation.txt
# tidymodels_prefer() uses the `conflicted` package to handle 
# common conflicts with `tidymodels` and other packages.
tidymodels_prefer()
# NOTE: we want to predict `Sale_Price`
ggplot(data = ames, aes(x = sale_price)) + 
  geom_histogram() + 
  theme_bw()
# First histogram with defaults not optimal
ggplot(data = ames, aes(x = sale_price)) + 
  geom_histogram(bins = 50, color = "white") + 
  theme_bw()
# Skew right - use a log transformation
ggplot(data = ames, aes(x = sale_price)) + 
  geom_histogram(bins = 50, color = "white") + 
  scale_x_log10() +
  theme_bw()
###
ggplot(data = ames, aes(x = gr_liv_area, 
                        y = sale_price,
                        color = sale_condition)) + 
  geom_point() +
  theme_bw()

####
ames <- ames |> 
  filter(gr_liv_area < 4000) |> 
  mutate(sale_price = log10(sale_price))
#####
ggplot(data = ames, aes(x = gr_liv_area, 
                        y = sale_price,
                        color = sale_condition)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 

ggplot(data = ames, aes(x = sale_price)) + 
  geom_histogram(bins = 50, color = "white") + 
  theme_bw()




#### Splitting Data
set.seed(38)
ames_split <- initial_split(ames,
                            prop = 3/4,
                            strata = sale_price)
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
  
ames_recipe <- recipe(sale_price ~ neighborhood + gr_liv_area + year_built + 
                      bldg_type + latitude + longitude, data = ames_train) |> 
    step_log(gr_liv_area, base = 10) |> 
    step_other(neighborhood, threshold = 0.01) |> 
    step_dummy(all_nominal_predictors()) 

ames_recipe2 <- recipe(sale_price ~ lot_area + total_bsmt_sf +
                       gr_liv_area + garage_cars + fireplaces,
                       data = ames_train) |> 
  step_corr(all_numeric_predictors(), threshold = 0.8) |> 
  step_nzv(all_nominal_predictors()) |> 
  step_nzv(all_nominal_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  # step_other(neighborhood, threshold = 0.02) |> 
  step_dummy(all_nominal(), - all_outcomes()) 
      

#### Workflows and metrics

## lm_model

ames_wkfl_lm <- workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_recipe)

######

# Repeated Cross Validation
set.seed(33)
folds <- vfold_cv(ames_train, v = 10, repeats = 5)
folds

ames_wkfl_lm |> 
  fit_resamples(resamples = folds) -> lm_res
collect_metrics(lm_res)


######

ames_wkfl_lm_fit <- ames_wkfl_lm |> 
  last_fit(split = ames_split)

ames_wkfl_lm_fit |> 
  collect_metrics()

ames_lm_fit <- lm_model |> 
      fit(sale_price ~ ., data = ames_train)
tidy(ames_lm_fit)
glance(ames_lm_fit)
####################################


ames_wkfl2_lm <- workflow() |> 
  add_model(lm_model) |> 
  add_recipe(ames_recipe2)

ames_wkfl2_lm |> 
  fit_resamples(resamples = folds) -> lm_res2
collect_metrics(lm_res2)

ames_wkfl2_lm_fit <- ames_wkfl2_lm |> 
  last_fit(split = ames_split)

ames_wkfl2_lm_fit |> 
  collect_metrics()

ames_lm_fit2 <- lm_model |> 
  fit(sale_price ~ lot_area + total_bsmt_sf +
        gr_liv_area + garage_cars + fireplaces, data = ames_train)
tidy(ames_lm_fit2)


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
    sale_price ~., 
    data = ames_train_prep) -> ames_dt_fit 
rpart.plot::rpart.plot(ames_dt_fit$fit)
############################################
## Lets tune the decision tree
dt_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune(),
                               min_n = tune()) |> 
  set_engine("rpart") |> 
  set_mode("regression")
dt_tune_model
ames_tune_wkfl <- ames_wkfl_dt |> 
  update_model(dt_tune_model)
ames_tune_wkfl
# Grid
dt_grid <- grid_random(hardhat::extract_parameter_set_dials(dt_tune_model),
                       size = 21)
dt_grid
# Hyperparameter tuning
# This will take a minute or two
dt_tuning <- ames_tune_wkfl |> 
  tune_grid(resamples = folds,
            grid = dt_grid)
dt_tuning
dt_tuning |> 
  collect_metrics()

dt_tuning |> 
  show_best(metric = "rmse")

best_dt_model <- dt_tuning |> 
  select_best(metric = "rmse")
best_dt_model

# Finalize the workflow

final_ames_dt_wkfl <- ames_tune_wkfl |> 
  finalize_workflow(best_dt_model)
final_ames_dt_wkfl

# Train finalized workflow

ames_final_dt_fit <- final_ames_dt_wkfl |> 
  last_fit(split = ames_split)
ames_final_dt_fit

ames_final_dt_fit |> 
  collect_metrics()

############################################

 best_spec <- finalize_model(dt_tune_model, best_dt_model)
 final_model <- fit(best_spec, sale_price ~ ., ames_train)
 # Graph the final_model
 rpart.plot::rpart.plot(final_model$fit)
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




