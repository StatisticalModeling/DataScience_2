## ----include = FALSE------------------------------
library(knitr) # packages
library(tidyverse)
library(scales)
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", comment = NA, message = FALSE,  warning = FALSE)


## -------------------------------------------------
library(tidyverse)
cars2018 <- read_csv("cars2018.csv")


## -------------------------------------------------
# Print the cars2018 object
glimpse(cars2018)


## -------------------------------------------------
# Plot the histogram
ggplot(cars2018, aes(x = mpg)) +
  geom_histogram(binwidth = 2, color = "black", fill = "red") +
  labs(x = "Fuel efficiency (mpg)",
       y = "Number of cars") + 
  theme_bw()


## -------------------------------------------------
# Consider using log10(mpg) instead of mpg
cars2018 <- cars2018 |> 
  mutate(log_mpg = log(mpg))
ggplot(cars2018, aes(x = log_mpg)) +
  geom_histogram(binwidth = 0.15, color = "black", fill = "red") +
  labs(x = "Fuel efficiency (log_mpg)",
       y = "Number of cars") + 
  theme_bw()


## -------------------------------------------------
# Deselect the 2 columns to create cars_vars
car_vars <- cars2018 |> 
    select(-model, -model_index)


## -------------------------------------------------
# Fit a linear model
fit_all <- lm(mpg ~ . - log_mpg, data = car_vars)


## -------------------------------------------------
# Print the summary of the model
summary(fit_all)
# Better yet
broom::tidy(fit_all) |> 
  knitr::kable()
# and
broom::glance(fit_all) |> 
  knitr::kable()


## -------------------------------------------------
# Load tidymodels
library(tidymodels)


## -------------------------------------------------
# Split the data into training and test sets
set.seed(1234)
car_split <- car_vars |> 
    select(-mpg) |> 
    initial_split(prop = 0.8, strata = transmission)

car_train <- training(car_split)
car_test <- testing(car_split)

glimpse(car_train)
glimpse(car_test)


## -------------------------------------------------
# Build a linear regression model specification
lm_spec <- linear_reg() |> 
    set_engine("lm")

# Train a linear regression model
fit_lm <- lm_spec |> 
    fit(log_mpg ~ ., data = car_train)

# Print the model object
broom::tidy(fit_lm) |> 
  knitr::kable()
# and
broom::glance(fit_lm) |> 
  knitr::kable()


## -------------------------------------------------
# Build a random forest model specification
rf_spec <- rand_forest() |> 
    set_engine("ranger", importance = "impurity") |> 
    set_mode("regression")

# Train a random forest model
fit_rf <- rf_spec |> 
    fit(log_mpg ~ ., data = car_train)

# Print the model object
fit_rf

## -------------------------------------------------
vip::vip(fit_rf) +
  theme_bw()


## -------------------------------------------------
# Create the new columns
# rename and relocate are from dplyr
results <- car_train |> 
    bind_cols(predict(fit_lm, car_train) |> 
                  rename(.pred_lm = .pred)) |> 
    bind_cols(predict(fit_rf, car_train) |> 
                  rename(.pred_rf = .pred)) |> 
    relocate(log_mpg, .pred_lm, .pred_rf, .before = displacement)
head(results) |> 
  knitr::kable()


## -------------------------------------------------
# Evaluate the performance
metrics(data = results, truth = log_mpg, estimate = .pred_lm) |> 
  knitr::kable()
metrics(data = results, truth = log_mpg, estimate = .pred_rf) |> 
  knitr::kable()


## -------------------------------------------------
# Create the new columns
results <- car_test  |> 
    bind_cols(predict(fit_lm, car_test)  |> 
                  rename(.pred_lm = .pred))  |> 
    bind_cols(predict(fit_rf, car_test)  |> 
                  rename(.pred_rf = .pred)) |> 
    relocate(log_mpg, .pred_lm, .pred_rf, .before = displacement)

# Evaluate the performance
metrics(results, truth = log_mpg, estimate = .pred_lm) |> 
  knitr::kable()
metrics(results, truth = log_mpg, estimate = .pred_rf) |> 
  knitr::kable()


## -------------------------------------------------
## Create bootstrap resamples
set.seed(444)
car_boot <- bootstraps(data = car_train, times = 25)


## -------------------------------------------------
#| label: "bothmodels"
#| cache: true
# Evaluate the models with bootstrap resampling
lm_res <- lm_spec |> 
    fit_resamples(
        log_mpg ~ .,
        resamples = car_boot,
        control = control_resamples(save_pred = TRUE)
    )

rf_res <- rf_spec |> 
    fit_resamples(
        log_mpg ~ .,
        resamples = car_boot,
        control = control_resamples(save_pred = TRUE)
    )


## -------------------------------------------------
results <-  bind_rows(lm_res |> 
                          collect_predictions() |> 
                          mutate(model = "lm"),
                      rf_res |> 
                          collect_predictions() |> 
                          mutate(model = "rf"))

glimpse(results)


## -------------------------------------------------
results |> 
  group_by(model) |> 
  metrics(truth = log_mpg, estimate = .pred) |> 
  knitr::kable()


## -------------------------------------------------
results |> 
  ggplot(aes(x = log_mpg, y = .pred)) +
  geom_abline(lty = "dashed", color = "gray50") +
  geom_point(aes(color = id), size = 1.5, alpha = 0.1, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(~ model) + 
  coord_obs_pred() + 
  theme_bw() + 
  labs(y = "Predicted (log_mpg)",
       x = "Actual (log_mpg)")


## -------------------------------------------------
# Build a random forest model specification
rf_spec <- rand_forest(mtry = tune(),
                       min_n = tune(),
                       trees = 1000) |> 
    set_engine("ranger", importance = "impurity") |> 
    set_mode("regression")
rf_spec


## -------------------------------------------------
set.seed(42)
car_folds <- vfold_cv(car_train, v = 10, repeats = 5)


## -------------------------------------------------
rf_recipe <- recipe(log_mpg ~.,  data = car_train) 
rf_wkfl <- workflow() |> 
  add_recipe(rf_recipe) |> 
  add_model(rf_spec)
rf_wkfl


## -------------------------------------------------
#| cache: true
#| label: "rf_tune"
library(future)
plan(multisession, workers = 4)
set.seed(321)
rf_tune <- tune_grid(rf_wkfl, resample = car_folds, grid = 15)
rf_tune


## -------------------------------------------------
show_best(rf_tune, metric = "rmse")


## -------------------------------------------------
# rf_param <- tibble(mtry = 6, min_n = 2)
rf_param <- select_best(rf_tune, metric = "rmse")
rf_param
final_rf_wkfl <- rf_wkfl |> 
  finalize_workflow(rf_param)
final_rf_wkfl


## -------------------------------------------------
final_rf_fit <- final_rf_wkfl |> 
  fit(car_train)
# Variable importance plot
vip::vip(final_rf_fit) + 
  theme_bw()


## -------------------------------------------------
augment(final_rf_fit, new_data = car_test) |> 
  metrics(truth = log_mpg, estimate = .pred) -> RF
RF |> 
  knitr::kable()


## -------------------------------------------------
# R-squared plot
augment(final_rf_fit, new_data = car_test) |> 
  ggplot(aes(x = log_mpg, y = .pred)) + 
  geom_point() +
  geom_smooth(method = "gam") +
  geom_abline(lty = "dashed") +
  coord_obs_pred() + 
  theme_bw() + 
  labs(x = "Observed log_mpg",
       y = "Predicted log_mpg",
       title = "R-squared Plot")


## -------------------------------------------------
library(probably)
augment(final_rf_fit, new_data = car_test) |>
cal_plot_regression(truth = log_mpg, estimate = .pred) + 
  theme_bw()


## -------------------------------------------------
xgboost_spec <- 
  boost_tree(trees = tune(), min_n = tune(), tree_depth = tune(), 
             learn_rate = tune(), loss_reduction = tune(), 
             sample_size = tune()) |>  
  set_mode("regression") |>  
  set_engine("xgboost") 
xgboost_spec


## -------------------------------------------------
xgboost_recipe <- 
  recipe(formula = log_mpg ~  . , data = car_train) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors())  |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors(), threshold = 0.9)


## -------------------------------------------------
xgboost_workflow <- 
  workflow() |>  
  add_recipe(xgboost_recipe) |>  
  add_model(xgboost_spec) 
xgboost_workflow


## -------------------------------------------------
#| cache: true
#| label: "xgb_tune"
library(finetune) 
# used for tune_race_anova() which...
# after an initial number of resamples have been evaluated, 
# the process eliminates tuning parameter combinations that 
# are unlikely to be the best results using a repeated 
# measure ANOVA model.
set.seed(49)
xgboost_tune <-
  tune_race_anova(xgboost_workflow, resamples = car_folds, grid = 15)
xgboost_tune


## -------------------------------------------------
show_best(xgboost_tune, metric = "rmse")


## -------------------------------------------------
# xgboost_param <- tibble(trees = 2000, 
#                        min_n = 9, 
#                        tree_depth = 6, 
#                        learn_rate = 0.00681,
#                        loss_reduction = 0.0000000155, 
#                        sample_size = 0.771)
xgboost_param <- select_best(xgboost_tune)
final_xgboost_wkfl <- xgboost_workflow |> 
  finalize_workflow(xgboost_param)
final_xgboost_wkfl


## -------------------------------------------------
final_xgboost_fit <- final_xgboost_wkfl |> 
  fit(car_train)

augment(final_xgboost_fit, new_data = car_test) |> 
  metrics(truth = log_mpg, estimate = .pred) -> R5
R5 |> 
  knitr::kable()


## -------------------------------------------------
enet_spec <- linear_reg(penalty = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("regression")
enet_spec


## -------------------------------------------------
enet_recipe <- 
  recipe(formula = log_mpg ~  . , data = car_train) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors())  |> 
  step_normalize(all_numeric_predictors()) |> 
  step_corr(all_numeric_predictors(), threshold = 0.9)


## -------------------------------------------------
enet_workflow <- 
  workflow() |>  
  add_recipe(enet_recipe) |>  
  add_model(enet_spec) 
enet_workflow


## -------------------------------------------------
library(finetune)
set.seed(49)
enet_tune <-
  tune_race_anova(enet_workflow, resamples = car_folds, grid = 15)
enet_tune


## -------------------------------------------------
show_best(enet_tune, metric = "rmse")
enet_param <- select_best(enet_tune, metric = "rmse")
final_enet_wkfl <- enet_workflow |> 
  finalize_workflow(enet_param)
final_enet_wkfl


## -------------------------------------------------
final_enet_fit <- final_enet_wkfl |> 
  fit(car_train)


## -------------------------------------------------
augment(final_enet_fit, new_data = car_test) |> 
  metrics(truth = log_mpg, estimate = .pred) -> R6
R6 |> 
  knitr::kable()


## -------------------------------------------------
# Print the model object
broom::tidy(final_enet_fit) |> 
  knitr::kable()


## -------------------------------------------------
lm_spec <- linear_reg() |> 
  set_engine("lm")
ns_recipe <- recipe(log_mpg ~ ., data = car_train) |> 
  step_ns(displacement, cylinders, gears, deg_free = 6) |> 
  step_interact(~drive:transmission + drive:recommended_fuel)
ns_wkfl <- workflow() |> 
  add_recipe(ns_recipe) |> 
  add_model(lm_spec)
ns_wkfl


## -------------------------------------------------
final_lm_fit <- ns_wkfl |> 
  fit(car_train)


## -------------------------------------------------
augment(final_lm_fit, new_data = car_test) |> 
  metrics(truth = log_mpg, estimate = .pred) -> R7
R7 |> 
  knitr::kable()


## -------------------------------------------------
broom::tidy(final_lm_fit) |> 
  knitr::kable()

