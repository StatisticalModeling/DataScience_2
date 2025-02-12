# Cars
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

cars2018 <- read_csv("./Data/cars2018.csv")

# Print the cars2018 object
glimpse(cars2018)

# Plot the histogram
ggplot(cars2018, aes(x = mpg)) +
  geom_histogram(bins = 25, color = "black", fill = "red") +
  labs(x = "Fuel efficiency (mpg)",
       y = "Number of cars") + 
  theme_bw()

# Deselect the 2 columns to create cars_vars
car_vars <- cars2018 |> 
  select(-model, -model_index) |> 
  mutate(log_mpg = log(mpg)) |> 
  select(-mpg)

# Fit a linear model
fit_all <- lm(log_mpg ~ ., data = car_vars)

# Print the summary of the model
summary(fit_all)


# Split the data into training and test sets
set.seed(44)
car_split <- car_vars |> 
  initial_split(prop = .80, strata = transmission)

car_train <- training(car_split)
car_test <- testing(car_split)

glimpse(car_train)
glimpse(car_test)

# Build a linear regression model specification
lm_mod <- linear_reg() |> 
  set_engine("lm")

# Train a linear regression model
fit_lm <- lm_mod |> 
  fit(log_mpg ~ ., data = car_train)

# Print the model object
fit_lm |> 
  tidy()

# Build a random forest model specification
rf_mod <- rand_forest()  |> 
  set_engine("ranger")  |> 
  set_mode("regression")

# Train a random forest model
fit_rf <- rf_mod  |> 
  fit(log_mpg ~ ., data = car_train)

# Print the model object
fit_rf 

#### Evaluate Model Performance

# Create the new columns
results <- car_train  |> 
  bind_cols(predict(fit_lm, car_train)  |> 
              rename(.pred_lm = .pred))  |> 
  bind_cols(predict(fit_rf, car_train)  |> 
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = log_mpg, estimate = .pred_lm)
metrics(results, truth = log_mpg, estimate = .pred_rf)


# We are really not interested in the performance on Training Data
# Rather we want the performance on unseen Test Data

# Create the new columns
results <- car_test  |> 
  bind_cols(predict(fit_lm, car_test)  |> 
              rename(.pred_lm = .pred))  |> 
  bind_cols(predict(fit_rf, car_test)  |> 
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = log_mpg, estimate = .pred_lm)
metrics(results, truth = log_mpg, estimate = .pred_rf)

### Bootstrap using rsample package

## Create bootstrap resamples
set.seed(32)
car_boot <- bootstraps(car_train)

# Evaluate the models with bootstrap resampling
lm_res <- lm_mod |> 
  fit_resamples(log_mpg ~ .,
    resamples = car_boot,
    control = control_resamples(save_pred = TRUE)
  )
lm_res |> 
  collect_metrics()

rf_res <- rf_mod |> 
  fit_resamples(
    log_mpg ~ .,
    resamples = car_boot,
    control = control_resamples(save_pred = TRUE)
  )

glimpse(rf_res)
rf_res |> 
  collect_metrics()
# Modeling Results

results <-  bind_rows(
                      lm_res |> 
                        collect_predictions() |> 
                        mutate(model = "lm"),
                      rf_res |> 
                        collect_predictions() |> 
                        mutate(model = "rf")
                      )

glimpse(results)

# Plot Results
results |> 
  ggplot(aes(log_mpg, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  coord_obs_pred() + 
  geom_point(aes(color = id), size = 1.5, alpha = 0.3, show.legend = FALSE) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(model)) + 
  theme_bw()


#####################################################################
ranger_recipe <- recipe(formula = log_mpg ~ ., data = car_train) 

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

set.seed(333)
car_folds <- vfold_cv(car_train, v = 10, repeats = 5)


set.seed(8675309)
ranger_tune <-
  tune_grid(ranger_workflow, resamples = car_folds, grid = 32)


######
ranger_tune
autoplot(ranger_tune)
show_best(ranger_tune, metric = "rmse")

ranger_param <- tibble(mtry = 6, min_n = 3)
final_ranger_wkfl <- ranger_workflow |> 
  finalize_workflow(ranger_param)
final_ranger_wkfl 

final_ranger_fit <- final_ranger_wkfl |> 
  fit(car_train)


car_test |> 
  bind_cols(predict(final_ranger_fit, car_test)) -> stuff
stuff <- stuff |> 
  relocate(.pred, .after = log_mpg)
stuff

# Test RMSE - 0.0725
rmse(stuff, log_mpg, .pred)
# R^2 = 0.889
rsq(stuff, log_mpg, .pred)
# 
metrics(stuff, log_mpg, .pred)

library(vip)
vip(final_ranger_fit) -> g1
g1
