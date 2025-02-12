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
  select(-model, -model_index)

# Fit a linear model
fit_all <- lm(mpg ~ ., data = car_vars)

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
  fit(log(mpg) ~ ., data = car_train)

# Print the model object
fit_lm |> 
  tidy()

# Build a random forest model specification
rf_mod <- rand_forest()  |> 
  set_engine("ranger")  |> 
  set_mode("regression")

# Train a random forest model
fit_rf <- rf_mod  |> 
  fit(log(mpg) ~ ., data = car_train)

# Print the model object
fit_rf 

#### Evaluate Model Performance

# Create the new columns
results <- car_train  |> 
  mutate(mpg = log(mpg))  |> 
  bind_cols(predict(fit_lm, car_train)  |> 
              rename(.pred_lm = .pred))  |> 
  bind_cols(predict(fit_rf, car_train)  |> 
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)


# We are really not interested in the performance on Training Data
# Rather we want the performance on unseen Test Data

# Create the new columns
results <- car_test  |> 
  mutate(mpg = log(mpg))  |> 
  bind_cols(predict(fit_lm, car_test)  |> 
              rename(.pred_lm = .pred))  |> 
  bind_cols(predict(fit_rf, car_test)  |> 
              rename(.pred_rf = .pred))

# Evaluate the performance
metrics(results, truth = mpg, estimate = .pred_lm)
metrics(results, truth = mpg, estimate = .pred_rf)