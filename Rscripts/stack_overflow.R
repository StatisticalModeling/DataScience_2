# Stack Overflow


library(tidyverse)
library(tidymodels)
tidymodels_prefer()

stack_overflow <- read_csv("./Data/stack_overflow.csv")

# Take a look at stack_overflow
glimpse(stack_overflow)

# First count for `remote`
stack_overflow |>  
  count(remote, sort = TRUE)

# then count for `country`
stack_overflow |>  
  count(country, sort = TRUE)

# Use the appropriate column from the data set so you can 
# plot a boxplot with remote status on the x-axis and 
# professional experience on the y-axis.

ggplot(stack_overflow, aes(x = remote, y = years_coded_job)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")


# Before you deal with the imbalance in the remote/not remote classes, 
# first split your data into training and testing sets. You create 
# subsets of your data for training and testing your model for the same 
# reasons you did before: to reduce overfitting and obtain a more accurate 
# estimate for how your model will perform on new data.

stack_overflow <- stack_overflow |> 
  mutate(remote = factor(remote, levels = c("Remote", "Not remote"))) |> 
  mutate_if(is.character, factor)


# Create stack_select dataset
stack_select <- stack_overflow |> 
  select(-respondent)

# Split the data into training and testing sets
set.seed(1234)
stack_split <- stack_select |> 
  initial_split(prop = 0.8,
                strata = remote)

stack_train <- training(stack_split)
stack_test <- testing(stack_split)

glimpse(stack_train)
glimpse(stack_test)

# Process with a recipe

library(themis)
stack_recipe <- recipe(remote ~ ., data = stack_train) |>  
  step_downsample(remote)

stack_recipe

# Once your recipe is defined, you can estimate the parameters required to actually preprocess the data, 
# and then extract the processed data. This typically isn’t necessary is you use a workflow() for modeling, 
# but it can be helpful to diagnose problems or explore your preprocessing results.

# First, prep() the recipe.
# Then, bake() the prepped recipe with new_data = NULL to see the processed training data.
# Check out the results of counting remote status after downsampling! You likely will not 
# need to prep() and bake() when building a model but you can use this to check and explore, 
# as well as to troubleshoot when things go wrong.

stack_prep <- prep(stack_recipe)
stack_down <- bake(stack_prep, new_data = NULL)

stack_down |> 
  count(remote)

# When you bake() the prepped recipe stack_prep with new_data = NULL, you extract the 
# processed (i.e. balanced) training data.

## Train Models

# Finally! It’s time to train predictive models for this data set of Stack Overflow Developer 
# Survey responses. We will specify our machine learning models with parsnip, and use workflows for convenience.


# Specify a logistic regression model using logistic_reg().
# Build a workflow() to hold your modeling components.
# Add your model specification to your workflow() before fitting.

## Build a logistic regression model
glm_spec <- logistic_reg()  |> 
  set_engine("glm")

## Start a workflow (recipe only)
stack_wf <- workflow()  |> 
  add_recipe(stack_recipe)

## Add the model and fit the workflow
stack_glm <- stack_wf |> 
  add_model(glm_spec)  |> 
  fit(data = stack_train)

# Print the fitted model
stack_glm


## Build a decision tree model with downsampling.

# Specify a decision tree regression model using decision_tree().
# Add your recipe stack_recipe to your workflow().
# Fit your workflow, after you have added your model to it.

## Build a decision tree model
tree_spec <- decision_tree()  |>          
  set_engine("rpart")  |>       
  set_mode("classification") 

## Start a workflow (recipe only)
stack_wf <- workflow()  |> 
  add_recipe(stack_recipe)

## Add the model and fit the workflow
stack_tree <- stack_wf  |> 
  add_model(tree_spec)  |> 
  fit(data = stack_train)

# Print the fitted model
stack_tree

####
results <- stack_test  |> 
  bind_cols(predict(stack_glm, stack_test)  |> 
              rename(.pred_glm = .pred_class))


# Confusion matrix for logistic regression model
results  |> 
  conf_mat(truth = remote, estimate = .pred_glm)
## Calculate accuracy
accuracy(results, truth = remote, estimate = .pred_glm)
## Calculate positive predict value
ppv(results, truth = remote, estimate = .pred_glm)
####
results <- stack_test  |> 
bind_cols(predict(stack_tree, stack_test)  |> 
            rename(.pred_tree = .pred_class))

# Confusion matrix for decision tree model
results  |> 
  conf_mat(truth = remote, estimate = .pred_tree)

## Calculate accuracy
accuracy(results, truth = remote, estimate = .pred_tree)

## Calculate positive predict value
ppv(results, truth = remote, estimate = .pred_tree)

# Confusion matrix for decision tree model
results |> 
  conf_mat(truth = remote, estimate = .pred_tree)

#### Classification model metrics
results <- stack_test  |> 
  bind_cols(predict(stack_glm, stack_test)  |> 
              rename(.pred_glm = .pred_class))  |> 
  bind_cols(predict(stack_tree, stack_test)  |> 
              rename(.pred_tree = .pred_class))
names(results)

## Calculate accuracy
accuracy(results, truth = remote, estimate = .pred_glm)
accuracy(results, truth = remote, estimate = .pred_tree)

## Calculate positive predict value
ppv(results, truth = remote, estimate = .pred_glm)
ppv(results, truth = remote, estimate = .pred_tree)
