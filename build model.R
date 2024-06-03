library(tidymodels) 
tidymodels_prefer() 

set.seed(17) 
split <- initial_split(model_data, prop = 0.75, strata = seq_length)
train <- training(split) 
test <- testing(split) 

set.seed(18)
folds <- 
  vfold_cv(train, strata = seq_length, repeats = 5) 

rf_recipe <- 
  recipe(mean_exp_rv ~., data = train) 

rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") 

rf_wflow <- 
  workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_spec) 

set.seed(19)
rf_grid <- 
  grid_latin_hypercube(
    min_n(), 
    mtry(range = c(3,10)), 
    trees(), 
    size = 50
  )

options(mc.cores = parallel::detectCores())
set.seed(20) 
rf_tune <- 
  rf_wflow %>% 
  tune_grid(
    resamples = folds, grid = rf_grid, 
    metrics = metric_set(rmse, rsq)
  )

show_best(rf_tune, metric = "rmse")
show_best(rf_tune, metric = "rsq")  
# Both metrics are improved by having fewer mtry and more min_n 
# No. of trees doesn't seem to matter much, so we'll go with the fewest

set.seed(21) 
final_rf <- ranger(mean_exp_rv ~., data = train, 
                   num.trees = 220, 
                   mtry = 4, 
                   importance = "impurity",
                   min.node.size = 40) 
barplot(importance(final_rf))
# Model relies heavily on the first pitch type 

results <- predict(final_rf, test) 

test <- 
  test %>% 
  mutate(pred_mean_rv = results$predictions) 
