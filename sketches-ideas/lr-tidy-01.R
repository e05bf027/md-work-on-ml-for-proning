# lr-tidy-01.R
# at attempt at LR after becoming more educated in tidymodels

# setup
rm(list = ls())

set.seed(3.14)
library(tidyverse)
library(tidymodels)
tidymodels_prefer(quiet = T)
library(hardhat)

# load dataframe
load(file = 'sketches-ideas/prone_session_1_cleaned.Rda')


# create data resamples
lr_v_fold <- 
  vfold_cv(data = prone_session_1,
           v = 5, 
           repeats = 5, 
           strata = mortality_28)


# create recipe (NB I don't think prep() is needed if this goes in a workflow
lr_recipe <- 
  recipe(prone_session_1, formula = mortality_28 ~ .) %>% 
  step_rm(patient_id,
          bmi,
          weight_kg) %>% 
  step_dummy(all_factor_predictors(), -mortality_28) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_zv()


# MODEL
# create a model (1) - with no tuning
lr_model_01 <- 
  logistic_reg(mode = 'classification', 
               engine = 'glm')

# create a glmnet model with NO tuning
lr_model_02 <- 
  logistic_reg(mode = 'classification',
               engine = 'glmnet') %>% 
  set_args(maxit=1e+06)

# create a glmnet model with tuning
lr_model_03 <- 
  logistic_reg(mode = 'classification',
               engine = 'glmnet',
               penalty = tune(),
               mixture = 1) %>% 
  set_args(maxit=1e+06)


# create a workflow and tune
lr_workflow <- 
  workflow() %>% 
  add_model(lr_model_03) %>% 
  add_recipe(lr_recipe) 


# create a grid for hyperparameter tuning - not neeeded for model01
lr_param <- extract_parameter_set_dials(lr_model_03)
lr_grid <- grid_regular(penalty(range = c(-5, 3)), levels = 25)


# run model_01
lr_workflow <- 
  workflow() %>%
  add_model(lr_model_03) %>% 
  add_recipe(lr_recipe) %>% 
  tune_grid(resamples = lr_v_fold,
            grid = lr_grid)


# tune the model (not for model_01!)
lr_results <- 
  lr_workflow %>% 
  tune_grid(resamples = lr_v_fold,
            grid = lr_grid,
            control = control_grid(save_pred = T))


# overview of results
lr_results
lr_metrics <- collect_metrics(lr_results)



# TEST FIT
keep_pred <- control_resamples(save_pred = T, save_workflow = T)
metrics_test_01 <- metric_set(accuracy, roc_auc, kap)

test_fit <- 
  lr_workflow %>% 
  fit_resamples(resamples = lr_v_fold,
                control = keep_pred)

collect_metrics(test_fit)


# PLOT
lr_plot <- 
  lr_results %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number(),
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
  theme_light()
  
lr_plot
