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
  step_corr(all_numeric_predictors(), threshold = 0.8) %>% 
  step_zv()


# MODEL
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

lr_tuning_grid <- 
  grid_max_entropy(penalty(range = c(-5, 3)), size = 10)


# run model_01
lr_workflow <- 
  workflow() %>%
  add_model(lr_model_03) %>% 
  add_recipe(lr_recipe) %>% 
  tune_grid(resamples = lr_v_fold,
            grid = lr_tuning_grid,
            metrics = metric_set(f_meas, 
                                 accuracy, 
                                 kap,
                                 roc_auc, 
                                 sens, 
                                 spec))


# overview of results
lr_metrics <- collect_metrics(lr_workflow)


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
