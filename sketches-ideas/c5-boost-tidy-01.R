# c5-boost-tidy-01.R
# ---------------------------------
# training a boosted c5 on the data


# setup
set.seed(3.14)
library(tidyverse)
library(tidymodels)
tidymodels_prefer(quiet = T)
library(hardhat)


# load dataframe
load(file = 'sketches-ideas/prone_session_1_cleaned.Rda')


# create data resamples
c5Boost_v_fold <- 
  vfold_cv(data = prone_session_1,
           v = 5, 
           repeats = 5, 
           strata = mortality_28)


# create recipe
c5Boost_recipe <- 
  recipe(prone_session_1, formula = mortality_28 ~ .,) %>% 
  step_rm(c('patient_id',
            'bmi',
            'weight_kg') %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_numeric_predictors() , threshold = 0.8) %>% 
  prep()


# make model
c5Boost_mod <- 
  boost_tree(mode = 'classification',
             engine = 'C5.0',
             trees = tune(),
             min_n = tune(),
             sample_size = tune()
             ) %>% 
  set_args(earlyStopping = FALSE) %>% 
  translate()


# create tuning grid:
# 1. finalize stuff
c5Boost_param_fin <- extract_parameter_set_dials(xgb_mod) %>% 
  finalize(juice(c5Boost_recipe))

c5Boost_grid <- 
  grid_max_entropy(trees(range = c(1, 10)),
                   min_n(),
                   sample_prop(range = c(0.1, 0.999)),
                   size = 100, 
                   variogram_range = 0.5
                   )


# create workflow
c5Boost_results <- 
  workflow() %>% 
  add_model(c5Boost_mod) %>% 
  add_recipe(c5Boost_recipe) %>% 
  tune_grid(resamples = c5Boost_v_fold,
            grid = c5Boost_grid,
            metrics = metric_set(recall, 
                                 precision, 
                                 f_meas, 
                                 accuracy, 
                                 kap,
                                 roc_auc, 
                                 sens, 
                                 spec)
            )


# collect metrics
c5Boost_metrics <- collect_metrics(c5Boost_results)
