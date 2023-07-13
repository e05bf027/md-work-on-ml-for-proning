# dt-tidy-01.R
# implementation of a c5.0 algo to predict proning mortality


# setup
rm(list = ls())

set.seed(3.14)
library(tidyverse)
library(tidymodels)
tidymodels_prefer(quiet = T)
library(hardhat)
library(rules)

# load dataframe
load(file = 'sketches-ideas/prone_session_1_cleaned.Rda')


# create data resamples
dt_v_fold <- 
  vfold_cv(data = prone_session_1,
           v = 5, 
           repeats = 5, 
           strata = mortality_28)


# make recipe
c5_recipe <-
  recipe(prone_session_1, formula = mortality_28 ~ .,) %>% 
  step_rm(patient_id, bmi) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_factor_predictors()) %>% 
  step_corr(all_numeric_predictors() , threshold = 0.8)


# make model
c5_mod <- 
  C5_rules(mode = 'classification', 
           engine = 'C5.0', 
           trees = tune(), 
           min_n = tune()) 


# create tuning grid
dt_param <- extract_parameter_set_dials(c5_mod)
dt_grid <- grid_regular(trees(),
                        min_n(), 
                        levels = 10)


# create workflow
dt_results <- 
  workflow() %>% 
  add_model(c5_mod) %>% 
  add_recipe(c5_recipe) %>% 
  tune_grid(resamples = dt_v_fold,
            grid = dt_grid)