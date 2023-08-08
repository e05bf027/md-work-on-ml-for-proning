# svm-tidy-01.R
# tuned tidymodel for an SVM on the proning data


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
svm_v_fold <- 
  vfold_cv(data = prone_session_1,
           v = 5, 
           repeats = 5, 
           strata = mortality_28)


# make recipe
svm_recipe <-
  recipe(prone_session_1, formula = mortality_28 ~ .,) %>% 
  step_rm(patient_id) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_factor_predictors()) %>% 
  step_corr(all_numeric_predictors() , threshold = 0.8) %>% 
  step_impute_bag(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

svm_recipe_2 <-
  recipe(prone_session_1, formula = mortality_28 ~ .,) %>% 
  step_rm(patient_id) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_factor_predictors()) %>% 
  step_corr(all_numeric_predictors() , threshold = 0.9) %>% 
  step_impute_bag(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(threshold = 0.75, num_comp = 3)

svm_recipe_3 <- 
  recipe(prone_session_1, formula = mortality_28 ~ .,) %>%
  step_rm(patient_id, 
          bmi,
          weight_kg,
          time_between_abg,
          aa_gradient_paco2_prone,
          aa_gradient_paco2_supine_post,
          peak_pressure_coalesced_prone,
          peak_pressure_coalesced_supine_post,
          pf_ratio_prone,
          pf_ratio_supine_post) %>%
  step_dummy(all_factor_predictors()) %>%
  step_impute_bag(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors() , threshold = 0.8) 


# model creation (rbf)
svm_rbf_mod <- 
  svm_rbf(mode = 'classification', 
          engine = 'kernlab', 
          cost = tune(), 
          rbf_sigma = tune(), 
          margin = tune())

# model creation (poly)
svm_poly_mod <- 
  svm_poly(mode = 'classification',
           engine = 'kernlab', 
           cost = tune(), 
           degree = tune(), 
           scale_factor = tune(),
           margin = tune())

# model creation (linear)
svm_linear_mod <- 
  svm_linear(mode = 'classification', 
             engine = 'kernlab', 
             cost = tune(), 
             margin = tune())


# create tuning grid
svm_param <- extract_parameter_set_dials(svm_rbf_mod)
svm_grid <- grid_regular(cost(range = c(1, 100)), 
                         rbf_sigma(range = c(0.01, 10)),
                         svm_margin(range = c(0.01, 1)),
                         levels = 10)

svm_grid_poly <- 
  grid_regular(cost(range = c(1, 100)),
               degree(),
               scale_factor(),
               svm_margin(range = c(0.01, 1)),
               levels = 10)


# create a workflow
svm_results<- 
  workflow() %>% 
  add_model(svm_rbf_mod) %>% 
  add_recipe(svm_recipe_3) %>% 
  tune_grid(resamples = svm_v_fold,
            grid = svm_grid)
  