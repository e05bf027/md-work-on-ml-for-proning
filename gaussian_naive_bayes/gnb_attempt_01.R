# gnb_attempt_01.R
# this script estimates risk of death based on patient response to initial
# placement in the prone position, using the gaussian naive bayes algorithm
# with k-fold cross-validation
# -------------------------------------------------------------------------

# setup
rm(list = ls())
library(recipes)
library(rsample)
library(tidyverse)
library(yardstick)
library(broom)
library(naivebayes)
library(DataExplorer)
set.seed(3.14)

# load df
load('data/pre_post_changes.Rda')

# split to folds
folds <- 5
prone_sess_folds <- pre_post_changes %>% vfold_cv(v = folds, strata = outcome)

# create recipe
prone_sess_recipe <- recipe(outcome ~ ., data = pre_post_changes) %>% 
  step_filter(proning_session == 1) %>% # THIS STEP IS BROKEN!
  step_rm(ends_with(c('prone', 'percent')),
          all_nominal_predictors(),
          patient_id, 
          proning_session,
          time_supine,
          time_prone,
          los_days,
          plateau_airway_pressure_d_supine,
          resistance_d_supine,
          ionised_calcium_abg_change_absolute,
          ionised_calcium_abg_change_percent,
          ionised_calcium_abg_supine,
          total_proning_sessions,
          aa_p_aco2_change_percent,
          aa_p_aco2_change_absolute,
          aa_gradient_p_aco2_supine,
          ph_abg_supine,
          ph_change_absolute,
          die_in_72,
          die_in_120,
          die_in_168) %>%
  step_zv() %>% 
  prep()

# initiate empty df with accuracy measures
gnb_performance <- tibble(fold = 0,
                          accuracy = 0, 
                          sensitivity = 0, 
                          specificity = 0, 
                          roc_auc = 0, 
                          f_measure = 0, 
                          kappa = 0)

# loop to train, test, create measures of gnb models
for (i in 1:folds) {
  fold_train <- prone_sess_folds$splits[[i]] %>% analysis()
  fold_train <- bake(object = prone_sess_recipe, new_data = fold_train, composition = 'tibble')
  
  fold_train_predictors <- fold_train %>% select(-outcome) %>% as.matrix()
  fold_train_outcome <- fold_train$outcome
  
  gnb_fold <- gaussian_naive_bayes(x = fold_train_predictors, y = fold_train_outcome)
  
  fold_test <- prone_sess_folds$splits[[i]] %>% assessment()
  fold_test <- bake(object = prone_sess_recipe, new_data = fold_test, composition = 'tibble')
  
  fold_test_predictors <- fold_test %>% select(-outcome) %>% as.matrix()
  fold_test_outcome <- fold_test$outcome
  
  fold_test$pred_outcome <- predict(gnb_fold, newdata = fold_test_predictors, type = 'class')
  fold_test$pred_prob <- predict(gnb_fold, newdata = fold_test_predictors, type = 'prob')[, 1]
  
  # metrics
  acc_test <-accuracy(data = fold_test, truth = outcome, estimate = pred_outcome)
  sens_test <- sensitivity(data = fold_test, truth = outcome, estimate = pred_outcome)
  spec_test <-specificity(data = fold_test, truth = outcome, estimate = pred_outcome)
  roc_auc_test <- roc_auc(data = fold_test, truth = outcome, estimate = pred_prob)
  fmeas_test <- f_meas(data = fold_test, truth = outcome, estimate = pred_outcome)
  kappa_test <- kap(data = fold_test, truth = outcome, estimate = pred_outcome)
  
  gnb_performance <- add_row(gnb_performance, 
                             fold = i,
                             accuracy = round(acc_test$.estimate, 2), 
                             sensitivity = round(sens_test$.estimate, 2),
                             specificity = round(spec_test$.estimate, 2),
                             roc_auc = round(roc_auc_test$.estimate, 2),
                             f_measure = round(fmeas_test$.estimate, 2),
                             kappa = round(kappa_test$.estimate, 2))
}

# tidy the accuracy params tibble
gnb_performance <- gnb_performance[2:(folds + 1), ]

# tidy
rm(i,
   folds,
   fold_test_outcome,
   fold_train_outcome,
   acc_test, 
   fmeas_test,
   kappa_test,
   roc_auc_test,
   sens_test,
   spec_test,
   fold_test,
   fold_test_predictors, 
   fold_train, 
   fold_train_predictors,
   prone_sess_folds)