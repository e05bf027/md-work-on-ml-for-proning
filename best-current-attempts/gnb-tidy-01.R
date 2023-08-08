# gnb-tidy-01.R
# -------------
# this script fits, predicts, and evaluates the performance metrics of a gnb
# approach on the proning 1 data


# setup
set.seed(3.14)
library(tidyverse)
library(tidymodels)
tidymodels_prefer(quiet = T)
library(naivebayes)


# load data and refine to include only prone session 1
load(file = 'data/prone_session_1_cleaned.Rda')


# create train and test splits (k-fold, k=5)
folds <- 25
gnb_folds <- 
  prone_session_1 %>% 
  vfold_cv(v = sqrt(folds), 
           repeats = sqrt(folds), 
           strata = mortality_28)


# create recipe
gnb_recipe <- 
  prone_session_1 %>% 
  recipe(formula = mortality_28 ~ .) %>% 
  step_rm(c('patient_id', 
            'bmi', 
            'fi_o2_supine',
            'minute_volume_coalesced_supine',
            'peak_pressure_coalesced_supine',
            'peak_pressure_coalesced_prone',
            'peak_pressure_coalesced_supine_post')) %>% 
  step_dummy(all_factor_predictors(), 
             -mortality_28) %>% 
  step_corr(all_numeric_predictors(), 
            threshold = 0.8) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize() %>% 
  prep()


# initiate empty df with accuracy measures
gnb_performance <- tibble(fold = 0,
                          accuracy = 0, 
                          sensitivity = 0, 
                          specificity = 0,
                          recall = 0,
                          # roc_auc = 0, 
                          f_measure = 0, 
                          kappa = 0)


# loop to train, test, create measures of gnb models
for (i in 1:folds) {
  fold_train <- gnb_data_folded$splits[[i]] %>% analysis()
  fold_train <- bake(object = prone_gnb_recipe, new_data = fold_train, composition = 'tibble')
  
  fold_train_predictors <- fold_train %>% select(-mortality_28) %>% as.matrix()
  fold_train_outcome <- fold_train$mortality_28
  
  gnb_fold <- gaussian_naive_bayes(x = fold_train_predictors, y = fold_train_outcome)
  
  fold_test <- gnb_data_folded$splits[[i]] %>% assessment()
  fold_test <- bake(object = prone_gnb_recipe, new_data = fold_test, composition = 'tibble')
  
  fold_test_predictors <- fold_test %>% select(-mortality_28) %>% as.matrix()
  fold_test_outcome <- fold_test$mortality_28
  
  fold_test$pred_outcome <- predict(gnb_fold, newdata = fold_test_predictors, type = 'class')
  fold_test$pred_prob <- predict(gnb_fold, newdata = fold_test_predictors, type = 'prob')[, 1]
  
  # metrics
  acc_test <- accuracy(data = fold_test, truth = mortality_28, estimate = pred_outcome)
  sens_test <- sensitivity(data = fold_test, truth = mortality_28, estimate = pred_outcome)
  spec_test <- specificity(data = fold_test, truth = mortality_28, estimate = pred_outcome)
  recall_test <- recall(data = fold_test, truth = mortality_28, estimate = pred_outcome)
  # roc_auc_test <- roc_auc(data = fold_test, truth = mortality_28, estimate = pred_prob)
  fmeas_test <- f_meas(data = fold_test, truth = mortality_28, estimate = pred_outcome)
  kappa_test <- kap(data = fold_test, truth = mortality_28, estimate = pred_outcome)
  
  gnb_performance <- add_row(gnb_performance, 
                             fold = i,
                             accuracy = round(acc_test$.estimate, 2), 
                             sensitivity = round(sens_test$.estimate, 2),
                             specificity = round(spec_test$.estimate, 2),
                             recall = round(recall_test$.estimate, 2),
                             # roc_auc = round(roc_auc_test$.estimate, 2),
                             f_measure = round(fmeas_test$.estimate, 2),
                             kappa = round(kappa_test$.estimate, 2))
}


# tidy the accuracy params tibble
gnb_performance <- gnb_performance[2:(folds + 1), ]


# tidy
rm(i,
   prone_gnb_recipe,
   folds,
   fold_test_outcome,
   fold_train_outcome,
   acc_test, 
   fmeas_test,
   kappa_test,
   # roc_auc_test,
   sens_test,
   spec_test,
   fold_test,
   fold_test_predictors, 
   fold_train, 
   fold_train_predictors,
   gnb_data_folded)
