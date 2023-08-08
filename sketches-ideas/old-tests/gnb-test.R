# gnb-test.R
# sketches out an attempt at using the Gaussian Naive Bayes classifier

# setup
set.seed(3.14)
library(rsample)
library(tidyverse)
library(yardstick)
library(naivebayes)
library(DataExplorer)


# load data and refine to include only prone session 1
load(file = 'data/prone_session_1_cleaned.Rda')


# create train and test splits (k-fold, k=5)
folds <- 5
data_folded <- prone_session_1 %>% vfold_cv(v = folds, repeats = 1, strata = mortality_28)
rm(folds)


# create recipe
prone_gnb_recipe <- recipe(data = prone_session_1, 
                           formula = mortality_28 ~ .) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.9) %>% 
  step_rm(all_factor_predictors(),
          all_nominal_predictors()) %>% 
  step_rm(bmi, fi_o2_supine, time_between_abg, pa_o2_supine) %>% 
  step_zv() %>% 
  prep()


# get training data
fold_train <- data_folded$splits[[1]] %>% analysis() %>% 
  bake(object = prone_gnb_recipe, 
       composition = 'tibble')


# train gnb model
fold_train_predictors <- fold_train %>% select(-mortality_28) %>% as.matrix()
fold_train_outcome <- fold_train$mortality_28

gnb_fold <- gaussian_naive_bayes(x = fold_train_predictors, y = fold_train_outcome)

fold_test <- data_folded$splits[[1]] %>% assessment()
fold_test <- bake(object = prone_gnb_recipe, new_data = fold_test, composition = 'tibble')

fold_test_predictors <- fold_test %>% select(-mortality_28) %>% as.matrix()
fold_test_outcome <- fold_test$mortality_28

fold_test$pred_28_mort <- predict(gnb_fold, newdata = fold_test_predictors, type = 'class')


# check performance
fold_test$pred_28_mort <- factor(fold_test$pred_28_mort)

kap(data = fold_test,
    truth = mortality_28,
    estimate = pred_28_mort)  

sens(data = fold_test,
     truth = mortality_28,
     estimate = pred_28_mort)  

spec(data = fold_test,
     truth = mortality_28,
     estimate = pred_28_mort) 
