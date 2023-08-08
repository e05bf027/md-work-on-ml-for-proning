# xgboost-tidy-01.R
# documents the construction of an xgboost model to classify response to 
# prone positioning

# setup
set.seed(3.14)
library(tidyverse)
library(tidymodels)
tidymodels_prefer(quiet = T)
library(hardhat)


# load dataframe
load(file = 'sketches-ideas/prone_session_1_cleaned.Rda')


# create data resamples
xgb_v_fold <- 
  vfold_cv(data = prone_session_1,
           v = 5, 
           repeats = 5, 
           strata = mortality_28)


# create recipe
xgb_recipe <- 
  recipe(prone_session_1, formula = mortality_28 ~ .) %>% 
  step_rm(patient_id,
          bmi,
          weight_kg, 
          fi_o2_supine,
          pa_o2_supine) %>% 
  step_dummy(all_factor_predictors(), -mortality_28) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_zv() %>% 
  prep()


# make model
xgb_mod <- 
  boost_tree(mode = 'classification',
             engine = 'xgboost',
             mtry = tune(),
             # trees = tune(),
             min_n = tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             loss_reduction = tune(),
             sample_size = tune(),
             stop_iter = tune()
             )


# create tuning grid:
# 1. finalize stuff
xgb_param_fin <- extract_parameter_set_dials(xgb_mod) %>% 
  finalize(juice(xgb_recipe))

xgb_grid <- grid_regular(mtry(range = c(1, 39)),
                         # trees(range = c(1L, 100L)),
                         min_n(),
                         tree_depth(range = c(1, 15)),
                         learn_rate(),
                         loss_reduction(),
                         sample_size(range = c(1, 1)),
                         stop_iter(), 
                         levels = 5
                         )


# create workflow
xgb_results <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_recipe(xgb_recipe) %>% 
  tune_grid(resamples = xgb_v_fold,
            grid = xgb_grid,
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
xgb_metrics <- collect_metrics(xgb_results)


# find best roc_auc (its 0.5 :( )
xgb_metrics %>% 
  filter(.metric == 'roc_auc') %>% 
  arrange(desc(.metric))
