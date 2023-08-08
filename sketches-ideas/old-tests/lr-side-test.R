# lr-side-test.R
# a script to attempt logistic regression on a refined 28-day mortality dataset
# FINDINGS - as before, good sens, crap spec

# setup
set.seed(3.14)
library(tidyverse)
library(scorecard)
library(rsample)
library(recipes)
library(yardstick)

# load and refine data - isolate only certain data
load(file = 'data/prone_session_1_cleaned.Rda')


# remove vars with >5% missing
prone_session_1 <- prone_session_1 %>% 
  select(-c(bmi,
            lymphocytes_supine,
            peak_pressure_retain_absolute,
            minute_volume_coalesced_supine,
            peak_pressure_coalesced_supine,
            peak_pressure_change_absolute))


# remove unneeded
prone_session_1 <- prone_session_1 %>% 
  select(-c(patient_id, pa_o2_supine, fi_o2_supine, weight_kg, time_between_abg))


# create train and test splits (k-fold, k=5)
folds <- 5
data_folded <- prone_session_1 %>% vfold_cv(v = folds, repeats = 2, strata = mortality_28)
rm(folds)


# get some training data
training_fold <- data_folded$splits[[1]] %>% training()


# create recipe to fill in NAs, and bake the data
lr_data_recipe <- recipe(training_fold, formula = ~ .) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_dummy(ards_type) %>% 
  prep()

training_data_lr <- bake(object = lr_data_recipe, 
                         new_data = training_fold)


# create and train the model
lr_proning_1 <- glm(data = training_data_lr,
                    formula = mortality_28 ~ .,
                    family = 'binomial')


# get test data and make predictions
testing_fold <- data_folded$splits[[1]] %>% assessment()

testing_data_lr <- bake(object = lr_data_recipe,
                        new_data = testing_fold)

testing_data_lr$mort_odds <- predict(object = lr_proning_1,
                                     newdata = testing_data_lr,
                                     type = 'response')

testing_data_lr$mort_28_prediction <- if_else(condition = testing_data_lr$mort_odds > 0.5,
                                              true = 'T', 
                                              false = 'F')



# check performance
testing_data_lr$mort_28_prediction <- factor(testing_data_lr$mort_28_prediction)

kap(data = testing_data_lr,
    truth = mortality_28,
    estimate = mort_28_prediction)  # fair

sens(data = testing_data_lr,
     truth = mortality_28,
     estimate = mort_28_prediction)  # good

spec(data = testing_data_lr,
     truth = mortality_28,
     estimate = mort_28_prediction)  # not good (~50/50)
