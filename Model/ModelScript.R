# Building Model for Shiny App
library(tidyverse)
library(tidymodels)

cleaned_diabetic_data <- read_csv("cleaned_diabetic_data.csv")

model_data <- cleaned_diabetic_data %>% 
  select(race, gender, age, time_in_hospital, max_glu_serum, A1Cresult,
         diabetesMed, medical_specialty, num_lab_procedures,
         number_outpatient, number_emergency, number_inpatient, admission_type,
         discharge_disposition, admission_source, change, readmitted) %>% 
  mutate(across(where(is.character), factor))

set.seed(42)
# Create training and testing data
data_split <- initial_split(model_data, prop = .75, strata = readmitted)
training_data <- training(data_split)
testing_data <- testing(data_split)

model_recipe <- recipe(readmitted ~ ., data = training_data) %>% 
  step_other(admission_type, discharge_disposition, admission_source, threshold = 150) %>% 
  step_other(medical_specialty, threshold = 50) %>% 
  step_dummy(all_nominal_predictors())

# Create cross validation object
folds <- vfold_cv(training_data, v = 3)

# Create model object and tuning parameters
xgboost_mod <- boost_tree(trees = tune(), learn_rate = tune(), min_n = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgboost_grid <- grid_random(trees(), min_n(), learn_rate(),
                            size = 10)
xgboost_grid

# Create workflow and apply cross validation
xgboost_wf <- workflow() %>% 
  add_recipe(model_recipe) %>% 
  add_model(xgboost_mod)

xgboost_tune <- xgboost_wf %>% 
  tune_grid(
    resamples = folds,
    grid = xgboost_grid
  )

xgboost_param <- xgboost_tune %>% 
  select_best("roc_auc")

final_xgboost <- finalize_model(xgboost_mod, xgboost_param)

xgboost_final_wf <- workflow() %>% 
  add_recipe(model_recipe) %>% 
  add_model(final_xgboost)

xgboost_res <- last_fit(xgboost_final_wf, data_split)

xgboost_res %>% unnest(.predictions) %>% 
  conf_mat(truth = readmitted, estimate = .pred_class)
  
# Not the best fit but will go ahead and put it in the app
# Can work on better performance later if needed

saveRDS(xgboost_res, "readmission_model.rds")
