library(tidyverse)
# library(ranger)
library(haven)
# library(splitstackshape)
library(tidymodels)
library(doParallel)
library(vip)
library(conflicted)

evs <- readRDS("../EVS Dataset Iterations/evs_imp.rds")
boruta_formula <- readRDS("../Modeling Objects/boruta_formula.rds")

#### Environment Setup
# Preferring `tidymodels` funcs over funcs of other libraries
tidymodels_prefer()
conflict_prefer("rmse", "yardstick")
conflict_prefer("rsq", "yardstick")


#' Function: unregister_dopar
#' ---
#' Closes parallel processing, which prevents an error from occurring in
#' modeling scripts
#' 
#' Source: https://stackoverflow.com/questions/64519640/error-in-summary-connectionconnection-invalid-connection
#' ---
#' Inputs: N/A
#' ---
#' Output: N/A
unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
}


#### Data Set-Up

# Table of parties with at least 10% of survey respoindents per country
parties_with_0.1 <- readRDS("../Modeling Objects/parties_with_0.1.rds")

# Removing unused columns and rows from dataset
evs_mod <- evs %>%
    zap_label() %>%
    zap_labels() %>%
    select(-ends_with("fct_to_num")) %>% # TODO: deal with conf_armed_forces
    select(-ps_experts_decide) %>% # deselcted by boruta
    filter(political_party_pref %in% unique(parties_with_0.1$political_party_pref))

# Creating train-test split
set.seed(725559568)
evs_split <- initial_split(evs_mod, prop = 0.8, strata = country) #, strata = political_party_pref)
evs_train <- training(evs_split)
evs_test <- testing(evs_split)


#### Creating Random Forest Model

# Creating model recipe
evs_recipe <- recipe(left_right ~ ., data = evs_train) %>%
    update_role(id, weight, pop_size_weight, equilibrated_weight, new_role = "ID")

# Creating specification for the rf model using the ranger engine
rf_tune_spec <- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
) %>%
    set_mode("regression") %>%
    set_engine("ranger", importance = "permutation")

# Creating workflow for rf model
# A workflow allows for replication of a modeling process when future data is
# passed into a model
rf_tune_wf <- workflow() %>%
    add_recipe(evs_recipe) %>%
    add_model(rf_tune_spec)


#### Tuning Hyperparameters

# Creatinbg folds for 5-fold CV
set.seed(1359725511)
evs_folds <- vfold_cv(evs_train, v = 5, strata = country)

## First grid search: 10 candidate paramets in a latin hypercube grid

# Enabling parallel processing
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
doParallel::registerDoParallel()

# Tuning hyperparameters
set.seed(862861390)
rf_grid_tuned <- tune_grid(
    rf_tune_wf, # tuning the workflow
    resamples = evs_folds, # tuning the workflow on the folds
    grid = 10,
    control = control_grid(allow_par = TRUE, parallel_over = "everything")
)

# Closing parallel processing
stopCluster(cl)
showConnections()
unregister_dopar()

# saveRDS(rf_grid_tuned, "rf_grid_tuned.rds")

# Examining hyperparameter performance graphically
autoplot(rf_grid_tuned) +
    geom_smooth(se = FALSE)

# Best model by RMSE
rf_grid_tuned %>%
    show_best(metric = "rmse", n = 3) %>%
    select(mtry, min_n, mean, std_err) %>%
    rename(rmse = mean)


## Drilling in on Hyperparameter Grid

# Creating smaller, rectangular hyperparameter grid space
rf_tune_drill_grid <- grid_regular(
    mtry(range = c(40, 56)),
    min_n(range = c(2, 15)),
    levels = 5
)
rf_tune_drill_grid %>%
    head()

# Enabling parallel processing
num_cores <- detectCores() - 1
cl <- makePSOCKcluster(num_cores)
doParallel::registerDoParallel()

# Tuning hyperparameters
set.seed(1375368270)
rf_drill_tuned <- tune_grid(
    rf_tune_wf, # tuning the workflow
    resamples = evs_folds, # tuning the workflow on the folds
    grid = rf_tune_drill_grid,
    control = control_grid(allow_par = TRUE, parallel_over = "everything")
)

# Closing parallel processing
stopCluster(cl)
showConnections()
unregister_dopar()

# Examining hyperparameter performance graphically
autoplot(rf_drill_tuned)

# Best model by RMSE
rf_drill_tuned %>%
    show_best(metric = "rmse", n = 3) %>%
    select(mtry, min_n, mean, std_err) %>%
    rename(rmse = mean)


#### Selecting Best Model

# Extracting the optimal model hyperparameters from the final drilling by RMSE
best_rf_hyperparams_by_rmse <- select_best(rf_drill_tuned, "rmse")
best_rf_hyperparams_by_rmse %>%
    select(-.config)


#### Finalizing Best Model

# Finalizing the best model by RMSE
final_rf_spec <- finalize_model(
    rf_tune_spec,
    best_rf_hyperparams_by_rmse
)

# Finalizing the workflow, as determined by the best model
final_rf_wf <- rf_tune_wf %>%
    update_model(final_rf_spec)

# Taking the best fit and evaluating it on the test data
final_rf_last_fit <- final_rf_wf %>%
    last_fit(evs_split)


#### Model Diagnostics

# Calculating the random forest model's metrics on the test set
rf_mod_test_metrics <- final_rf_last_fit %>%
    collect_metrics() %>%
    select(.metric, .estimate) %>%
    pivot_wider(names_from = .metric, values_from = .estimate)

# Generating predixtions on the testing dataset
evs_test_rf_preds <- final_rf_last_fit %>%
    collect_predictions()

# Random forest model's residual plot
rf_resid_plt <- ggplot(evs_test_rf_preds,
                       aes(x = left_right,
                           y = left_right - .pred)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(title = "Random Forest Residual Plot",
         x = "left_right",
         y = "Residual")

# Random forest model's QQ residual plot
rf_qq_plt <- ggplot(evs_test_rf_preds,
                    aes(sample = left_right - .pred)) +
    geom_qq() +
    geom_qq_line() +
    labs(title = "QQ-Plot of Residuals vs Normal Distribution",
         x = "Normal Quantiles",
         y = "Residual Quantiles")

# VIP for finalized random forest model
rf_vip <- final_rf_spec %>%
    set_engine("ranger", importance = "permutation") %>%
    fit(boruta_formula, data = evs_train) %>%
    vip(n = boruta_formula %>%
            as.character() %>%
            .[3] %>%
            str_split("\\+") %>%
            .[[1]] %>%
            length()) +
    labs(title = "Variable Importance Plot - Important Features in Explaining\nIndividual Left-Right Party Placement")

rf_vip +
    labs(title = "Variable Importance Plot - Important Features in Explaining\nIndividual Left-Right Party Placement")
ggsave("vip_rf.png", device = "png", width = 12, height = 15)


#' plots of data themselves
#' re-run model 2 without party preference, and with the parties left-right scale from lppips, remove conf_armed_forces
#' MF will get some stuff by break about fitting multi-level model to incorporate evs model into lppips priors
#' 

