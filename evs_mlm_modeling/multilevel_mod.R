library(tidyverse)
library(haven)
library(tidymodels)
# library(doParallel)
# library(vip)
library(conflicted)
library(lmerTest)
library(multilevelmod)
library(ggExtra)
library(ggflags)

# https://multilevelmod.tidymodels.org/articles/multilevelmod.html

evs <- readRDS("../EVS Dataset Iterations/evs_imp.rds")
boruta_formula <- readRDS("../Modeling Objects/boruta_formula.rds")
evs_countries <- readRDS("evs_countries.rds")

#### Environment Setup
# Preferring `tidymodels` funcs over funcs of other libraries
tidymodels_prefer()
conflict_prefer("rmse", "yardstick")
conflict_prefer("rsq", "yardstick")


#### Data Set-Up

# Table of parties with at least 10% of survey respoindents per country
parties_with_0.1 <- readRDS("../Modeling Objects/parties_with_0.1.rds")

# Removing unused columns and rows from dataset
evs_mod <- evs %>%
    zap_label() %>%
    zap_labels() %>%
    select(-ends_with("fct_to_num")) %>% # TODO: deal with conf_armed_forces
    select(-ps_experts_decide) %>% # deselcted by boruta
    filter(political_party_pref %in% unique(parties_with_0.1$political_party_pref)) %>%
    select(-weight, -pop_size_weight, -equilibrated_weight)

# saveRDS(evs_mod, "evs_mod.rds")

# Creating train-test split
set.seed(725559568)
evs_split <- initial_split(evs_mod, prop = 0.8, strata = country) #, strata = political_party_pref)
evs_train <- training(evs_split)
evs_test <- testing(evs_split)



## Generalized estimator equations (GEE)

evs_gee_formula <- evs_train %>%
    select(-c(id, left_right, political_party_pref, country)) %>%
    colnames() %>%
    str_c(collapse = " + ") %>%
    str_c("left_right ~ ", .) %>%
    str_c(" + id_var(political_party_pref)") %>% # TODO: cant use 2 levels
    as.formula

evs_gee_spec <- 
    linear_reg() %>% 
    set_engine("gee", corstr = "exchangeable")

evs_gee_fit <- evs_gee_spec %>% 
    fit(evs_gee_formula, data = evs_train)



## Linear mixed effects

lmer_spec <-  linear_reg() %>% 
    set_engine("lmer")

lmer_formula <- evs_train %>%
    select(-c(id, left_right, political_party_pref, country)) %>%
    colnames() %>%
    str_c(collapse = " + ") %>%
    str_c("left_right ~ ", .) %>%
    str_c(" + (1 | country/political_party_pref)") %>%
    as.formula

lmer_wflow <-  workflow() %>% 
    add_variables(outcomes = left_right,
                  predictors = evs_train %>%
                      select(-c(id, left_right)) %>%
                      colnames()) %>% 
    add_model(lmer_spec, formula = lmer_formula)

lmer_fit <- lmer_wflow %>%
    fit(data = evs_train) %>%
    extract_fit_engine()
lmer_fit

# Residual plots
predict(lmer_fit, evs_test) %>% 
    enframe(value = ".pred") %>%
    bind_cols(evs_test) %>%
    ggplot() +
    geom_point(aes(x = left_right, y = left_right - .pred))

pred_vs_actual_lr_per_party_plt <- predict(lmer_fit, evs_test) %>% 
    enframe(value = ".pred") %>%
    bind_cols(evs_test) %>%
    group_by(country, political_party_pref) %>%
    summarize(mean_actual_lr = mean(left_right),
              mean_pred_lr = mean(.pred),
              .groups = "drop") %>%
    left_join(evs_countries, by = "country") %>%
    ggplot(aes(x = mean_actual_lr, y = mean_pred_lr)) + 
    geom_point() + # need to add geom_point() for ggMarginal()
    geom_flag(aes(country = str_to_lower(country_code))) +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 10)) +
    labs(x = "Mean Actual Left-Right",
         y = "Mean Predicted Left-Right")
ggMarginal(pred_vs_actual_lr_per_party_plt, type = "histogram")
    
    

# Summary outputs
summary(lmer_fit) # REML = restricted maximum likelihood method
fixef(lmer_fit)
# confint(lmer_fit) # takes a while to output
ranef(lmer_fit) # random effects estimates, all countries have 0

# Plots of random effects per party
lmer_fit %>%
    ranef() %>%
    .$`political_party_pref:country` %>%
    rownames_to_column(var = "political_party") %>%
    mutate(country_code = str_sub(political_party, end = 2),
           political_party = str_extract(political_party, "^[a-z]{2}: [^:]+")) %>%
    arrange(`(Intercept)`) %>%
    mutate(political_party = factor(political_party, levels = .$political_party)) %>%
    ggplot(aes(x = `(Intercept)`, y = political_party)) +
    geom_col() +
    geom_point() +
    geom_flag(aes(country = str_to_lower(country_code)))

lmer_fit %>%
    ranef() %>%
    .$`political_party_pref:country` %>%
    rownames_to_column(var = "political_party") %>%
    mutate(country_code = str_sub(political_party, end = 2),
           political_party = str_extract(political_party, "^[a-z]{2}: [^:]+")) %>%
    arrange(country_code, `(Intercept)`) %>%
    mutate(political_party = factor(political_party, levels = .$political_party)) %>%
    ggplot(aes(x = `(Intercept)`, y = political_party)) +
    facet_wrap(~country_code, scale = "free_y") +
    geom_col() +
    geom_point() +
    geom_flag(aes(country = str_to_lower(country_code)))


#' how consistent are political parties with views of members?
#' in what ways are these rankings consist with each others, do parties and constituents see eye to eye
#' can look at coefficients of models and see what are important, bonferroni correction
#' something about income inequality
#' traditionally left has been party of working class, but lately right wing parties have become populists and have more economic ideologies, specially thru scapegoating immigrants and elites
#' can see how left-wing voters differ across countries, same with right-wing voters
#' can look at dfiferences between countries, try to explain with the policies they have
#' talk clearly about data sources, document them
#' 10-15 pages, aim for 15
#' structure: series of drafts
#' first draft by end of march

#' Abstract:
#' Introduction:
#' - set the stage for the importance of political scales in understanding politics, hence why so much research is dedicated to it and why it is a dominating lens thru which politics is viewed, particularly wiht how little people actually follow politics closely
#' - parties' placements on political spectrums are essentially a cue of where to vote
#' Methods:
#' Data:
#' Previous Research (can prob go within each question)
#' Question 1:
#' Question 2:
#' Question 3:
#' Discussion:
#' Future Research / Limitations:
#' References:





