#' Modeling lppips_lr scores using "Measuring Political Party Ideologies – Combining Expert Scale and Text Based Approaches"
#' variable creations

library(tidyverse)
library(manifestoR)
library(performance)
library(glmnet)
library(pls)
library(vip)
library(ggthemes)

# `theme_538_mod()` is a preferred ggplot theme that modifies
# `theme_fivethirtyeight()`
theme_538_mod <- function() {
    theme_fivethirtyeight() +
        theme(legend.position = "right",
              legend.direction = "vertical",
              axis.title = element_text(),
              axis.line.x = element_line(color = "black",
                                         linewidth = 0.5,
                                         linetype = "solid"),
              axis.line.y = element_line(color = "black",
                                         linewidth = 0.5,
                                         linetype = "solid"))
}
theme_set(theme_538_mod())

mp_setapikey(key = "0f38735f15c0eeddec1c35794c214d4a")

evs_lppips_joined <- readRDS("../EVS Dataset Iterations/evs_lppips_joined.rds")

lppips <- readRDS("../Modeling Objects/lppips.rds")

## get CMP party-year rile data
cmp <- mp_maindataset(south_america = FALSE) %>%
    mutate(countryname = str_to_lower(countryname))

## Calculating "Measuring Political Party Ideologies – Combining Expert Scale and Text Based Approaches" variables
lppips_mod_df <- evs_lppips_joined %>%
    left_join(cmp %>%
                  select(-country),
              by = c("country" = "countryname",
                          "cmp_party_code" = "party",
                          "manifesto_election_date" = "edate")) %>%
    mutate(urban_vs_rural = (per704 - per703) / (per703 + per704),
           social_liberalism = (per603 - (per503 + per604)) / (per503 + per603 + per604),
           privatization_vs_state_ownership =
               ((per412 + per413 + per4123 + per4124 + per4131 + per4132) -
               (per401 + per4011 + per4012 + per4013 + per4014)) /
               ((per412 + per413 + per4123 + per4124 + per4131 + per4132 + per401 + per4011 + per4012 + per4013 + per4014)),
           productivity_vs_environmental_protection = (per410 - (per416 + per501)) / (per410 + per416 + per501),
           decentralization_vs_centralization = (per301 + per2033 - per302) / (per301 + per302 + per2033),
           welfare_vs_taxes = (per402 + per505 - per504) / (per402 + per505 + per504),
           foreign_policy = (per101 + per107 + per1012 + per1013 + per1014 + per1015 + per1021 - (per102 + per109 + per1011 + per1022 + per1023 + per1024 + per1025 + per1026))/
               (per101 + per107 + per1012 + per1013 + per1014 + per1015 + per1021 + per102 + per109 + per1011 + per1022 + per1023 + per1024 + per1025 + per1026),
           eu = (per108 - per110) / (per108 + per110),
           deregulation = ((per403 + per412) - (per401 + per402 + per405)) / (per401 + per402 + per403 + per405 + per412),
           free_trade_vs_protectionism = (per407 - per406) / (per406 + per407),
           internationalism_vs_nationalism = ((per107 + per602 + per607 + per6072) - (per109 + per601 + per608 + per6013 + per6081))/
               (per107 + per602 + per607 + per608 + per109 + per601 + per608 + per6013 + per6081),
           regionalism_secessionism = ((per204 + per301 + per602) - (per203 + per302 + per601))/(per204 + per301 + per602 + per203 + per302 + per601),
           military = (per105 - per104) / (per105 + per104)) %>%
    select(-starts_with("per")) %>%
    mutate(across(everything(), function(col) ifelse(is.nan(col), 0, col))) %>%
    select(country:lppips_lr, urban_vs_rural:military) %>%
    mutate(row_num = row_number()) %>%
    filter(!is.na(lppips_lr)) %>%
    na.omit()

## Creating train-test split
set.seed(1519974431)
train_rows <- lppips_mod_df %>%
    group_by(country) %>%
    summarize(row_nums = list(row_num)) %>%
    select(row_nums) %>%
    deframe %>%
    map(function(row_nums_per_country) {
        
        num_train_samples <- floor(0.8 * length(row_nums_per_country))
        
        sample(
            row_nums_per_country,
            size = num_train_samples,
            replace = FALSE
        ) %>%
            sort()
    }) %>%
    unlist()

lppips_mod_train <- lppips_mod_df %>%
    filter(row_num %in% train_rows) %>%
    select(-row_num)

lppips_mod_test <- lppips_mod_df %>%
    filter(!(row_num %in% train_rows))  %>%
    select(-row_num)

## Creating modeling formula for this exercise, which is simply lppips left-right
## score as response and "Measuring Political Party Ideologies" vars as predictors
lppips_mod_formula <- str_c(
    "lppips_lr ~ ",
    str_c(
        lppips_mod_df %>%
            select(urban_vs_rural:military) %>%
            colnames(),
        collapse = " + "
    )
) %>%
    as.formula()

# Creating X and y train and test modeling objects
X_train <- model.matrix(lppips_mod_formula, lppips_mod_train)[, -1]
y_train <- lppips_mod_train$lppips_lr
X_test <- model.matrix(lppips_mod_formula, lppips_mod_test)[, -1]
y_test <- lppips_mod_test$lppips_lr


## Linear regression
lm_mod <- lm(lppips_mod_formula, lppips_mod_train)
summary(lm_mod)

lm_preds <- predict(lm_mod, lppips_mod_test)
lm_mse <- mean((lm_preds - y_test)^2)
lm_mse

model_performance(lm_mod)
check_model(lm_mod)

## Ridge regression

# Vector of candidate lambdas
lambda_grid <- 10^seq(10, -2, length=100)

# CV for lambda
set.seed(1937432654)
ridge_cv_out <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid, nfolds = 5)
plot(ridge_cv_out,
     main = "Mean-Squared Error for Lambda Ridge Regression Hyperparameter Tuning\n\n")
    

glmnet(X_train, y_train, alpha = 0, lambda = lambda_grid) %>%
    plot(xvar = "lambda", label = TRUE,
         main = "Trace Plot for Ridge Regression Cross Validation on Lambda Hyperparamter\n\n")

bestlam <- ridge_cv_out$lambda.min
bestlam

# Fitting selected ridge model
ridge_mod <- glmnet(X_train, y_train, alpha = 0, lambda = bestlam)
coef(ridge_mod)

# Getting new ridge regression coefficients
predict(ridge_mod, s = bestlam, type = "coefficients")

# Predictions
ridge_preds <- predict(ridge_mod, s = bestlam, newx = X_test)
ridge_mse <- mean((ridge_preds - y_test)^2)
ridge_mse


## PCR model
set.seed(1999751621)
pcr_mod <- pcr(lppips_mod_formula, data = lppips_mod_train, scale = TRUE, validation = "CV")
summary(pcr_mod)

# CV on components
validationplot(pcr_mod, val.type = "MSEP", legendpos = "topright")

pcr_preds <- predict(pcr_mod, lppips_mod_test %>% select(-lppips_lr), ncomp = 5)
pcr_mse <- mean((pcr_preds - y_test)^2)
pcr_mse

## Aggregating MSEs
tibble(
    Model = c("Linear Regression", "Ridge", "PCR"),
    MSE = c(lm_mse, ridge_mse, pcr_mse)
)

# So, select ridge model
# PCR model was terrible poor due to all vars being relatively important

vip(ridge_mod,
    num_features = lppips_mod_df %>%
        select(urban_vs_rural:military) %>%
        ncol(),
    geom = "col") +
    labs(title = "Variable Importance Plot - Important Features in Explaining\nManifesto-Based Left-Right Party Placement",
         subtitle = "Ridge Regression Model")
# ggsave("vip_ridge.png", device = "png", width = 12, height = 10)
