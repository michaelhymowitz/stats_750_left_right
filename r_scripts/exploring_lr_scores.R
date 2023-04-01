#' Combining lr scores from EVS dataset and "Locating Political Parties in
#' Policy Space" (lppips) research
library(tidyverse)
library(ggExtra)
library(ggflags)
library(countrycode)
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

evs <- readRDS("../EVS Dataset Iterations/evs_post_eda.rds") # EVS cleaned dataset
lr_scores <- readRDS("../EVS Dataset Iterations/lr_scores.rds") # lppips lr scores

# political parties which make up >=10% of voters in a country, according to EVS
parties_with_0.1 <- readRDS("../Modeling Objects/parties_with_0.1.rds")

evs_countries <- evs %>%
    select(country) %>%
    distinct() %>%
    mutate(country_code =
               str_to_lower(
                   countrycode(country, origin = "country.name", destination = "iso2c")
               ))

# Combing EVS and lppips left-right scores
self_w_lppips_lr <- evs %>%
    group_by(country, political_party_pref) %>%
    summarize(num_respondents = n(),
              median_self_lr = median(left_right, na.rm = TRUE),
              .groups = "drop") %>%
    right_join(parties_with_0.1, by = c("country", "political_party_pref")) %>%
    
    # since evs lr score is on a scale of 1-10, and lppips lr is on a scale of 0-10,
    # we have to bring evs lr score to a 0-10 scale
    mutate(flipped_median_self_lr = 10 - median_self_lr,
           adj_flipped_median_self_lr = (10 / 9) * flipped_median_self_lr,
           median_self_lr = 10 - adj_flipped_median_self_lr) %>%
    select(-flipped_median_self_lr, -adj_flipped_median_self_lr) %>%
    
    left_join(lr_scores %>%
                  select(country, evs_party, lppips_lr),
              by = c("country" = "country",
                     "political_party_pref" = "evs_party")) %>%
    mutate(lppips_lr = as.numeric(lppips_lr),
           diff_lr = median_self_lr - lppips_lr) %>%
    
    left_join(evs_countries, by = "country")

# Pearson correlation
self_w_lppips_lr %>%
    select(ends_with("_lr")) %>%
    cor(use = "complete.obs",
        method = "pearson")

# Spearman correlation
self_w_lppips_lr %>%
    select(ends_with("_lr")) %>%
    cor(use = "complete.obs",
        method = "spearman")

# Plotting the two left-right scores
lr_plt <- self_w_lppips_lr %>%
    na.omit() %>%
    ggplot(aes(x = median_self_lr, y = lppips_lr)) +
    geom_point() + # need to add geom_point() for ggMarginal()
    geom_flag(aes(country = str_to_lower(country_code))) +
    scale_x_continuous(limits = c(0, 10)) +
    scale_y_continuous(limits = c(0, 10)) +
    labs(x = "Self-Placed Left-Right",
         y = "LPPIPS (Manifesto) Left-Right")
ggMarginal(lr_plt, type = "histogram")

# Distribution of differences of scores
summary(self_w_lppips_lr$diff_lr)
ggplot(self_w_lppips_lr, aes(x = diff_lr, y = 1)) +
    geom_violin() +
    geom_boxplot(width = 0.1)


################################################################################

summary_of_views_per_party <- evs %>%
    select(-left_right) %>% # will be joined in
    group_by(country, political_party_pref) %>%
    summarize(across(where(is.logical), mean, na.rm = TRUE),
              
              # most frequent factor level
              # https://stackoverflow.com/questions/32766325/fastest-way-of-determining-most-frequent-factor-in-a-grouped-data-frame-in-dplyr
              across(where(is.factor), function(col) names(which(table(col) == max(table(col)))[1])),
              
              across(where(is.numeric), median, na.rm = TRUE),
              .groups = "drop") %>%
    right_join(self_w_lppips_lr, by = c("country", "political_party_pref")) %>%
    select(-where(is.factor))

# cors <- summary_of_views_per_party %>%
#     filter(!is.na(lppips_lr)) %>%
#     select(where(is.numeric), -id, -contains("weight")) %>%
#     cor() %>%
#     as.data.frame() %>%
#     rownames_to_column(var = "lr_var") %>%
#     filter(lr_var %in% c("median_self_lr", "lppips_lr", "diff_lr")) %>%
#     pivot_longer(cols = where(is.numeric),
#                  names_to = "feature",
#                  values_to = "cor") %>%
#     
#     filter(lr_var != feature) %>%
#     arrange(desc(abs(cor))) %>%
#     pivot_wider(id_cols = "feature", names_from = "lr_var", values_from = "cor") %>%
#     filter(!(feature %in% c("median_self_lr", "lppips_lr", "diff_lr"))) %>%
#     mutate(diff_cor = median_self_lr - lppips_lr) %>%
#     arrange(desc(abs(diff_cor)))

# make differences a hypothesis test
cors <- summary_of_views_per_party %>%
    filter(!is.na(lppips_lr)) %>%
    select(where(is.numeric), -id, -contains("weight")) %>%
    inspect_cor() %>%
    filter(col_1 == "median_self_lr" | col_2 == "median_self_lr" |
               col_1 == "lppips_lr" | col_2 == "lppips_lr" |
               col_1 == "diff_lr" | col_2 == "diff_lr") %>%
    filter(!(str_detect(col_1, "_lr$") & str_detect(col_2, "_lr$"))) %>%
    mutate(lr_var = ifelse(str_detect(col_1, "_lr$"), col_1, col_2),
           feature = ifelse(str_detect(col_1, "_lr$"), col_2, col_1),
           .before = everything()) %>%
    select(-col_1, -col_2)

View(cors)

cors %>%
    pivot_wider(id_cols = "feature", names_from = "lr_var", values_from = "corr") %>%
    mutate(diff_cor = median_self_lr - lppips_lr) %>%
    arrange(desc(abs(diff_cor))) %>% View

#' Note that these confidence intervals are wide, so not necessarily statistically significant
#' 
#' - `private_vs_state_ownership`, `immigrant_parent`, `is_immigrant`,
#'   `conf_police_fct_to_num`, and `conf_churches_fct_to_num` all had very little
#'   lppips_lr cor, but strongly negative median_self_lr cor
#' - `pa_peacful_demonstration_fct_to_num` had much greater positive median_self_lr
#'   cor than lppips_lr cor
#' - `conf_press_fct_to_num` had little median_self_lr cor but some positive
#'   lppips_lr cor
    

################################################################################
evs_pca <- evs %>%
    select(-c(id, country, weight, pop_size_weight, equilibrated_weight)) %>%
    select(-where(is.factor)) %>%
    na.omit() %>%
    prcomp(center = TRUE, scale. = TRUE)
summary(evs_pca)
ggbiplot::ggbiplot(evs_pca)
ggbiplot::ggscreeplot(evs_pca)


# https://towardsdatascience.com/learn-principle-component-analysis-in-r-ddba7c9b1064
# important vars accoridng to pca
tibble(
    predictors = evs %>%
        select(-c(id, country, weight, pop_size_weight, equilibrated_weight)) %>%
        select(-where(is.factor)) %>%
        colnames(),
    eigenvalues = evs_pca$sdev ^ 2
) %>% View

qplot(c(1:71), evs_pca$sdev^2 / sum(evs_pca$sdev^2)) + 
    geom_line() + 
    xlab("Principal Component") + 
    ylab("Variance Explained") +
    ggtitle("Scree Plot")

################################################################################

evs <- readRDS("evs_imp.rds") %>%
    select(-conf_armed_forces) %>%
    select(-ends_with("fct_to_num"))

library(Boruta)
set.seed(1602336391)
boruta_train <- Boruta(left_right ~ . - id - country - weight - pop_size_weight - equilibrated_weight,
                       data = evs,
                       doTrace = 2)

attStats(boruta_train)

boruta_train %>%
    attStats() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    arrange(meanImp) %>%
    mutate(predictor = factor(predictor, levels = unique(.$predictor))) %>%
    ggplot(aes(y = predictor)) +
    geom_point(aes(x = meanImp, color = decision))

final_boruta <- TentativeRoughFix(boruta_train)

final_boruta %>%
    attStats() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    arrange(meanImp) %>%
    mutate(predictor = factor(predictor, levels = unique(.$predictor))) %>%
    ggplot(aes(y = predictor)) +
    geom_point(aes(x = meanImp, color = decision))

plot(final_boruta)

boruta_formula <- getConfirmedFormula(final_boruta)
# saveRDS(boruta_formula, "boruta_formula.rds")

################################################################################

selected_boruta_colnames <- final_boruta %>%
    attStats() %>%
    rownames_to_column("predictor") %>%
    as_tibble() %>%
    filter(decision == "Confirmed") %>%
    .$predictor

# saveRDS(selected_boruta_colnames, "selected_boruta_colnames.rds")

evs %>%
    select(all_of(selected_boruta_colnames))

# TODO: imputation





#'
#' start to write a couple of paragraphs (with a quick research question about an anomaly in the data) and make a couple graphs (SEND SUNDAY 2/19)
#' build a model predicting left_right, see which variables are important (VarImp) (vip)
#' build a model from manifesto data predicting the lppips score
#' see difference in variables that are important overall and for specific parties/countries
#' 
