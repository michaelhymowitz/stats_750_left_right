#' Making parties table

library(tidyverse)
library(gt)
library(cowplot)

# Dataset used for EVS modeling
evs_mod <- readRDS("../EVS Dataset Iterations/evs_mod.rds")

# Contains proper party names
evs_cmp_map <- read_csv("../Other Data/evs_parties.csv")

# Table of parties with at least 10% of survey respoindents per country
parties_with_0.1 <- readRDS("../Modeling Objects/parties_with_0.1.rds")

#' Function: gt_theme_538_mod
#' https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset1=espn-table&panelset2=table2&panelset3=theme-code2&panelset4=table-code3&panelset5=table-code4
#' ---
#' `gt` theme that mimics 538 tables
#' Slightly different than `gtExtras::gt_theme_538()` (hence "_mod")
#' ---
#' Inputs:
#'     (data): `gt_tbl` table
#'     (...): additional arguments to be passed to `gt::tab_options()`
#' --
#' Output:
#'     `data` with mimicked 538 theme
gt_theme_538_mod <- function(data, ...) {
    data %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            ...
        )
}

n_per_party <-evs_mod %>%
    group_by(country, political_party_pref) %>%
    summarize(n = n(),
              .groups = "drop")

cmp_abbrev_to_country <- evs_cmp_map %>%
    mutate(abbrev = str_extract(cmp_party_code_name, "^[a-z]+")) %>%
    distinct(abbrev, country) %>%
    filter(!is.na(abbrev))

make_country_row_groups <- function(gt_df, cmp_abbrev_to_country_in) {
    
    # must do backwards so that resulting table is ordered properly
    for (i in nrow(cmp_abbrev_to_country_in):1) {
        gt_df <- gt_df %>%
            tab_row_group(
                label = cmp_abbrev_to_country_in$country[i],
                rows = str_detect(party, str_glue("^{cmp_abbrev_to_country_in$abbrev[i]}:"))
            )
    }
    return(gt_df)
}


party_sizes <- parties_with_0.1 %>%
    left_join(n_per_party, by = c("country", "political_party_pref")) %>%
    left_join(evs_cmp_map, by = c("country", "political_party_pref")) %>%
    select(country, political_party_pref, cmp_party_code_name, n) %>%
    mutate(party =
               case_when(
                   political_party_pref == "fr: the republic onwards" ~ "fra: The Republic Onwards",
                   political_party_pref == "es: we can" ~ "esp: We Can",
                   political_party_pref == "es: citizens" ~ "esp: Citizens",
                   TRUE ~ cmp_party_code_name
               )) %>%
    select(party, n)

party_sizes_pt1 <- party_sizes %>%
    slice(1:17)

party_sizes_pt2 <- party_sizes %>%
    slice(18:32)

party_sizes_pt3 <- party_sizes %>%
    slice(33:47)

make_party_n_gt <- function(df, include_title = FALSE) {
    
    df %>%
        gt() %>%
        cols_label(
            party = md("**Party**"),
            n = md("**N**")
        ) %>%
        make_country_row_groups(cmp_abbrev_to_country) %>%
        {
            if (include_title) {
                (.) %>%
                    tab_header(
                        title = html("Political Parties and Sample Sizes<br>in Modeling Dataset")
                    )
            } else {
                (.)
            }
        } %>%
        gt_theme_538_mod()
}

party_sizes_pt1 %>%
    make_party_n_gt() %>%
    gtsave("party_sizes_pt1.png")

party_sizes_pt2 %>%
    make_party_n_gt(include_title = TRUE) %>%
    gtsave("party_sizes_pt2.png")

party_sizes_pt3 %>%
    make_party_n_gt() %>%
    gtsave("party_sizes_pt3.png")

party_sizes_pt1_ggdraw <- ggdraw() + draw_image("party_sizes_pt1.png", scale = 0.8)
party_sizes_pt2_ggdraw <- ggdraw() + draw_image("party_sizes_pt2.png", scale = 0.8)
party_sizes_pt3_ggdraw <- ggdraw() + draw_image("party_sizes_pt3.png", scale = 0.8)
plot_grid(party_sizes_pt1_ggdraw, party_sizes_pt2_ggdraw, party_sizes_pt3_ggdraw, nrow = 1, align = "h", axis = "b")


