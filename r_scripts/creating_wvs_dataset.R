library(tidyverse)
library(haven)
library(janitor)
library(labelled)
library(sjlabelled)

source("wvs_data_consts.R")

# https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp
evs_wvs_joint <- readRDS("../EVS Dataset Iterations/EVS_WVS_Joint_Rds_v4_0.rds")

evs_w5_raw <- evs_wvs_joint %>%
    filter(to_character(study) == "evs",
           wave == 5)

# Determining which questions belong only to the WVS survey based on if all of
# their answers are missing for 
wvs_cols_to_remove <- evs_w5_raw %>%
    
    # this will keep labeled columns that under the hood are characters, but are
    # labelled with numbers
    select(where(is.numeric)) %>%
    
    # in this dataset, labeled values of `< 0` are always various degrees of
    # missing
    mutate(across(everything(), function(col) col < 0)) %>%
    
    # calculating rate of missingness per column
    summarize(across(everything(), function(col) mean(col))) %>%
    pivot_longer(everything(),
                 names_to = "column_name",
                 values_to = "missingness_rate") %>%
    
    # based on observing the data, variables with over 50% missing are not from
    # the EVS dataset (either way, a variable with this much missigness would
    # eventually be removed in EDA)
    filter(missingness_rate >= 0.5) %>%
    .$column_name

evs_pre_feat_engin <- evs_w5_raw %>%
    select(-all_of(wvs_cols_to_remove)) %>%
    
    # converting the column labels to the variable names themselves, due to the
    # existing names being unhelpful
    sjlabelled::label_to_colnames() %>%
    clean_names() %>%
    mutate(country = to_character(country_iso_3166_1_numeric_code),
           .after = country_iso_3166_1_numeric_code) %>%
    mutate(political_party_pref = to_character(which_political_party_appeals_to_you_most_iso_3166_1_evs5),
           .after = which_political_party_appeals_to_you_most_iso_3166_1_evs5) %>%
        
    # selecting only cols we want to utilize and renaming them
    select(all_of(selected_colnames)) %>%
    rename(any_of(selected_colnames))

# Vector of values that will need to be converted to NA for data analysis
convert_to_na <- c(
    "not allowed to vote",
    "other answer (if volunteered only)",
    
    # All negative values are various versions of missing (known from exploring
    # every selected variable)
    evs_pre_feat_engin %>%
        
        # will deal with `political_party_pref` independently
        select(-political_party_pref) %>%
        
        as.list() %>%
        map(function(col) {
            attr(col, "labels") %>%
                .[. < 0] %>%
                names
        }) %>%
        unlist() %>%
        unname()
) %>%
    unique()

# Creating a regex pattern which if detected in `political_party_pref`, then
# that value represents missingness
political_party_pref_convert_to_na <- c(
    "party appeals to me",
    "other, please specify (write in)",
    "donÂ´t know",
    "other$",
    "not applicable",
    "no answer"
) %>%
    str_c(collapse = "|") %>%
    str_replace_all("\\(", "\\\\(") %>%
    str_replace_all("\\)", "\\\\)") %>%
    str_replace_all("\\[", "\\\\[") %>%
    str_replace_all("\\]", "\\\\]")
    

evs <- evs_pre_feat_engin %>%
    
    # converting values that represent missingness to `NA`
    mutate(
        across(where(is.labelled),
               function(col) {
                   replace(col, labelled::to_character(col) %in% convert_to_na, NA)
               }),
        political_party_pref =
            replace(political_party_pref,
                    str_detect(political_party_pref,
                               political_party_pref_convert_to_na),
                    NA)
    ) %>%
    filter(!is.na(political_party_pref)) %>%

    # removing unused labels
    labelled::drop_unused_value_labels() %>%
    
    # converting cols to more appropriate datatypes
    mutate(
        country = as.factor(country),
        
        across(# cols to be unordered factors
            c(sex,
              immigrant,
              immigrant_mother,
              immigrant_father,
              marital_status,
              employment_status,
              institution_of_occupation,
              religious_person,
              people_trustworthy,
              member_religious_org,
              member_labor_union,
              member_political_party,
              protect_environ_vs_econ_growth,
              aims_of_country_first_choice,
              willing_fight_country,
              political_party_pref),
               labelled::to_factor,
               ordered = FALSE),
        across(# cols to be ordered factors
            c(age_bucket,
              educ_level,
              importance_politics,
              importance_work,
              future_less_importance_work,
              future_more_respect_authority,
              proud_of_nationality,
              close_to_europe,
              close_to_country,
              work_duty_society,
              work_should_come_first,
              trust_other_religion,
              trust_other_nationality,
              vote_local_elections,
              vote_national_elections,
              interest_politics,
              post_materialist_index,
              men_vs_women_political_leaders,
              govt_video_surveillance,
              govt_monitor_emails,
              govt_collect_info,
              impact_of_immigrants,
              country_votes_counted,
              country_rich_buy_elections,
              pa_sign_petition,
              pa_join_boycott,
              pa_peacful_demonstration,
              pa_strike,
              conf_churches,
              conf_armed_forces,
              conf_press,
              conf_labor_unions,
              conf_police,
              conf_parliament,
              conf_civil_services,
              conf_eu,
              conf_govt,
              conf_political_parties,
              conf_major_companies,
              conf_environ_protect_movement,
              conf_justice_system,
              conf_un,
              ps_strong_leader,
              ps_experts_decide,
              ps_army_rule,
              ps_dem_political_system),
            labelled::to_factor,
            ordered = TRUE),
        across(# cols to be integers, only actual numeric vars or questions on 1-10 scale
            c(num_children,
              satis_w_life,
              justif_homosexuality,
              justif_prostitution,
              justif_abortion,
              justif_divorce,
              justif_euthanasia,
              justif_suicide,
              justif_casual_sex,
              justif_political_violence,
              justif_death_penalty,
              left_right,
              satis_political_system,
              private_vs_state_ownership,
              govt_responsibility,
              competition_good_or_harmful,
              income_equality,
              importance_of_democracy,
              dem_govts_tax_rich,
              dem_religion_interpret_laws,
              dem_people_vote,
              dem_unemployment_aid,
              dem_civil_rights,
              dem_gender_equality,
              dem_equal_incomes,
              dem_obey_rulers),
            to_integer_w_label)) %>%
    
    # converting factor columns to booleans
    # separate `mutate()` commands to preserve col ordering
    mutate(sex_is_male = labelled::to_character(sex) == "male", .after = sex) %>%
    mutate(is_immigrant = labelled::to_character(immigrant) == "i am an immigrant to this country", .after = immigrant) %>%
    mutate(immigrant_parent =
               (labelled::to_character(immigrant_mother) == "no") |
               (labelled::to_character(immigrant_father) == "no"),
           .after = immigrant_father) %>%
    mutate(has_been_married =
               labelled::to_character(marital_status) != "single/never married",
           .after = marital_status) %>%
    mutate(are_people_trustworthy =
               labelled::to_character(people_trustworthy) == "most people can be trusted",
           .after = people_trustworthy) %>%
    mutate(across(c(member_religious_org, member_labor_union, member_political_party),
                  function(col) labelled::to_character(col) == "mentioned")) %>%
    mutate(protect_environ_over_econ_growth =
               labelled::to_character(protect_environ_vs_econ_growth) == "protecting environment",
           .after = protect_environ_vs_econ_growth) %>%
    mutate(willing_fight_country =
               labelled::to_character(willing_fight_country) == "yes") %>%
    mutate(is_parent = num_children > 0, .after = num_children) %>%
    select(-c(sex, immigrant, immigrant_mother, immigrant_father,
              marital_status, people_trustworthy,
              protect_environ_vs_econ_growth, num_children)) %>%
    
    filter(!is.na(political_party_pref))
    
    

# generate_dictionary(wvs) %>% View




# TODO: not sure how to work with employment_status
# TODO: go thru and check if there are still any negative values in any columns (if add more vars)
#     map(colnames(wvs), function(col) {
#     wvs %>% select(all_of(col)) %>% deframe %>% val_labels() %>% .[. < 0]
#     }) %>% unlist %>% names %>% unique
