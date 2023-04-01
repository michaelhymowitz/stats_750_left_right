library(tidyverse)
library(labelled)
library(haven)

# Reading in "Locating Political Parties in Policy Space" Data
lppips_raw <- read_dta("../Locating Political Parties in Policy Space Data/lr_pola_f_k_2014b_updated2.dta")

# Reading in `evs_pre_eda.rds`
evs <- readRDS("../EVS Dataset Iterations/evs_pre_eda.rds")

# Vector of Western European countries that are available in the dataset
# https://en.wikipedia.org/wiki/Western_Europe
western_europe_countries <-
    c("austria", "denmark", "finland", "france", "germany", "great britain",
      "iceland", "italy", "netherlands", "norway", "portugal", "spain",
      "sweden", "switzerland")

lppips <- lppips_raw %>%
    filter(countryname %in% str_to_title(western_europe_countries)) %>%
    mutate(year = date %/% 100) %>%
    select(countryname, edate, year, party, partyname, lr_gen_fk) %>%
    rename(country = countryname,
           election_date = edate,
           cmp_party_code = party,
           party_name = partyname,
           lppips_lr = lr_gen_fk) %>%
    mutate(country = str_to_lower(country),
           cmp_party_code_name = labelled::to_character(cmp_party_code)) %>%
    group_by(country, cmp_party_code) %>%
    filter(election_date == max(election_date)) %>%
    ungroup()
# saveRDS(lppips, "lppips.rds")

lppips_parties <- lppips %>%
    select(country, cmp_party_code, cmp_party_code_name) %>%
    arrange(country, cmp_party_code_name)

# evs parties with at least 5% of country's respondents
evs_parties <- evs %>%
    select(country, political_party_pref, left_right) %>%
    na.omit() %>%
    
    # calculating number of survey respondents per country
    group_by(country) %>%
    mutate(respondents_per_country = n()) %>%
    ungroup() %>%
    
    # calculating number of survey respondents per party
    group_by(country, political_party_pref) %>%
    mutate(respondents_per_party_pref = n()) %>%
    ungroup() %>%
    
    # only selecting parties that make up at least 5% of respondents in country
    filter(respondents_per_party_pref / respondents_per_country >= 0.05) %>%
    mutate(country = droplevels(country),
           political_party_pref = droplevels(political_party_pref)) %>%
    select(country, political_party_pref) %>%
    distinct() %>%
    arrange(country, political_party_pref) %>%
    filter(country %in% western_europe_countries) %>%
    mutate(country = as.character(country),
           political_party_pref = as.character(political_party_pref))



evs_lppips_joined_raw <- read_csv("../wvs7_evs5/evs_parties.csv")
evs_lppips_joined <- evs_lppips_joined_raw %>%
    mutate(need_to_check = case_when(
        !is.na(need_to_check) & need_to_check ~ need_to_check,
        is.na(cmp_party_code_name) ~ TRUE,
        !is.na(cmp_party_code_name) ~ FALSE
    )) %>%
    select(-cmp_party_code_name) %>%
    left_join(lppips, by = c("country", "cmp_party_code")) %>%
    select(country, year, election_date, cmp_party_code, political_party_pref, party_name,
           cmp_party_code_name, need_to_check, lppips_lr) %>%
    inner_join(evs_parties.1, by = c("country", "political_party_pref")) %>%
    rename(manifesto_year = year,
           manifesto_election_date = election_date,
           evs_party = political_party_pref)
saveRDS(evs_lppips_joined, "evs_lppips_joined.rds")

#' Questions
#' Is it a problem that im using more recent survey data?
#' TODO: some parties not in manifesto
