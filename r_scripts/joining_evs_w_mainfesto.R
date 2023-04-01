library(tidyverse)
library(manifestoR)

mp_setapikey(key = "0f38735f15c0eeddec1c35794c214d4a")

evs_lppips_joined <- readRDS("../EVS Dataset Iterations/evs_lppips_joined.rds")


# get CMP party-year rile data
cmp_riles <- mp_maindataset(south_america = FALSE) %>%
    select(countryname, edate, party, rile) %>%
    mutate(countryname = str_to_lower(countryname))

lr_scores <- evs_lppips_joined %>%
    left_join(cmp_riles,
              by = c("country" = "countryname",
                     "manifesto_election_date" = "edate",
                     "cmp_party_code" = "party"))
saveRDS(lr_scores, "lr_scores.rds")
