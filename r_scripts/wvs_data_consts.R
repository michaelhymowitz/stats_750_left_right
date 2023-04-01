

# Column names utilized in research
# Names are used to `rename()` selected columns to cleaner, clearer names
selected_colnames <- c(
    
    # ID / weight
    id = "unified_respondent_number_joint",
    country = "country",
    weight = "weight",
    pop_size_weight = "population_size_weight",
    equilibrated_weight = "equilibrated_weight_1000",
    
    # Biographical info
    sex = "sex",
    age_bucket = "age_recoded_6_intervals",
    immigrant = "respondent_immigrant_born_in_country",
    immigrant_mother = "mother_born_in_country",
    immigrant_father = "father_born_in_country",
    marital_status = "marital_status",
    num_children = "how_many_children_do_you_have",
    educ_level = "highest_educational_level_attained_respondent_recoded",
    employment_status = "employment_status_respondent",
    institution_of_occupation = "institution_of_occupation",
    religious_person = "religious_person",
    
    # General views on/in life
    importance_politics = "important_in_life_politics",
    importance_work = "important_in_life_work",
    satis_w_life = "satisfaction_with_your_life",
    people_trustworthy = "most_people_can_be_trusted",
    work_duty_society = "work_is_a_duty_towards_society",
    work_should_come_first = "work_should_come_first_even_if_it_means_less_spare_time",
    trust_other_religion = "trust_people_of_another_religion_b",
    trust_other_nationality = "trust_people_of_another_nationality_b",
    future_less_importance_work = "future_changes_less_importance_placed_on_work",
    future_more_respect_authority = "future_changes_greater_respect_for_authority",
    justif_homosexuality = "justifiable_homosexuality",
    justif_prostitution = "justifiable_prostitution",
    justif_abortion = "justifiable_abortion",
    justif_divorce = "justifiable_divorce",
    justif_euthanasia = "justifiable_euthanasia",
    justif_suicide = "justifiable_suicide",
    justif_casual_sex = "justifiable_having_casual_sex",
    justif_political_violence = "justifiable_political_violence",
    justif_death_penalty = "justifiable_death_penalty",
    proud_of_nationality = "how_proud_of_nationality",
    close_to_europe = "how_close_you_feel_continent_e_g_europe_asia_etc",
    close_to_country = "how_close_do_you_feel_to_country",
    
    # Membership
    member_religious_org = "member_belong_to_religious_organization",
    member_labor_union = "member_belong_to_labour_unions",
    member_political_party = "member_belong_to_political_parties",
    
    # Voting habits
    vote_local_elections = "vote_in_elections_local_level",
    vote_national_elections = "vote_in_elections_national_level",
    
    # General political/economic/democracy views
    political_party_pref = "political_party_pref",
    left_right = "self_positioning_in_political_scale",
    interest_politics = "interest_in_politics",
    satis_political_system = "satisfaction_with_the_political_system",
    protect_environ_vs_econ_growth = "protecting_environment_vs_economic_growth",
    aims_of_country_first_choice = "aims_of_country_first_choice",
    post_materialist_index = "post_materialist_index_4_item",
    willing_fight_country = "willingness_to_fight_for_country",
    private_vs_state_ownership = "private_vs_state_ownership_of_business",
    govt_responsibility = "government_responsibility",
    competition_good_or_harmful = "competition_good_or_harmful",
    income_equality = "income_equality",
    men_vs_women_political_leaders = "men_make_better_political_leaders_than_women_do",
    importance_of_democracy = "importance_of_democracy",
    dem_govts_tax_rich = "democracy_governments_tax_the_rich_and_subsidize_the_poor",
    dem_religion_interpret_laws = "democracy_religious_authorities_interpret_the_laws",
    dem_people_vote = "democracy_people_choose_their_leaders_in_free_elections",
    dem_unemployment_aid = "democracy_people_receive_state_aid_for_unemployment",
    dem_civil_rights = "democracy_civil_rights_protect_peoplea_t_ms_liberty_against_oppression",
    dem_gender_equality = "democracy_women_have_the_same_rights_as_men",
    dem_equal_incomes = "democracy_the_state_makes_peoples_incomes_equal",
    dem_obey_rulers = "democracy_people_obey_their_rulers",
    govt_video_surveillance = "government_has_the_right_keep_people_under_video_surveillance_in_public_areas",
    govt_monitor_emails = "government_has_the_right_monitor_all_e_mails_and_any_other_information_exchange",
    govt_collect_info = "government_has_the_right_collect_information_about_anyone_living_in_country_w",
    impact_of_immigrants = "evaluate_the_impact_of_immigrants_on_the_development_of_your_country",
    # TODO: not sure about these next 2, seems like it could be very dependent on country
    country_votes_counted = "how_often_in_countrys_elections_votes_are_counted_fairly",
    country_rich_buy_elections = "how_often_in_countrys_elections_rich_people_buy_elections",
    
    # Political action undertaken
    pa_sign_petition = "political_action_signing_a_petition",
    pa_join_boycott = "political_action_joining_in_boycotts",
    pa_peacful_demonstration = "political_action_attending_lawful_peaceful_demonstrations",
    pa_strike = "political_action_joining_unofficial_strikes",
    
    # Confidence in institutions
    conf_churches = "confidence_churches",
    conf_armed_forces = "confidence_armed_forces",
    conf_press = "confidence_the_press",
    conf_labor_unions = "confidence_labour_unions",
    conf_police = "confidence_the_police",
    conf_parliament = "confidence_parliament",
    conf_civil_services = "confidence_the_civil_services",
    conf_eu = "confidence_the_european_union",
    conf_govt = "confidence_the_government",
    conf_political_parties = "confidence_the_political_parties",
    conf_major_companies = "confidence_major_companies",
    conf_environ_protect_movement = "confidence_the_environmental_protection_movement",
    conf_justice_system = "confidence_justice_system_courts",
    conf_un = "confidence_the_united_nations",
    
    # Views on political system
    ps_strong_leader = "political_system_having_a_strong_leader",
    ps_experts_decide = "political_system_having_experts_make_decisions",
    ps_army_rule = "political_system_having_the_army_rule",
    ps_dem_political_system = "political_system_having_a_democratic_political_system"
)

# update_convert_to_na <- function(wvs, convert_to_na_pre_update) {
#     neg_labels <- wvs_clean_pt1 %>%
#         select(-party_pref) %>%
#         as.list %>%
#         map(function(col) {
#             attr(col, "labels") %>%
#                 .[. < 0] %>%
#                 names
#         }) %>%
#         unlist %>%
#         unname %>%
#         unique
#     
#     c(convert_to_na_pre_update, neg_labels) %>%
#         unique
# }

# func to convert a column to an integer data type but keep the label 
to_integer_w_label <- function(col) {
    col_int <- as.integer(col)
    var_label(col_int) <- attr(col, "label")
    return(col_int)
}


#' show me people who answer questions similarly but give different placements on left-right placement
#' differences between left-right placement and manifesto data analysis
#' try to explain why hypothesized differences are same and vice versa
#' 
#' goal could be a l-r graph where x-axis is individual lr placement and y is some var, say support for democracy, and see what shapes are appearing
#'   can also change x-axis to to self placement relative to party manifesot viewpoint
#'     
#'     
#'     
#'    
#'    
#'    
#' send something short to fredrickson before next meeting with a paragraph or so explaining it
#' 


# DONE:
#' can read about mid-rank method for ordinal factor variables
#'    average rank of each group 
#' find a dataset which has parties left-right 1-10 placement based on expert surveys or manifesto data, and join that with this data
#'     pick a couple variable variabels and look at where self and party lr placement differ 
