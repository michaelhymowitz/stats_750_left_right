library(tidyverse)

evs <- readRDS("../EVS Dataset Iterations/evs_post_eda.rds")

getmode <- function(v){
    v %>%
        table() %>%
        as.matrix()  %>%
        as.data.frame() %>%
        rownames_to_column() %>%
        as_tibble() %>%
        slice_max(V1) %>%
        .$rowname %>%
        head(1) # in case of ties
}


evs_imp <- evs %>%
    # zap_label() %>%
    # zap_labels() %>%
    # select(religious_person) %>%
    # https://www.statology.org/dplyr-replace-na-with-mean/
    dplyr::mutate(across(where(is.numeric), function(col) replace_na(col, median(col, na.rm=TRUE))),
                  across(where(is.ordered), function(col) replace_na(col, getmode(col))),
                  across(where(is.logical), function(col) replace_na(as.character(col), getmode(col))),
                  across(where(is.factor), function(col) replace_na(as.character(col), getmode(col))),
                  .by = country
           )

# saveRDS(evs_imp, "evs_imp.rds")

View(evs_imp)


