# API calls to redcap to pull longitudinal data (annual questionnaires)
# Matt Kmiecik
# Started: 11 OCTOBER 2021

# Prepares workspace ----
source("r-prep.R") # Prepares R workspace
source("api-config.R") # Retrieves source info (e.g., token, url, etc)

# Forms available to extract ----
# data dictionaries
ocp_grant_data_dictionary <-
  read_csv(file = "../doc/ocp-grant-data-dictionary-11-OCT-2021.csv") %>%
  select(
    var_field = "Variable / Field Name",
    form = "Form Name",
    section_hdr = "Section Header",
    field_type = "Field Type",
    field_label = "Field Label"
  )

shortened_annuals_data_dictionary <-
  read_csv(file = "../doc/shortened-annual-data-dictionary-11-OCT-2021.csv") %>%
  select(
    var_field = "Variable / Field Name",
    form = "Form Name",
    section_hdr = "Section Header",
    field_type = "Field Type",
    field_label = "Field Label"
  )

forms <- unique(ocp_grant_data_dictionary$form) # forms available

as.list(forms) %>%
  map(., ~shortened_annuals_api(form = .x, record = 1))

shortened_annuals_api(form = forms[1], record = 2)


crampp_data <- vector("list", length = length(forms))
library(furrr)
plan(multiprocess)


for(i in 1:length(forms)){
  print(paste("Starting form:", forms[i], "..."))
  crampp_data[[i]] <-
    ss_codes$ss %>%
    future_map(
      ~ mcupp_api(form = forms[i], record = as.character(.x)), 
      .id = "ss", 
      .progress = TRUE
    )
  print(paste("Finished!"))
}






# Potential forms ----
daily_diaries <- "arm_1_daily_diary_screening_period"

test_data <- ss_codes  %>% filter(ss %in% c(1:10))

daily_diary_data <- 
  test_data$ss %>%
  map_dfr(~mcupp_api(form = daily_diaries, record = as.character(.x)), .id = "ss")

