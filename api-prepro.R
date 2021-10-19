# API Preprocessing
# Matt Kmiecik
# Started 12 OCTOBER 2021

# Purpose: this script preprocesses and cleans up the data pulled through the 
# Redcap API (see api-calls.R)

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/arm1_annuals_parsed.RData") # arm1 annual data
load("../output/arm1_avisit1_icsi_parsed.RData") # arm1 avisit1 icsi data
load("../output/arm2_annuals_parsed.RData") # arm2 annual data
load("../output/arm2_avisit1_icsi_parsed.RData") # arm2 avisit1 icsi data
load("../output/short_annuals_parsed.RData") # shortened annual data

# Brings in CRAMPP CODES -- a document that connects record numbers across the
# arms/redcap projects
crampp_codes <- read_excel("../data/crampp-codes.xlsx", sheet = "usethis")

# Preprocessing ----

#################
#               #
# ARM 1 ANNUALS #
#               #
#################

# arm1 annuals long (initial clean)
arm1_annuals_long <- 
  arm1_annuals_parsed %>%
  filter(
    field_name %in% c(
      "annual_questionnaire_combined060416_timestamp",
      "a99ic1a",
      "a99ic1b",
      "a99ic1c",
      "a99ic1d"
      )
    ) %>%
  mutate(
    year = case_when(
      event_id == "year_1_followup_do_arm_1" ~ 1,
      event_id == "year_2_followup_arm_1" ~ 2,
      event_id == "year_3_followup_arm_1" ~ 3,
      event_id == "year_4_followup_arm_1" ~ 4,
      event_id == "year_5_followup_arm_1" ~ 5
    ),
    meas = case_when(
      field_name == "annual_questionnaire_combined060416_timestamp" ~ "timestamp",
      field_name == "a99ic1a" ~ "ic1a",
      field_name == "a99ic1b" ~ "ic1b",
      field_name == "a99ic1c" ~ "ic1c",
      field_name == "a99ic1d" ~ "ic1d"
    ),
    value = ifelse(value == "[not completed]", NA, value) # converts to NA
  ) %>%
  select(record, year, meas, value)

# arm1 annuals wide ready to go
arm1_annuals_wide <-
  arm1_annuals_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(ic1a:ic1d, ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
    ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm1r), by = c("record" = "arm1r"))

#################
#               #
# ARM 2 ANNUALS #
#               #
#################

# Long format (initial cleaning)
arm2_annuals_long <- 
  arm2_annuals_parsed %>%
  filter(
    field_name %in% c(
      "annual_questionnaire_combined060416_timestamp",
      "a99ic1a",
      "a99ic1b",
      "a99ic1c",
      "a99ic1d"
    )
  ) %>%
  mutate(
    year = case_when(
      event_id == "annual_1_arm_1" ~ 1,
      event_id == "annual_2_arm_1" ~ 2,
      event_id == "annual_3_arm_1" ~ 3,
      event_id == "annual_4_arm_1" ~ 4, # should be none
      event_id == "annual_5_arm_1" ~ 5  # should be none
    ),
    meas = case_when(
      field_name == "annual_questionnaire_combined060416_timestamp" ~ "timestamp",
      field_name == "a99ic1a" ~ "ic1a",
      field_name == "a99ic1b" ~ "ic1b",
      field_name == "a99ic1c" ~ "ic1c",
      field_name == "a99ic1d" ~ "ic1d"
    ),
    value = ifelse(value == "[not completed]", NA, value) # converts to NA
  ) %>%
  select(record, year, meas, value)

# Wide format (ready to go)
arm2_annuals_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(ic1a:ic1d, ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
  ) # probably here left_join with ss_codes to get ss number

#####################
#                   #
# SHORTENED ANNUALS #
#                   #
#####################

short_annuals_long <- 
  short_annuals_parsed %>%
  filter(
    field_name %in% c(
      "amh_todaysdate",
      "a99ic1a_ba7c0e",
      "a99ic1b_018db5",
      "a99ic1c_7fe522",
      "a99ic1d_def4e9"
    )
  ) %>% 
  mutate(
    event_id = gsub(pattern = "shortened_annual_", replacement = "", x = event_id),
    event_id = gsub(pattern = "_arm_1", replacement = "", x = event_id),
    year = event_id,
    meas = case_when(
      field_name == "amh_todaysdate" ~ "timestamp",
      field_name == "a99ic1a_ba7c0e" ~ "ic1a",
      field_name == "a99ic1b_018db5" ~ "ic1b",
      field_name == "a99ic1c_7fe522" ~ "ic1c",
      field_name == "a99ic1d_def4e9" ~ "ic1d"
    )
    ) %>%
  select(record, year, meas, value)


short_annuals_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(ic1a:ic1d, ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
  ) # probably here left_join with ss_codes to get ss number

#
# Next step is to clean up the ICSI avisit 1s to get ICSI score and timestamp 
#



  
