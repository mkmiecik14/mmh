# API Preprocessing
# Matt Kmiecik
# Started 12 OCTOBER 2021

# Purpose: this script preprocesses and cleans up the data pulled through the 
# Redcap API (see api-calls.R)

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/arm1_annuals_parsed.RData") # arm1 annual data
load("../output/arm1_avisit1_icsi_parsed.RData") # arm1 avisit1 icsi data

# Preprocessing ----
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
    event_id = case_when(
      event_id == "year_1_followup_do_arm_1" ~ 1,
      event_id == "year_2_followup_arm_1" ~ 2,
      event_id == "year_3_followup_arm_1" ~ 3,
      event_id == "year_4_followup_arm_1" ~ 4,
      event_id == "year_5_followup_arm_1" ~ 5
    ),
    
  )
