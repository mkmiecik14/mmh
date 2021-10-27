# API Preprocessing
# Matt Kmiecik
# Started 12 OCTOBER 2021

# Purpose: this script preprocesses and cleans up the data pulled through the 
# Redcap API (see api-calls.R)

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/arm1_annuals_parsed.RData") # arm1 annual data
load("../output/arm1_avisit1_api_parsed.RData") # arm1 avisit1 icsi data
load("../output/arm2_annuals_parsed.RData") # arm2 annual data
load("../output/arm2_avisit1_api_parsed.RData") # arm2 avisit1 icsi data
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

# cleans up the annual data
arm1_annuals_clean <- 
  arm1_annuals_wide %>% 
  select(ss, year, timestamp, icsi) %>%
  # subject 196 was given avisit2 on year 1 and vice versa, so these are swapped
  mutate(
    year = ifelse(ss == 196 & year == 1, 22, year), # can't figure out a better way
    year = ifelse(ss == 196 & year == 2, 1, year),
    year = ifelse(ss == 196 & year == 22, 2, year),
    # annual visit 3 for ss196 was accidentally completed and removed here:
    ss = ifelse(ss == 196 & year == 3, NA, ss) 
    ) %>%
  filter(complete.cases(ss))


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
arm2_annuals_wide <- 
  arm2_annuals_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(ic1a:ic1d, ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm2r), by = c("record" = "arm2r"))

arm2_annuals_clean <- 
  arm2_annuals_wide %>%
  select(ss, year, timestamp, icsi) %>%
  # ss 183 completed year 3 in shortened annual, so remove this entry:
  mutate(ss = ifelse(ss == 183 & year == 3 & is.na(timestamp), NA, ss)) %>%
  filter(complete.cases(ss))

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
    year = as.numeric(event_id),
    meas = case_when(
      field_name == "amh_todaysdate" ~ "timestamp",
      field_name == "a99ic1a_ba7c0e" ~ "ic1a",
      field_name == "a99ic1b_018db5" ~ "ic1b",
      field_name == "a99ic1c_7fe522" ~ "ic1c",
      field_name == "a99ic1d_def4e9" ~ "ic1d"
    )
    ) %>%
  select(record, year, meas, value)


# wide format
short_annuals_wide <- 
  short_annuals_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(ic1a:ic1d, ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, shortannualsr), by = c("record" = "shortannualsr"))

# cleans up short annual data
short_annuals_clean <- 
  short_annuals_wide %>%
  select(ss, year, timestamp, icsi) %>% 
  filter(complete.cases(ss))

#####################
#                   #
# Assessment Visits #
#                   #
#####################

# timestamp_cols <- 
#   arm1_avisit1_api_parsed %>% 
#   select(record, field_name, value) %>% 
#   pivot_wider(id_cols = record, names_from = field_name, values_from = value) %>%
#   select(record, contains("timestamp")) %>%
#   pivot_longer(-record, names_to = "meas", values_to = "values")
  
# Arm1 - long format
arm1_avisit1_icsi_long <- 
  arm1_avisit1_api_parsed %>%
  filter(
    field_name %in% c(
      "ic_problem_and_symptoms_indices_timestamp", 
      "ic1a", "ic1b", "ic1c", "ic1d"
      )
    ) %>%
  mutate(
    year = 0, # indicates baseline visit
    meas = ifelse(
      field_name == "ic_problem_and_symptoms_indices_timestamp", 
      "timestamp", 
      field_name
      )
    ) %>%
  select(record, year, meas, value)

# Arm 1 - wide format
arm1_avisit1_icsi_wide <- 
  arm1_avisit1_icsi_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(c(ic1a, ic1b, ic1c, ic1d), ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm1r), by = c("record" = "arm1r"))

# Arm 1 - cleaned up
arm1_avisit1_icsi_clean <- 
  arm1_avisit1_icsi_wide %>%
  select(ss, year, timestamp, icsi) %>% 
  filter(complete.cases(ss))

# Arm 2 -long format
arm2_avisit1_icsi_long <- 
  arm2_avisit1_api_parsed %>%
  filter(
    field_name %in% c(
      "todaysdate", 
      "ic1a", "ic1b", "ic1c", "ic1d"
    )
  ) %>%
  mutate(
    year = 0, # indicates baseline visit
    meas = ifelse(
      field_name == "todaysdate", 
      "timestamp", 
      field_name
    )
  ) %>%
  select(record, year, meas, value)

# Arm 2 - wide format
arm2_avisit1_icsi_wide <- 
  arm2_avisit1_icsi_long %>%
  pivot_wider(id_cols = c(record, year), names_from = meas, values_from = value) %>%
  mutate(
    timestamp = as.Date(timestamp), # converts to date
    across(c(ic1a, ic1b, ic1c, ic1d), ~as.numeric(.x)), # converts icsi meas to numeric
    icsi = ic1a+ic1b+ic1c+ic1d # sums to form the ICSI
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm2r), by = c("record" = "arm2r"))

# Arm2 - cleaned up
arm2_avisit1_icsi_clean <- 
  arm2_avisit1_icsi_wide %>%
  select(ss, year, timestamp, icsi) %>% 
  filter(complete.cases(ss))



#################################################
#                                               #
# Combines all annual ICSI data into one tibble #
#                                               #
#################################################

icsi_annual_data <- 
  bind_rows(
    arm1_avisit1_icsi_clean,
    arm2_avisit1_icsi_clean,
    arm1_annuals_clean,
    arm2_annuals_clean,
    short_annuals_clean
  ) %>%
  arrange(ss, year)

# Redcap stopped collecting timestamps at a certain point for assessment visit 1
# therefore, these are brought in externally here
avisit_1_dates <- 
  read_excel(
  path = "../data/crampp-av-dates.xlsx", 
  sheet = "for-r", 
  na = c("nc", "na", "nq", "no show", "Arm 3 Tracking Log")
  ) %>%
  # https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r
  mutate(
    across(
      .cols = c(screen_visit_date, timestamp, timestamp_edited),
      .fns = ~as.Date(as.numeric(.x), origin = "1899-12-30") # Excel -> R dates
      ),
    year = 0 # helps with the join
  )

# corrects the missing annual visit 1 dates
icsi_annual_data_fixed <- 
  left_join(
    icsi_annual_data, 
    select(avisit_1_dates, ss, year, timestamp_edited),
    by = c("ss", "year")
    ) %>%
  mutate(timestamp = if_else(is.na(timestamp), timestamp_edited, timestamp)) %>%
  select(-timestamp_edited)


# calculates days that passed from baseline
icsi_annual_days_wide <-
  icsi_annual_data_fixed %>%
  pivot_wider(
    id_cols = ss, 
    names_from = year, 
    names_prefix = "year_", 
    values_from = c(timestamp),
    values_fill = NA
    ) %>%
    select(ss, year_0, year_1, year_2, year_3, year_4, year_5) %>%
  mutate(
    across(
      .cols = year_0:year_5,
      .fns = ~as.numeric(.x-year_0),
      .names = "{.col}_days"
    )
    )

# gathers icsi timestamps in long format
icsi_timestamps <-
  icsi_annual_days_wide %>%
  select(ss, year_0:year_5) %>%
  pivot_longer(-ss, names_to = "year", values_to = "timestamp") %>%
  mutate(year = as.numeric(gsub("year_", "", year)))

# gathers icsi days from baseline in long format
icsi_days <- 
  icsi_annual_days_wide %>%
  select(ss, year_0_days:year_5_days) %>%
  pivot_longer(-ss, names_to = "year", values_to = "days_from_baseline") %>%
  mutate(year = as.numeric(regmatches(year, regexpr("[[:digit:]]", year))))

# combining everything
icsi_all <- 
  icsi_timestamps %>%
  left_join(., icsi_days, by = c("ss", "year")) %>%
  left_join(
    ., 
    select(icsi_annual_data_fixed, ss, year, icsi), 
    by = c("ss", "year")
    )

# gets rid of icsi NAs
icsi_complete <- icsi_all %>% filter(complete.cases(icsi))

test <- icsi_complete %>% filter(is.na(days_from_baseline))
#! try to find out what happened to these individuals with icsi scores but without timestamps


  



