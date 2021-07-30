# CRAMPP Annual Questionnaires
# Matt Kmiecik
# 26 July 2021

# Purpose: determination fo sample size and estimates needed for grant proposal;
# break into other scripts when ready

source("r-prep.R") # Prepare R workspace

# Loads in data/prepares ----

# Subject codes with shortened annual record ids
crampp_codes <- read_excel(path = "../data/crampp-codes.xlsx", sheet = "usethis") 

# Long annual data from ARM 1
long_annual_data <- read_csv(file = "../data/long-annual-data.csv")

# Year 2 annuals from CRAMPP long survey with ROME and ICSI
long_year_2 <- 
  long_annual_data %>%
  select(
    record_number, 
    redcap_event_name, 
    timestamp = annual_questionnaire_combined060416_timestamp,
    ssid = a99subject_id,
    contains("rome"),
    a99ic1a,
    a99ic1b,
    a99ic1c,
    a99ic1d
    ) %>%
  filter(redcap_event_name %in% "year_2_followup_arm_1") %>%
  left_join(., crampp_codes, by = c("record_number" = "arm1r")) %>%
  select(ss, record_number, redcap_event_name:notes3)

long_year_2_icsi <-
  long_year_2 %>%
  select(ss, contains("ic")) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(-ss) %>%
  group_by(ss) %>%
  summarise(year_2_icsi = sum(value), n = n())


# Shortened annual data
short_annual_data <- read_csv(file = "../data/short-annual-data.csv")

short_year_2 <- 
  short_annual_data %>%
  filter(redcap_event_name %nin% "contact_info_arm_1") %>%
  select(
    record_id, 
    redcap_event_name,
    timestamp = shortened_annual_questionnaire_timestamp,
    contains("rome"),
    contains("ic1")
    ) %>%
  filter(redcap_event_name %in% "shortened_annual_2_arm_1") %>%
  left_join(., crampp_codes, by = c("record_id" = "shortannualsr")) %>%
  select(ss, record_id, redcap_event_name:notes3)
  
short_year_2_icsi <-
  short_year_2 %>%
  select(ss, contains("ic"), -a99ic1e_sxtotal_dfe15d) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(-ss) %>%
  group_by(ss) %>%
  summarise(year_2_icsi = sum(value), n = n())

# Next step is to get the arm2 people
long_annual_arm2 <- read_csv(file = "../data/long-annual-data-arm2.csv")

long_annual_arm2_data <- 
  long_annual_arm2 %>%
  select(
    record_number, 
    redcap_event_name, 
    timestamp = annual_questionnaire_combined060416_timestamp,
    contains("rome"),
    a99ic1a,
    a99ic1b,
    a99ic1c,
    a99ic1d
  ) %>%
  filter(redcap_event_name %in% c("annual_1_arm_1", "annual_2_arm_1")) %>% # YEAR 1 AND 2
  left_join(., crampp_codes, by = c("record_number" = "arm2r")) %>%
  select(ss, record_number, redcap_event_name:notes3)

# looking at ICSI
test <- 
  long_annual_arm2_data %>%
  select(ss, redcap_event_name, contains("ic")) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(c(-ss, -redcap_event_name)) %>%
  group_by(ss, redcap_event_name) %>%
  summarise(icsi = sum(value), n = n())
  
  