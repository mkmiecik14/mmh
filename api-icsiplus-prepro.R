# API Preprocessing
# Matt Kmiecik
# Started 12 OCTOBER 2021

# Purpose: this script preprocesses and cleans up the data pulled through the 
# Redcap API *AFTER* initially cleaning for ICSI... 
# (see api-calls.R & api-icsi-prepro.R)

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/arm1_annuals_parsed.RData") # arm1 annual data
load("../output/arm1_avisit1_api_parsed.RData") # arm1 avisit1 icsi data
load("../output/arm2_annuals_parsed.RData") # arm2 annual data
load("../output/arm2_avisit1_api_parsed.RData") # arm2 avisit1 icsi data
load("../output/short_annuals_parsed.RData") # shortened annual data
load("../output/complete-icsi-data.Rdata") # complete ICSI api data longitudinal

# Brings in CRAMPP CODES -- a document that connects record numbers across the
# arms/redcap projects
crampp_codes <- read_excel("../data/crampp-codes.xlsx", sheet = "usethis")

#* Variables of interest across assessment visits and annual questionnaires were
#* summarized in an excel document here:
icsiplus_data_dict <- 
  read_excel(path = "../doc/annuals-compressed-data-dictionary.xlsx")

# converting into long format for easy data extraction
#icsiplus_data_dict_long <- 
icsiplus_data_dict_long <- 
  icsiplus_data_dict %>% 
  select(questionid:shortened_annual_var) %>%
  pivot_longer(-questionid, names_to = "proj", values_to = "question") %>%
  left_join(
    ., 
    select(icsiplus_data_dict, questionid, custom_name, cat_1), 
    by = "questionid"
    ) %>%
  separate(cat_1, into = c("main", "sub"), remove = FALSE)
  

####################
#                  #
# SHORTENED ANNUAL #
#                  #
####################

short_annual_data <- 
  short_annuals_parsed %>% 
  filter(field_name %in% icsiplus_data_dict_long$question) %>% # narrows down
  mutate(year = as.numeric(regmatches(event_id, regexpr("\\d", event_id)))) %>% # retrieves year
  left_join(., icsiplus_data_dict_long, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) %>%
  mutate(value = as.numeric(value)) # THIS SCREWS UP THE DATES! FIND A NEW SOLUTION


