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
load("../output/arm1_year1_followup_parsed.RData") # special arm1 1 year 1 data
load("../output/arm2_annuals_parsed.RData") # arm2 annual data
load("../output/arm2_avisit1_api_parsed.RData") # arm2 avisit1 icsi data
load("../output/short_annuals_parsed.RData") # shortened annual data
load("../output/complete-icsi-data.Rdata") # complete ICSI api data longitudinal

# Some of the baseline data are stored in the screen visit; loaded here:
load("../output/arm1_screenvisit_api_parsed.RData") # arm 1 screen visit data
load("../output/arm2_screenvisit_api_parsed.RData") # arm 2 screen visit data


# Brings in CRAMPP CODES -- a document that connects record numbers across the
# arms/redcap projects
crampp_codes <- read_excel("../data/crampp-codes.xlsx", sheet = "usethis")

#* Variables of interest across assessment visits and annual questionnaires were
#* summarized in an excel document here:
icsiplus_data_dict <- 
  read_excel(path = "../doc/annuals-compressed-data-dictionary.xlsx")

# converting into long format for easy data extraction
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

# initial data processing
short_annual_data_wide <- 
  short_annuals_parsed %>% 
  filter(field_name %in% icsiplus_data_dict_long$question) %>% # narrows down
  mutate(year = as.numeric(regmatches(event_id, regexpr("\\d", event_id)))) %>% # retrieves year
  left_join(., icsiplus_data_dict_long, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) %>%
  pivot_wider(id_cols = c(record, year), names_from = custom_name, values_from = value)

# prepares for the big join
short_annual_data_wide_ready <- 
  short_annual_data_wide %>%
  mutate(date_last_period = as.Date(date_last_period)) %>% # changes into date format
  mutate(across(where(is.character), as.numeric)) %>% # converts rest to dbl
  mutate(
    bsi = bsi1+bsi2+bsi3+bsi4+bsi5+bsi6, # computes bsi
    icsi = ic1a+ic1b+ic1c+ic1d, # computes icsi
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, shortannualsr), by = c("record" = "shortannualsr")) %>%
  # rearranges table and removes redundant information (e.g., indiv bsi meas)
  select(
    ss, year, cramp_pain_no_nsaid, mens_pain_days_per_month, date_last_period,
    days_missed_3_months, days_bed_3_months, cramp_pain_no_nsaid, 
    cramp_pain_with_nsaid, bsi, urination_pain_last_week, bowel_mov_pain_last_week,
    icsi, mens_nonmens_pain_week, avg_pain, global_health
  ) %>%
  
  # CLEANS UP ERRORS! SEE `api-icsi-prepro.R` for the work; it was repeated here:
  
  mutate(
    # ss109 seems to have completed two annuals within 30 days (year 4 is wrong)
    ss = ifelse(ss == 109 & year == 4, NA, ss)
  ) %>%
  filter(complete.cases(ss)) # removes those without a ss number

#! Note that the processing pipeline changed here by using this_dict for vars

#################
#               #
# ARM 1 ANNUALS #
#               #
#################

# To prevent duplicate values when doing pivot_wide later
this_dict <- icsiplus_data_dict_long %>% filter(proj == "arm1_annual_var")

# initial data processing
arm1_annuals_wide <- 
  arm1_annuals_parsed %>% 
  filter(field_name %in% this_dict$question) %>% # narrows down
  mutate(year = as.numeric(regmatches(event_id, regexpr("\\d", event_id)))) %>% # retrieves year
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) %>%
  pivot_wider(id_cols = c(record, year), names_from = custom_name, values_from = value)

# prepares for the big join
arm1_annuals_wide_ready <-
  arm1_annuals_wide %>%
  mutate(date_last_period = as.Date(date_last_period)) %>% # changes into date format
  mutate(across(where(is.character), as.numeric)) %>% # converts rest to dbl
  mutate(
    bsi = bsi1+bsi2+bsi3+bsi4+bsi5+bsi6, # computes bsi
    icsi = ic1a+ic1b+ic1c+ic1d, # computes icsi
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm1r), by = c("record" = "arm1r")) %>%
  # rearranges table and removes redundant information (e.g., indiv bsi meas)
  select(
    ss, year, cramp_pain_no_nsaid, mens_pain_days_per_month, date_last_period,
    days_missed_3_months, days_bed_3_months, cramp_pain_no_nsaid, 
    cramp_pain_with_nsaid, bsi, urination_pain_last_week, bowel_mov_pain_last_week,
    icsi, mens_nonmens_pain_week, avg_pain, global_health
  ) %>%
  
  # CLEANS UP ERRORS! SEE `api-icsi-prepro.R` for the work; it was repeated here:
  
  # subject 196 was given avisit2 on year 1 and vice versa, so these are swapped
  mutate(
    year = ifelse(ss == 196 & year == 1, 22, year), # can't figure out a better way
    year = ifelse(ss == 196 & year == 2, 1, year),
    year = ifelse(ss == 196 & year == 22, 2, year),
    # annual visit 3 for ss196 was accidentally completed and removed here:
    ss = ifelse(ss == 196 & year == 3, NA, ss),
    # ss92 has a year 4 duplicate in year 1 (removed here)
    ss = ifelse(ss == 92 & year == 1, NA, ss),
    # ss162 year 1 and 2 should be year 2 and 3, respectively
    year = ifelse(ss == 162, year+1, year),
    # ss156 year 4 should be year 3
    year = ifelse(ss == 156 & year == 4, 3, year)
  ) %>%
  filter(complete.cases(ss)) # removes those w/o a ss number

#############################
#                           #
# ARM 1 SPECIAL YEAR 1 DATA #
#                           #
#############################

#* Some year 1 followup data are *not* located in annual questionnaire fields,
#* but rather in standard assessment visit questionnaire fields (without the
#* leading "a")

# To prevent duplicate values when doing pivot_wide later
this_dict <- 
  icsiplus_data_dict_long %>% 
  filter(proj %in% c("arm1_avisit1_var", "arm1_annual_var"))

# initial data processing
arm1_year1_followup_wide <- 
  arm1_year1_followup_parsed %>% 
  filter(field_name %in% this_dict$question) %>% # narrows down
  mutate(year = as.numeric(regmatches(event_id, regexpr("\\d", event_id)))) %>% # retrieves year
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) %>%
  # ss227 has both arm1 followup and annual...therefore we will use the annual
  # from above 
  # ss479 has annual questionnaire, but missing data in medical questionnaire
  # so we will use annual from above
  filter(record != 227, record != 479) %>%
  pivot_wider(id_cols = c(record, year), names_from = custom_name, values_from = value)

# prepares
arm1_year1_followup_wide_clean <- 
  arm1_year1_followup_wide %>%
  mutate(date_last_period = as.Date(date_last_period)) %>% # changes into date format
  mutate(across(where(is.character), as.numeric)) %>% # converts rest to dbl
  mutate(
    bsi = bsi1+bsi2+bsi3+bsi4+bsi5+bsi6, # computes bsi
    icsi = ic1a+ic1b+ic1c+ic1d, # computes icsi
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm1r), by = c("record" = "arm1r")) %>%
  # rearranges table and removes redundant information (e.g., indiv bsi meas)
  select(
    ss, year, cramp_pain_no_nsaid, mens_pain_days_per_month, date_last_period,
    days_missed_3_months, days_bed_3_months, cramp_pain_no_nsaid, 
    cramp_pain_with_nsaid, bsi, urination_pain_last_week, bowel_mov_pain_last_week,
    icsi, mens_nonmens_pain_week, avg_pain, global_health
  ) %>%
  filter(complete.cases(ss)) # removes participants w/o ss numbers

# Notes:
# arm1_year1_followup_wide_clean is the superior dataframe for arm1 year 1 annual
# however, there are a few participants that are in arm1_annuals_wide_ready
# for year 1 that are not in arm1_year1_followup_wide_clean
# therefore, an anti_join will be used to gather these participants to append
left_out <- 
  anti_join(
    arm1_annuals_wide_ready %>% filter(year == 1), 
    arm1_year1_followup_wide_clean, 
    by = c("ss", "year")
    )

# appends the left out participants
arm1_annual1_best <- bind_rows(arm1_year1_followup_wide_clean, left_out)

# saves the variable onto itself 
arm1_annuals_wide_ready <- 
  arm1_annuals_wide_ready %>% 
  filter(year!=1) %>% # filters out the year 1 data
  bind_rows(., arm1_annual1_best) %>% # puts back the better arm1 annual 1 data
  arrange(ss, year) # reorders
  
#################
#               #
# ARM 2 ANNUALS #
#               #
#################

# To prevent duplicate values when doing pivot_wide later
this_dict <- icsiplus_data_dict_long %>% filter(proj == "arm2_annual_var")

# initial data processing
arm2_annuals_wide <- 
  arm2_annuals_parsed %>% 
  filter(field_name %in% this_dict$question) %>% # narrows down
  mutate(year = as.numeric(regmatches(event_id, regexpr("\\d", event_id)))) %>% # retrieves year
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) %>%
  pivot_wider(id_cols = c(record, year), names_from = custom_name, values_from = value)

# prepares for the big join
arm2_annuals_wide_ready <-
  arm2_annuals_wide %>%
  mutate(date_last_period = as.Date(date_last_period)) %>% # changes into date format
  mutate(across(where(is.character), as.numeric)) %>% # converts rest to dbl
  mutate(
    bsi = bsi1+bsi2+bsi3+bsi4+bsi5+bsi6, # computes bsi
    icsi = ic1a+ic1b+ic1c+ic1d, # computes icsi
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm2r), by = c("record" = "arm2r")) %>%
  # rearranges table and removes redundant information (e.g., indiv bsi meas)
  select(
    ss, year, cramp_pain_no_nsaid, mens_pain_days_per_month, date_last_period,
    days_missed_3_months, days_bed_3_months, cramp_pain_no_nsaid, 
    cramp_pain_with_nsaid, bsi, urination_pain_last_week, bowel_mov_pain_last_week,
    icsi, mens_nonmens_pain_week, avg_pain, global_health
  ) %>%
  
  # CLEANS UP ERRORS! SEE `api-icsi-prepro.R` for the work; it was repeated here:
  
  # ss 183 completed year 3 in shortened annual, so remove this entry:
  #mutate(ss = ifelse(ss == 183 & year == 3 & is.na(timestamp), NA, ss)) %>%
  # NOT RUN because ss183 did not have an annual3 in this case
  filter(complete.cases(ss)) # removes those without a ss number

##########################
#                        #
# ARM 1 ASSESSMENT VISIT #
#                        #
##########################

# To prevent duplicate values when doing pivot_wide later
this_dict <- icsiplus_data_dict_long %>% filter(proj == "arm1_avisit1_var")
this_screen_dict <- 
  icsiplus_data_dict_long %>% 
  filter(proj == "arm1_avisit1_var") %>%
  filter(grepl("^mh", question)) # | grepl("global07", question)
  
# initial data processing
# assessment visit
arm1_avisit1 <- 
  arm1_avisit1_api_parsed %>% 
  filter(field_name %in% this_dict$question) %>% # narrows down
  mutate(year = 0) %>% # baseline
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) 
# screen visit (usses this_screen_dict)
arm1_screenvisit <- 
  arm1_screenvisit_api_parsed %>%
  filter(field_name %in% this_screen_dict$question) %>%
  mutate(year = 0) %>% # baseline
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub)

# combines screen + assessment visit data
arm1_baseline <- bind_rows(arm1_avisit1, arm1_screenvisit) 

# prepares for big join
arm1_baseline_ready  <-
  arm1_baseline %>%
  pivot_wider(id_cols = c(record, year), names_from = custom_name, values_from = value) %>%
   mutate(date_last_period = as.Date(date_last_period)) %>% # changes into date format
   mutate(across(where(is.character), as.numeric)) %>% # converts rest to dbl
   mutate(
     bsi = bsi1+bsi2+bsi3+bsi4+bsi5+bsi6, # computes bsi
     icsi = ic1a+ic1b+ic1c+ic1d, # computes icsi
   ) %>%
   # retrieves subject number from table:
   left_join(., select(crampp_codes, ss, arm1r), by = c("record" = "arm1r")) %>%
   # rearranges table and removes redundant information (e.g., indiv bsi meas)
   select(
     ss, year, cramp_pain_no_nsaid, mens_pain_days_per_month, date_last_period,
     days_missed_3_months, days_bed_3_months, cramp_pain_no_nsaid, 
     cramp_pain_with_nsaid, bsi, urination_pain_last_week, bowel_mov_pain_last_week,
     icsi, mens_nonmens_pain_week, avg_pain, global_health
   ) %>%
   filter(complete.cases(ss)) # removes those without a ss number

##########################
#                        #
# ARM 2 ASSESSMENT VISIT #
#                        #
##########################

# To prevent duplicate values when doing pivot_wide later
this_dict <- icsiplus_data_dict_long %>% filter(proj == "arm2_avisit1_var")
this_screen_dict <- 
  icsiplus_data_dict_long %>% 
  filter(proj == "arm2_avisit1_var") %>%
  filter(grepl("^mh", question)) # | grepl("global07", question)

# initial data processing
# assessment visit
arm2_avisit1 <- 
  arm2_avisit1_api_parsed %>% 
  filter(field_name %in% this_dict$question) %>% # narrows down
  mutate(year = 0) %>% # baseline
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub) 
# screen visit (usses this_screen_dict)
arm2_screenvisit <- 
  arm2_screenvisit_api_parsed %>%
  filter(field_name %in% this_screen_dict$question) %>%
  mutate(year = 0) %>% # baseline
  left_join(., this_dict, by = c("field_name" = "question")) %>%
  select(record, year, question = field_name, value, questionid:sub)

# combines screen + assessment visit data
arm2_baseline <- bind_rows(arm2_avisit1, arm2_screenvisit) 

# prepares for big join
arm2_baseline_ready <-
  arm2_baseline %>%
  pivot_wider(id_cols = c(record, year), names_from = custom_name, values_from = value) %>%
  mutate(date_last_period = as.Date(date_last_period)) %>% # changes into date format
  mutate(across(where(is.character), as.numeric)) %>% # converts rest to dbl
  mutate(
    bsi = bsi1+bsi2+bsi3+bsi4+bsi5+bsi6, # computes bsi
    icsi = ic1a+ic1b+ic1c+ic1d, # computes icsi
  ) %>%
  # retrieves subject number from table:
  left_join(., select(crampp_codes, ss, arm2r), by = c("record" = "arm2r")) %>%
  # rearranges table and removes redundant information (e.g., indiv bsi meas)
  select(
    ss, year, cramp_pain_no_nsaid, mens_pain_days_per_month, date_last_period,
    days_missed_3_months, days_bed_3_months, cramp_pain_no_nsaid, 
    cramp_pain_with_nsaid, bsi, urination_pain_last_week, bowel_mov_pain_last_week,
    icsi, mens_nonmens_pain_week, avg_pain, global_health
  ) %>%
  filter(complete.cases(ss)) # removes those without a ss number

############
#          #
# BIG JOIN #
#          #
############

# icsi data loaded for dates:
load("../output/complete-icsi-data.Rdata")

ready_dfs <- ls(pattern = "*_ready") # creates vector of df var names

# Combines all the data from above into one df
#mget: https://stackoverflow.com/questions/51162948/apply-strings-as-variable-names-in-bind-rows
annual_data <- 
  bind_rows(mget(ready_dfs)) %>%
  arrange(ss, year) %>%
  left_join(
    ., 
    complete_icsi_data %>% select(ss, year, timestamp, days_from_baseline),
    by = c("ss", "year")
    ) %>%
  relocate(ss, year, timestamp, days_from_baseline)

#* building upon the timestamp issue seen in `icsi-annual-prepro.R`, I will 
#* mannual import these timestamps and add them for those that have missing
#* timestamp data for year 1 (because they were done before annual questionnaires)
still_missing <- annual_data %>% filter(is.na(timestamp))

# Manually imports these 
crampp_annual_dates <- 
  read_excel(
    "../data/crampp-annual-dates.xlsx", 
    sheet = "for-r",
    na = c("nq", "No")
  )

# joins data
still_missing_info <- 
  still_missing %>% left_join(., crampp_annual_dates, by = "ss") 

# narrows down necessary features
still_missing_info_long <- 
  still_missing_info %>% 
  select(ss, year_0:year_5) %>%
  pivot_longer(-ss, names_to = "year", values_to = "timestamp") %>%
  mutate(year = as.numeric(gsub("year_","",year)))

# temporary df with baseline dates
baseline_dates <- 
  annual_data %>% 
  filter(year == 0) %>% 
  select(ss, baseline = timestamp)

# fixes days_since baseline with new dates
still_missing_fixed <-
  still_missing %>% 
  left_join(., still_missing_info_long, by = c("ss", "year")) %>%
  mutate(timestamp.y = as.Date(as.numeric(timestamp.y), origin = "1899-12-30")) %>%
  relocate(ss, year, timestamp = timestamp.y) %>%
  select(-timestamp.x) %>%
  left_join(., baseline_dates, by = "ss") %>%
  mutate(days_from_baseline = as.numeric(timestamp - baseline)) %>%
  select(-baseline) %>%
  distinct()

# THE COMPLETE EXTRA ANNUAL QUESTIONS DATA SET
complete_extra_annual_data <- 
  bind_rows(annual_data, still_missing_fixed) %>%
  filter(complete.cases(timestamp)) %>%
  arrange(ss, year)

##############################################################
#                                                            #
# SAVES OUT CLEANED AND PROCESSED ANNUAL EXTRA DATA          #
#                                                            #
##############################################################

save(complete_extra_annual_data, file = "../output/complete-extra-annual-data.Rdata")     # RDATA
write_csv(complete_extra_annual_data, file = "../output/complete-extra-annual-data.csv")  # CSV

# script objects to remove
rm_objs<-
  paste(
    "arm1",
    "arm2",
    "icsi_",
    "short_annuals",
    "avisit_1",
    "crampp",
    "rm_objs",
    "still_missing",
    "baseline_dates",
    "annual",
    "icsi",
    "this",
    "left_out",
    sep = "|"
  )

rm(list = ls(pattern = rm_objs)) # cleans up workspace objects






