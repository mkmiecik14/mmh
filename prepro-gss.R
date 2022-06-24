# GSS Preprocessing
# Matt Kmiecik
# Started 20 June 2022

# Purpose: to retrieve the questionnaires that comprise the Generalized Sensory
# Sensitivity (GSS) measure developed by Schrepf et al. (2018) and previously
# reported on from our lab Schrepf et al. (in press)

# Prep workspace - - - -
source("r-prep.R")

# Load data - - - -

# Redcap data pulled through API
#load("../output/arm1_screenvisit_api_parsed.RData") # arm 1 screen visit data
#load("../output/arm2_screenvisit_api_parsed.RData") # arm 2 screen visit data

# Brings in CRAMPP CODES -- a document that connects record numbers across the
# arms/redcap projects
crampp_codes <- read_excel("../data/crampp-codes.xlsx", sheet = "usethis")
load("../output/mmh-res.RData") # loads in PCA data for ss numbers

#* PROBLEM:
#* the CMSI is an endorsement questionnaire, meaning that participants would 
#* endorse (check a box) if they experienced a symptom. If you pull data through
#* the Recap api, it does not report anything if a participant did not endorse a
#* symptom. This is a problem because then you do not have any way of knowing if
#* a participant skipped the questionnaire or if they simply did not endorse 
#* symptom(s). When downloading the data from Redcap via Excel, it instead
#* places a zero in the column for no endorsement regardless if they took the
#* questionnaire or not. In sum, there is no efficient or 100% reliable method
#* to know whether a participant completed the CMSI. This was somewhat later fixed
#* by including a 99-no pain option on cmsi_fibro1, but this still doesn't
#* fix the earlier cmsi questions.
#* My temporary solution is to assume that all participants that are included in
#* the PCA analysis (i.e., had complete QST data) completed the CMSI and use the 
#* excel version.
#* UPDATE: Kevin provided a document that lists participants with missing CMSI 
#* data. Use this to replace values with NA

# This is the sheet with the missing cmsi data
cmsi_missing_data <- 
  read_excel(path = "../data/cmsi-missing-data.xlsx", sheet = "for-r", na = "ms")

# df of missing subjects only
missing_ss <- cmsi_missing_data %>% filter(is.na(cmsi_gen1___1))


# Reads in csv data (i.e., excel version)
arm1_screen_data <- 
  read_csv(file = "../data/arm-1-screen-data.csv") # arm 1 screen data
arm2_screen_data <- 
  read_csv(file = "../data/arm-2-screen-data.csv") # arm 2 screen data

# preps arm 1 cmsi data
arm1_cmsi <-
  arm1_screen_data %>%
  select(record_number, contains("cmsi")) %>%
  left_join(
    ., 
    crampp_codes %>% select(ss, arm1r), 
    by = c("record_number" = "arm1r")
    ) %>%
  relocate(ss) %>%
  select(-record_number) %>%
  filter(complete.cases(ss))

# preps arm 2 cmsi data
arm2_cmsi <-
  arm2_screen_data %>%
  select(record_number, contains("cmsi")) %>%
  left_join(
    ., 
    crampp_codes %>% select(ss, arm2r), 
    by = c("record_number" = "arm2r")
  ) %>%
  relocate(ss) %>%
  select(-record_number) %>%
  filter(complete.cases(ss))

# combines arm 1 and 2 cmsi data 
cmsi_data <- 
  bind_rows(arm1_cmsi, arm2_cmsi) %>% 
  arrange(ss)

# narrows down data for 200 PCA participants - - - -

# # gets factor scores and ss for later
# fi <- 
#   as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
#   mutate(ss = as.numeric(ss)) %>%
#   left_join(., crampp_codes %>% select(ss, group), by = "ss") %>%
#   select(ss, group, V1:V40)
# 
# 
# # THE MAIN DATA FRAME FOR GSS COMPUTATIONS
# cmsi_data_pca <- cmsi_data %>% filter(ss %in% fi$ss)


#############
#           #
# PAIN SITE #
#           #
#############

cmsi_pain_site_ss <- 
  cmsi_data %>% 
  select(ss, starts_with("cmsi_fibro1")) %>% # pain at sites in last 7 days
  # removes the no pain column, as can be inferred from the remaining columns
  select(-cmsi_fibro1___99) %>%
  pivot_longer(-ss) %>%
  group_by(ss) %>%
  summarise(sum = sum(value), total_possible = n()) %>%
  ungroup() %>%
  mutate(sub_q = "pain_sites") %>%
  mutate(sum = ifelse(ss %in% missing_ss$ss, NA, sum)) # INCLUDES NAs



###########################################
#                                         #
# Somatic Awareness & Sensory Sensitivity #
#                                         #
###########################################

sa_ss_data_ss <-
  cmsi_data %>%
  select(ss, starts_with("cmsi_gen")) %>%
  pivot_longer(-ss) %>%
  separate(name, into = c("meas", "q", "time")) %>%
  mutate(
    q = as.numeric(gsub("gen", "", q)),
     sub_q = case_when(
      q %in% c(35, 36, 37, 39) ~ "sens", # sensory sensitivity
      q %in% c(3,6,7,8,9,10,11,14,15,16,17,18,20,21,22,24,25) ~ "sa", # somat aware 
      TRUE ~ as.character(q)
      )
    )

# this is the new and improved CMSI data set for use in this study
# it will be saved out and replace the old method
cmsi_data_clean <- 
  sa_ss_data_ss %>%
  mutate(value = ifelse(ss %in% missing_ss$ss, NA, value)) %>%
  group_by(ss, time) %>%
  summarise(sum = sum(value), n = n()) %>% # here is a mid calc check
  ungroup() %>%
  pivot_wider(id_cols = ss, names_from = time, values_from = sum) %>%
  rename(cmsi_3_months = `1`, cmsi_lifetime = `2`)

# saves out
# uncomment to save out
#save(cmsi_data_clean, file = "../output/cmsi-data-clean.rda")
#write_csv(cmsi_data_clean, file = "../output/cmsi-data-clean.csv")


# calculates sum total score
sa_ss_data_sum <- 
  sa_ss_data_ss %>%
  filter(sub_q %in% c("sens", "sa")) %>% # filters out questions not part of gss
  group_by(ss, time, sub_q) %>%
  summarise(sum = sum(value), total_possible = n()) %>%
  ungroup() %>%
  mutate(sum = ifelse(ss %in% missing_ss$ss, NA, sum)) # INCLUDES NAs
  

# creating GSS data frame
gss_data_long <-
  sa_ss_data_sum %>% 
  filter(time == 1) %>% # pain in last three months
  select(-time, -total_possible) %>%
  bind_rows(., cmsi_pain_site_ss %>% select(-total_possible)) %>%
  arrange(ss, sub_q)

# Saves out GSS data - - - -
save(gss_data_long, file = "../output/gss-data-long.rda")
write_csv(gss_data_long, file = "../output/gss-data-long.csv")

#############
#           #
# GSS Brief #
#           #
#############

# GSS body site bins (from cmsi_fibro1__n)
a_head_neck <- c(1, 8, 15)
b_right_shoulder_arm_hand <- c(9, 10, 11)
c_left_shoulder_arm_hand <- c(2, 3, 4)
d_right_hip_leg_food <- c(12, 13, 14)
e_left_hip_leg_foot <- c(5, 6, 7)
f_chest_abdomen <- c(17, 18)
g_back <- c(16, 19)

gss_brief_site_ss <-
  cmsi_data %>%
  select(ss, starts_with("cmsi_fibro1")) %>% # pain at sites in last 7 days
  # removes the no pain column, as can be inferred from the remaining columns
  select(-cmsi_fibro1___99) %>%
  pivot_longer(-ss) %>%
  separate(name, into = c("meas", "time", "site")) %>%
  mutate(
    brief = case_when(
      site %in% a_head_neck ~ "A",
      site %in% b_right_shoulder_arm_hand ~ "B",
      site %in% c_left_shoulder_arm_hand ~ "C",
      site %in% d_right_hip_leg_food ~ "D",
      site %in% e_left_hip_leg_foot ~ "E",
      site %in% f_chest_abdomen ~ "F",
      site %in% g_back ~ "G"
    )
  )

# bins regions and summates
gss_brief_site_ss_check <- 
  gss_brief_site_ss %>% 
  group_by(ss, brief) %>%
  summarise(sum = sum(value), n = n()) %>%
  ungroup() %>%
  mutate(checked = ifelse(sum>=1, 1, 0)) # logical whether 1 or more were endorsed

# sums the regions and scores them according to GSS brief
gss_brief_site_ss_regions <- 
  gss_brief_site_ss_check %>% 
  group_by(ss) %>%
  summarise(regions = sum(checked)) %>%
  ungroup() %>%
  # scores the regions according to GSS brief
  mutate(region_score = case_when(
    between(regions, 0, 1) ~ 0,
    between(regions, 2, 3) ~ 1,
    between(regions, 4, 5) ~ 2,
    between(regions, 6, 7) ~ 3
  )
  ) %>%
  mutate(region_score = ifelse(ss %in% missing_ss$ss, NA, region_score)) # INCLUDES NAs

gss_brief_symp_score <- 
  cmsi_data %>%
  select(
    ss, 
    # had any of these symptoms for at least 3 months in the past year (1)
    cmsi_gen8___1, # dry mouth
    cmsi_gen8___1, # rapid heart rate
    cmsi_gen20___1, # problems with balance
    cmsi_gen35___1, # sensitivity to certain chemicals, such as perfumes, ...
    cmsi_gen36___1, # sensitivity to sound
    cmsi_gen39___1 # frequent sensitivity to bright lights
    ) %>%
  pivot_longer(-ss) %>% 
  group_by(ss) %>%
  summarise(symp_score = sum(value)) %>%
  ungroup() %>%
  mutate(symp_score = ifelse(ss %in% missing_ss$ss, NA, symp_score)) # INCLUDES NAs

# combines both site score and symptom score for total gss brief (range = 0-9)
gss_brief_data <-
  left_join(gss_brief_site_ss_regions, gss_brief_symp_score, by = "ss") %>%
  mutate(gss_brief = region_score + symp_score)
range(gss_brief_data$gss_brief, na.rm = TRUE) # checks range to see 0-9

# saves out gss brief score
save(gss_brief_data, file = "../output/gss-brief-data.rda") # rda
write_csv(gss_brief_data, file = "../output/gss-brief-data.csv") # csv

# removes script objects - - - -
rm(
  arm1_cmsi,
  arm1_screen_data,
  arm2_cmsi,
  arm2_screen_data,
  cmsi_data,
  cmsi_data_pca,
  cmsi_pain_site_ss,
  cmsi_missing_data,
  missing_ss,
  crampp_codes,
  gss_data_long,
  sa_ss_data_ss,
  sa_ss_data_sum,
  cmsi_data_clean,
  bada_res,
  bada_res_2,
  gss_brief_data,
  gss_brief_site_ss,
  gss_brief_site_ss_check,
  gss_brief_site_ss_regions,
  gss_brief_symp_score,
  pca_res,
  shrs_res,
  a_head_neck,
  b_right_shoulder_arm_hand,
  c_left_shoulder_arm_hand,
  d_right_hip_leg_food,
  e_left_hip_leg_foot,
  f_chest_abdomen,
  g_back,
  iters
)


# PREVIOUS CODE SOLUTION - - - - -

# # Arm 1 screen visit
# arm1_pain_site <- 
#   arm1_screenvisit_api_parsed %>% 
#   filter(field_name == "cmsi_fibro1") %>% # LAST 7 DAYS
#   left_join(., crampp_codes %>% select(ss, arm1r), by = c("record" = "arm1r")) %>%
#   select(ss, field_name, value)
# 
# # Arm 2 screen visit
# arm2_pain_site <- 
#   arm2_screenvisit_api_parsed %>% 
#   filter(field_name == "cmsi_fibro1") %>% # LAST 7 DAYS
#   left_join(., crampp_codes %>% select(ss, arm2r), by = c("record" = "arm2r")) %>%
#   select(ss, field_name, value)
# 
# # combines arm 1 and 2
# pain_site_data <- 
#   bind_rows(arm1_pain_site, arm2_pain_site) %>% 
#   mutate(value = as.numeric(value))
# 
# # separates participants (the reason for this is to really make sure that 
# # no participants reported no pain then endorsed a site)
# pain_site_no_pain_ss <- pain_site_data %>% filter(value == 99)
# pain_site_pain_ss <- pain_site_data %>% filter(value != 99)
# 
# # Sums pain subjects
# pain_site_pain_ss_sum <- 
#   pain_site_pain_ss %>% 
#   group_by(ss) %>% 
#   summarise(pain_sites = n())
# 
# # records 0 for no pain subjects
# pain_site_no_pain_ss_sum <- 
#   pain_site_no_pain_ss %>% 
#   mutate(pain_sites = 0) %>%
#   select(ss, pain_sites)
# 
# # combines sums together
# pain_site_sum <- 
#   bind_rows(pain_site_pain_ss_sum, pain_site_no_pain_ss_sum) %>%
#   mutate(meas = "pain_site") %>%
#   select(ss, meas, sum = pain_sites) # helps with join later
# 
# # any duplicates? answer is no
# # pain_site_sum %>% count(ss) %>% filter(n > 1)
# 
# ########
# #      #
# # CMSI #
# #      #
# ########
# 
# # The redcap API only returns items that were endorsed on the CMSI, not those 
# # that were not endorsed, making the current output ambiguous to work with
# # therefore, this workaround was devised
# 
# # inits
# ss_var_index <- c(1:41)
# ss_vars <- paste0(rep("cmsi_gen", length(ss_var_index)), ss_var_index)
# 
# # Arm 1 - - - -
# 
# # creates tibble of all possible CMSI responses
# cmsi_placeholder <-
#   tibble(
#   record = rep(unique(arm1_screenvisit_api_parsed$record), each = length(ss_vars)),
#   cmsi = rep(ss_vars, length(unique(record)))
# )
# 
# arm1_cmsi <- 
#   arm1_screenvisit_api_parsed %>%
#   filter(substr(field_name, 1, 8) == "cmsi_gen") %>%
#   mutate(value = as.numeric(value)) %>%
#   filter(value == 1) %>% # LAST 3 MONTHS
#   left_join(., crampp_codes %>% select(ss, arm1r), by = c("record" = "arm1r")) %>%
#   select(ss, cmsi = field_name)
#   
# arm2_cmsi <- 
#   arm2_screenvisit_api_parsed %>%
#   filter(substr(field_name, 1, 8) == "cmsi_gen") %>%
#   mutate(value = as.numeric(value)) %>%
#   filter(value == 1) %>% # LAST 3 MONTHS
#   left_join(., crampp_codes %>% select(ss, arm2r), by = c("record" = "arm2r")) %>%
#   select(ss, cmsi = field_name)
# 
# cmsi_comb <- 
#   bind_rows(arm1_cmsi, arm2_cmsi) %>%
#   mutate(
#     cmsi = as.numeric(gsub("cmsi_gen", "", cmsi)),
#     meas = case_when(
#       cmsi %in% c(35, 36, 37, 39) ~ "sens", # sensory sensitivity
#       cmsi %in% c(3,6,7,8,9,10,11,14,15,16,17,18,20,21,22,24,25) ~ "sa", # somat aware 
#       TRUE ~ as.character(cmsi)
#     )
#   )
# 
# # summates the sensory sensitivity and somatic awareness data
# cmsi_data_sum <- 
#   cmsi_comb %>%
#   filter(meas %in% c("sens", "sa")) %>%
#   group_by(ss, meas) %>%
#   summarise(sum = n()) %>%
#   ungroup()
# 
# load("../output/redcap-cmsi-data.RData")
# 
# 
# 
# 
# arm1_cmsi_ss <- 
#   left_join(cmsi_placeholder, arm1_cmsi, by = c("record", "cmsi")) %>%
#   mutate(value = ifelse(is.na(value), 0, 1)) %>%
#   left_join(., crampp_codes %>% select(ss, arm1r), by = c("record" = "arm1r")) %>%
#   filter(complete.cases(ss)) %>%
#   select(ss, cmsi, value)
# 
# # Arm 2 - - - -
# 
# # creates tibble of all possible CMSI responses
# cmsi_placeholder_2 <-
#   tibble(
#     record = rep(unique(arm2_screenvisit_api_parsed$record), each = length(ss_vars)),
#     cmsi = rep(ss_vars, length(unique(record)))
#   )
# 
# 
# 
# arm2_cmsi_ss <- 
#   left_join(cmsi_placeholder_2, arm2_cmsi, by = c("record", "cmsi")) %>%
#   mutate(value = ifelse(is.na(value), 0, 1)) %>%
#   left_join(., crampp_codes %>% select(ss, arm2r), by = c("record" = "arm2r")) %>%
#   filter(complete.cases(ss)) %>%
#   select(ss, cmsi, value)
# 
# # combines CMSI data across arms and adds labels to help with calculation
# cmsi_data <- 
#   bind_rows(arm1_cmsi_ss, arm2_cmsi_ss) %>%
#   mutate(
#     cmsi = as.numeric(gsub("cmsi_gen", "", cmsi)),
#     meas = case_when(
#       cmsi %in% c(35, 36, 37, 39) ~ "sens", # sensory sensitivity
#       cmsi %in% c(3,6,7,8,9,10,11,14,15,16,17,18,20,21,22,24,25) ~ "sa", # somat aware 
#       TRUE ~ as.character(cmsi)
#       )
#     )
# 
# # summates the sensory sensitivity and somatic awareness data
# cmsi_data_sum <- 
#   cmsi_data %>%
#   filter(meas %in% c("sens", "sa")) %>%
#   group_by(ss, meas) %>%
#   summarise(sum = sum(value)) %>%
#   ungroup()
# 
# # Combines all three measures of the GSS:
# gss_data <- bind_rows(cmsi_data_sum, pain_site_sum) %>% arrange(ss)
# 
# # Saves out data
# save(gss_data, file = "../output/gss-data.rda") # rda
# write_csv(gss_data, file = "../output/gss-data.csv") # csv
# 
# # cleans script objects
# rm(
#   arm1_cmsi,
#   arm1_cmsi_ss,
#   arm1_pain_site,
#   arm1_screenvisit_api_parsed,
#   arm2_cmsi,
#   arm2_cmsi_ss,
#   arm2_pain_site,
#   arm2_screenvisit_api_parsed,
#   cmsi_data,
#   cmsi_data_sum,
#   cmsi_placeholder,
#   cmsi_placeholder_2,
#   crampp_codes,
#   gss_data,
#   pain_site_data,
#   pain_site_no_pain_ss,
#   pain_site_no_pain_ss_sum,
#   pain_site_pain_ss,
#   pain_site_pain_ss_sum,
#   pain_site_sum,
#   ss_var_index,
#   ss_vars
# )
