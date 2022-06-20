# GSS Preprocessing
# Matt Kmiecik
# Started 20 June 2022

# Purpose: to retrieve the questionnaires that comprise the Generalized Sensory
# Sensitivity (GSS) measure developed by Schrepf et al. (2018) and previously
# reported on from our lab Schrepf et al. (in press)

# Prep workspace
source("r-prep.R")

# Load data
load("../output/arm1_screenvisit_api_parsed.RData") # arm 1 screen visit data
load("../output/arm2_screenvisit_api_parsed.RData") # arm 2 screen visit data

# Brings in CRAMPP CODES -- a document that connects record numbers across the
# arms/redcap projects
crampp_codes <- read_excel("../data/crampp-codes.xlsx", sheet = "usethis")

# FOR TODAY: USE THE EXCEL DOCUMENT OF SCREEN VISITS TO CORROBORATE FINDINGS!

# this was processed in `screenvisit-prepro.R` using the Excel doc of redcap
load("../output/redcap-cmsi-data.RData") 


#############
#           #
# PAIN SITE #
#           #
#############

# Arm 1 screen visit
arm1_pain_site <- 
  arm1_screenvisit_api_parsed %>% 
  filter(field_name == "cmsi_fibro1") %>% # LAST 7 DAYS
  left_join(., crampp_codes %>% select(ss, arm1r), by = c("record" = "arm1r")) %>%
  select(ss, field_name, value)

# Arm 2 screen visit
arm2_pain_site <- 
  arm2_screenvisit_api_parsed %>% 
  filter(field_name == "cmsi_fibro1") %>% # LAST 7 DAYS
  left_join(., crampp_codes %>% select(ss, arm2r), by = c("record" = "arm2r")) %>%
  select(ss, field_name, value)

# combines arm 1 and 2
pain_site_data <- 
  bind_rows(arm1_pain_site, arm2_pain_site) %>% 
  mutate(value = as.numeric(value))

# separates participants (the reason for this is to really make sure that 
# no participants reported no pain then endorsed a site)
pain_site_no_pain_ss <- pain_site_data %>% filter(value == 99)
pain_site_pain_ss <- pain_site_data %>% filter(value != 99)

# Sums pain subjects
pain_site_pain_ss_sum <- 
  pain_site_pain_ss %>% 
  group_by(ss) %>% 
  summarise(pain_sites = n())

# records 0 for no pain subjects
pain_site_no_pain_ss_sum <- 
  pain_site_no_pain_ss %>% 
  mutate(pain_sites = 0) %>%
  select(ss, pain_sites)

# combines sums together
pain_site_sum <- 
  bind_rows(pain_site_pain_ss_sum, pain_site_no_pain_ss_sum) %>%
  mutate(meas = "pain_site") %>%
  select(ss, meas, sum = pain_sites) # helps with join later

# any duplicates? answer is no
# pain_site_sum %>% count(ss) %>% filter(n > 1)

########
#      #
# CMSI #
#      #
########

# The redcap API only returns items that were endorsed on the CMSI, not those 
# that were not endorsed, making the current output ambiguous to work with
# therefore, this workaround was devised

# inits
ss_var_index <- c(1:41)
ss_vars <- paste0(rep("cmsi_gen", length(ss_var_index)), ss_var_index)

# Arm 1 - - - -

# creates tibble of all possible CMSI responses
cmsi_placeholder <-
  tibble(
  record = rep(unique(arm1_screenvisit_api_parsed$record), each = length(ss_vars)),
  cmsi = rep(ss_vars, length(unique(record)))
)

arm1_cmsi <- 
  arm1_screenvisit_api_parsed %>%
  filter(substr(field_name, 1, 8) == "cmsi_gen") %>%
  mutate(value = as.numeric(value)) %>%
  filter(value == 1) %>% # LAST 3 MONTHS
  left_join(., crampp_codes %>% select(ss, arm1r), by = c("record" = "arm1r")) %>%
  select(ss, cmsi = field_name)
  
arm2_cmsi <- 
  arm2_screenvisit_api_parsed %>%
  filter(substr(field_name, 1, 8) == "cmsi_gen") %>%
  mutate(value = as.numeric(value)) %>%
  filter(value == 1) %>% # LAST 3 MONTHS
  left_join(., crampp_codes %>% select(ss, arm2r), by = c("record" = "arm2r")) %>%
  select(ss, cmsi = field_name)

cmsi_comb <- 
  bind_rows(arm1_cmsi, arm2_cmsi) %>%
  mutate(
    cmsi = as.numeric(gsub("cmsi_gen", "", cmsi)),
    meas = case_when(
      cmsi %in% c(35, 36, 37, 39) ~ "sens", # sensory sensitivity
      cmsi %in% c(3,6,7,8,9,10,11,14,15,16,17,18,20,21,22,24,25) ~ "sa", # somat aware 
      TRUE ~ as.character(cmsi)
    )
  )

# summates the sensory sensitivity and somatic awareness data
cmsi_data_sum <- 
  cmsi_comb %>%
  filter(meas %in% c("sens", "sa")) %>%
  group_by(ss, meas) %>%
  summarise(sum = n()) %>%
  ungroup()

load("../output/redcap-cmsi-data.RData")




arm1_cmsi_ss <- 
  left_join(cmsi_placeholder, arm1_cmsi, by = c("record", "cmsi")) %>%
  mutate(value = ifelse(is.na(value), 0, 1)) %>%
  left_join(., crampp_codes %>% select(ss, arm1r), by = c("record" = "arm1r")) %>%
  filter(complete.cases(ss)) %>%
  select(ss, cmsi, value)

# Arm 2 - - - -

# creates tibble of all possible CMSI responses
cmsi_placeholder_2 <-
  tibble(
    record = rep(unique(arm2_screenvisit_api_parsed$record), each = length(ss_vars)),
    cmsi = rep(ss_vars, length(unique(record)))
  )



arm2_cmsi_ss <- 
  left_join(cmsi_placeholder_2, arm2_cmsi, by = c("record", "cmsi")) %>%
  mutate(value = ifelse(is.na(value), 0, 1)) %>%
  left_join(., crampp_codes %>% select(ss, arm2r), by = c("record" = "arm2r")) %>%
  filter(complete.cases(ss)) %>%
  select(ss, cmsi, value)

# combines CMSI data across arms and adds labels to help with calculation
cmsi_data <- 
  bind_rows(arm1_cmsi_ss, arm2_cmsi_ss) %>%
  mutate(
    cmsi = as.numeric(gsub("cmsi_gen", "", cmsi)),
    meas = case_when(
      cmsi %in% c(35, 36, 37, 39) ~ "sens", # sensory sensitivity
      cmsi %in% c(3,6,7,8,9,10,11,14,15,16,17,18,20,21,22,24,25) ~ "sa", # somat aware 
      TRUE ~ as.character(cmsi)
      )
    )

# summates the sensory sensitivity and somatic awareness data
cmsi_data_sum <- 
  cmsi_data %>%
  filter(meas %in% c("sens", "sa")) %>%
  group_by(ss, meas) %>%
  summarise(sum = sum(value)) %>%
  ungroup()

# Combines all three measures of the GSS:
gss_data <- bind_rows(cmsi_data_sum, pain_site_sum) %>% arrange(ss)

# Saves out data
save(gss_data, file = "../output/gss-data.rda") # rda
write_csv(gss_data, file = "../output/gss-data.csv") # csv

# cleans script objects
rm(
  arm1_cmsi,
  arm1_cmsi_ss,
  arm1_pain_site,
  arm1_screenvisit_api_parsed,
  arm2_cmsi,
  arm2_cmsi_ss,
  arm2_pain_site,
  arm2_screenvisit_api_parsed,
  cmsi_data,
  cmsi_data_sum,
  cmsi_placeholder,
  cmsi_placeholder_2,
  crampp_codes,
  gss_data,
  pain_site_data,
  pain_site_no_pain_ss,
  pain_site_no_pain_ss_sum,
  pain_site_pain_ss,
  pain_site_pain_ss_sum,
  pain_site_sum,
  ss_var_index,
  ss_vars
)
