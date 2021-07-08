# Assessment Visit 1 preprocessing script
# Matt Kmiecik
# Started 8 June 2021

# Purpose: load raw data from RedCap and prepare questionnaires/tasks for 
# analysis

source("r-prep.R") # Prepares R workspace

# Loads in data ----

# Arm 1
arm1_avisit1_data <- 
  read_csv("../data/arm-1-assessment-1-data.csv") %>% # arm 1
  mutate(
    bt1_oztoday = ifelse(bt1_oztoday %in% "8oz", "8", bt1_oztoday), # removes chars
    bt1_oztoday = as.numeric(bt1_oztoday) # makes numeric
  )

# Arm 2
arm2_avisit1_data <- 
  read_csv("../data/arm-2-assessment-1-data.csv") %>% # arm 2
  rename(subjectid = subject_id) %>% # renaming columns to be consistent with arm1
  mutate(redcap_event_name = "assessment_visit_1_arm_2") # signals arm2 participant
  
# combines arms 1 and 2
avisit1_data <- 
  bind_rows(arm1_avisit1_data, arm2_avisit1_data) %>% 
  rename(ss = subjectid) # to be consistent with ss_codes

load("../output/ss-codes.RData") # Loads in ss_codes

# First step is to align record numbers with ss id's across arms 1 and 2
arm1_temp <- 
  avisit1_data %>% 
  filter(redcap_event_name %in% "assessment_visit_1_arm_1") %>%
  left_join(., ss_codes, by = c("record_number" = "arm1r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm2r)
arm2_temp <- 
  avisit1_data %>% 
  filter(redcap_event_name %in% "assessment_visit_1_arm_2") %>%
  left_join(., ss_codes, by = c("record_number" = "arm2r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm1r)

# Combines arm 1 and 2 after aligning ss numbers with record numbers
avisit1_data_ss <- bind_rows(arm1_temp, arm2_temp)


# Now I will focus on specific questionnaires/tasks required for analysis:

# Bladder test ----
bladder_data <- 
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    starts_with("bt")
    ) %>%
  # DATA EDITING - some participants had missing data in REDCAP but had these
  # values written down in their binders (paper copies)
  mutate(
    # These  participants capped out so we look at their 120 min mark
    bt10c_mturg = ifelse(ss == 134 & is.na(bt10c_mturg), 88, bt10c_mturg),
    bt10c_mturg = ifelse(ss == 166 & is.na(bt10c_mturg), 64, bt10c_mturg),
    bt10c_mtpain = ifelse(ss == 134 & is.na(bt10c_mtpain), 1, bt10c_mtpain),
    bt10c_mtpain = ifelse(ss == 166 & is.na(bt10c_mtpain), 3, bt10c_mtpain)
  )


# Saving out data
save(bladder_data, file = "../output/bladder-data.RData") # RData
write_csv(bladder_data, file = "../output/bladder-data.csv") # CSV

# PPTs ----
redcap_ppt_data <- 
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    starts_with("pt")
  ) %>%
  # DATA EDITING - some participants had missing data in REDCAP but had these
  # values written down in their binders (paper copies)
  mutate(
    pt4b_cpmwater = ifelse(ss == 4 & is.na(pt4b_cpmwater), 4, pt4b_cpmwater),
    pt3_5d_prickling = ifelse(ss == 62 & is.na(pt3_5d_prickling), 0, pt3_5d_prickling),
    pt3_5a_sharp = ifelse(ss == 88 & is.na(pt3_5a_sharp), 3, pt3_5a_sharp),
    pt3_5a_sharp = ifelse(ss == 271 & is.na(pt3_5a_sharp), 2, pt3_5a_sharp),
    pt3_5b_pressing = ifelse(ss == 271 & is.na(pt3_5b_pressing), 4, pt3_5b_pressing),
    pt3_5c_dull = ifelse(ss == 271 & is.na(pt3_5c_dull), 2, pt3_5c_dull),
    pt3_5d_prickling = ifelse(ss == 271 & is.na(pt3_5d_prickling), 3, pt3_5d_prickling)
    )

# Saving out data
save(redcap_ppt_data, file = "../output/redcap-ppt-data.RData") # RData
write_csv(redcap_ppt_data, file = "../output/redcap-ppt-data.csv") # CSV

# Additional data that will not be included in the PCA, but is useful for
# modeling and coloring

# BSI
redcap_bsi_data <- 
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    bsi1:bsi7
  )

# Saving out data
save(redcap_bsi_data, file = "../output/redcap-bsi-data.RData") # RData
write_csv(redcap_bsi_data, file = "../output/redcap-bsi-data.csv") # CSV


# Female GUPI
# Pain subscale: sum of 1a-d, 2a-d, 3, 4
# Urinary Subscale: sum of 5 and 6
# QOL impact: sum of 7, 8, 9
# Total score = sum of all subscale scores
redcap_gupi_data <- 
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    starts_with("gupi")
  )

# Saving out data
save(redcap_gupi_data, file = "../output/redcap-gupi-data.RData") # RData
write_csv(redcap_gupi_data, file = "../output/redcap-gupi-data.csv") # CSV

# ICSI + ICPI
redcap_ic_data <-
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    ic1a, ic1b, ic1c, ic1d, # ICSI
    ic2a, ic2b, ic2c, ic2d  # ICPI
  )

# Saving out data
save(redcap_ic_data, file = "../output/redcap-ic-data.RData") # RData
write_csv(redcap_ic_data, file = "../output/redcap-ic-data.csv") # CSV

# ROME
redcap_rome_data <-
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    rome1:rome10
  )

# Saving out data
save(redcap_rome_data, file = "../output/redcap-rome-data.RData") # RData
write_csv(redcap_rome_data, file = "../output/redcap-rome-data.csv") # CSV

# PROMIS - Global
redcap_promis_global_data <-
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    global01, 
    global02, 
    global03, 
    global04, 
    global05, 
    global06, 
    global07, 
    global08, 
    global09, 
    global10 
  )

# Saving out data
save(redcap_promis_global_data, file = "../output/redcap-promis-global-data.RData") # RData
write_csv(redcap_promis_global_data, file = "../output/redcap-promis-global-data.csv") # CSV

# PROMIS - Pain Behavior
redcap_promis_pb_data <-
avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    painbe2,
    painbe3,
    painbe8,
    painbe24,
    painbe25,
    painbe37,
    painbe45
  )

# Saving out data
save(redcap_promis_pb_data, file = "../output/redcap-promis-pb-data.RData") # RData
write_csv(redcap_promis_pb_data, file = "../output/redcap-promis-pb-data.csv") # CSV

# PROMIS - Pain interference
redcap_promis_pi_data <- 
  avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    painin3,
    painin8,
    painin9,
    painin10,
    painin14,
    painin26
  )

# Saving out data
save(redcap_promis_pi_data, file = "../output/redcap-promis-pi-data.RData") # RData
write_csv(redcap_promis_pi_data, file = "../output/redcap-promis-pi-data.csv") # CSV



# Cleaning script objects ----
rm(
  arm1_avisit1_data, 
  arm2_avisit1_data, 
  avisit1_data, 
  bladder_data, 
  redcap_ppt_data,
  arm1_temp,
  arm2_temp,
  ss_codes,
  avisit1_data_ss,
  redcap_bsi_data,
  redcap_gupi_data,
  redcap_ic_data,
  redcap_promis_global_data,
  redcap_promis_pb_data,
  redcap_promis_pi_data,
  redcap_rome_data
  )
