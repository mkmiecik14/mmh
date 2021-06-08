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
  
avisit1_data <- bind_rows(arm1_avisit1_data, arm2_avisit1_data) %>% # combines arms 1 and 2
  rename(ss = subjectid) # to be consistent with ss_codes


# Now I will focus on specific questionnaires/tasks required for analysis:

# Bladder test ----
bladder_data <- 
  avisit1_data %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    starts_with("bt")
    )

# Saving out data
save(bladder_data, file = "../output/bladder-data.RData") # RData
write_csv(bladder_data, file = "../output/bladder-data.csv") # CSV

# PPTs ----
redcap_ppt_data <- 
  avisit1_data %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    starts_with("pt")
  )

# Saving out data
save(redcap_ppt_data, file = "../output/redcap-ppt-data.RData") # RData
write_csv(redcap_ppt_data, file = "../output/redcap-ppt-data.csv") # CSV

# Cleaning script objects ----
rm(
  arm1_avisit1_data, 
  arm2_avisit1_data, 
  avisit1_data, 
  bladder_data, 
  redcap_ppt_data
  )
