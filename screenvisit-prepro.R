# Screen visit preprocessing script
# Matt Kmiecik
# Started 8 July 2021

# Purpose: load raw data from RedCap and prepare questionnaires/tasks for 
# analysis from the screen visit

source("r-prep.R") # Prepares R workspace

# Loads in data ----
# arm 1 screen
arm1_screen_data <- 
  read_csv("../data/arm-1-screen-data.csv") %>%
  mutate(
    subject_id = as.numeric(subject_id), # makes numeric
    mh37a = as.numeric(mh37a), # makes numeric
    mh29_acu_mos = as.character(mh29_acu_mos), # makes character for easy compare to arm2
    cmsi_fatigue5 = as.character(cmsi_fatigue5)
    )
 
# arm 2 screen
arm2_screen_data <- 
  read_csv("../data/arm-2-screen-data.csv") %>%
  mutate(
    redcap_event_name = "screen_visit_arm_2", # signals arm2 participant
    cmsi_fatigue5 = as.character(cmsi_fatigue5) # arm two has an issue with a date for cmsi_fatigue5
    ) 

# combines arms 1 and 2
screen_data <- 
  bind_rows(arm1_screen_data, arm2_screen_data) %>% 
  rename(ss = subject_id) # to be consistent with ss_codes

load("../output/ss-codes.RData") # Loads in ss_codes

# First step is to align record numbers with ss id's across arms 1 and 2
arm1_temp <- 
  screen_data %>% 
  filter(redcap_event_name %in% "screen_visit_arm_1") %>%
  left_join(., ss_codes, by = c("record_number" = "arm1r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm2r)
arm2_temp <- 
  screen_data %>% 
  filter(redcap_event_name %in% "screen_visit_arm_2") %>%
  left_join(., ss_codes, by = c("record_number" = "arm2r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm1r)

# Combines arm 1 and 2 after aligning ss numbers with record numbers
screen_data_ss <- bind_rows(arm1_temp, arm2_temp) %>% arrange(ss)

# Gathering important data:

# Menstrual pain (mh23 and mh23a)
# I think only at screen

screen_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    mh23, mh23a
    )

# CMSI (is this only at screen?) - edit below
avisit1_data_ss %>%
  select(
    record_number, 
    redcap_event_name, 
    ss,
    starts_with("cmsi_gen1")
  )







