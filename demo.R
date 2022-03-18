# Demographic Information
# Matt Kmiecik
# Started 10 January 2022

# Purpose: organize demographic information for manuscript

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/mmh-res.RData") # pca data
load("../output/ss-codes.RData") # participant codes
# load("../output/arm1_screenvisit_api_parsed.RData") # arm1 screen visit
# load("../output/arm1_avisit1_api_parsed.RData") # arm1 assessment visit
# load("../output/arm2_screenvisit_api_parsed.RData") # arm2 screen visit
# load("../output/arm2_avisit1_api_parsed.RData") # arm2 assessment visit

# reading in data NOT pulled through Redcap API
# this is because not all data needed was pulled through Redcap API
arm1_screen_data  <- read_csv("../data/arm-1-screen-data.csv")
arm1_avisit1_data <- read_csv("../data/arm-1-assessment-1-data.csv")
arm2_screen_data  <- read_csv("../data/arm-2-screen-data.csv")
arm2_avisit1_data <- read_csv("../data/arm-2-assessment-1-data.csv")


# Organizing ----
# These are the participant numbers used in the PCA
ss_pca <- rownames(pca_res$Fixed.Data$ExPosition.Data$X)

# ss_codes but only with the PCA subjects
ss_codes_narrow <- ss_codes %>% filter(ss %in% ss_pca)

arm1_screen_demo <- 
  arm1_screen_data %>% 
  select(
    record_number, 
    age = mh2_age, 
    contains("race"), 
    ethnicity = mh4_ethnicity,
    contains("have_you_ever_been_diagnos"),
    pregnancies = mh13_ofpregs,
    deliveries = mh14_deliveries,
    vagbirths = mh15_vagbirths,
    height = pe2a_height,	
    weight = pe2b_weight,	
    bmi = pe2c_bmi
  ) %>%
  left_join(., ss_codes %>% select(ss, arm1r), by = c("record_number" = "arm1r")) %>%
  relocate(ss) %>% # moves ss to the left
  filter(ss %in% ss_pca) %>% # filters out participants not in the PCA
  select(-record_number) # takes out record number

arm2_screen_demo <- 
  arm2_screen_data %>%
  select(
    record_number, 
    age = mh2_age, 
    contains("race"), 
    ethnicity = mh4_ethnicity,
    contains("have_you_ever_been_diagnos"),
    pregnancies = mh13_ofpregs,
    deliveries = mh14_deliveries,
    vagbirths = mh15_vagbirths,
    height = pe2a_height,	
    weight = pe2b_weight,	
    bmi = pe2c_bmi
  ) %>%
  left_join(., ss_codes %>% select(ss, arm2r), by = c("record_number" = "arm2r")) %>%
  relocate(ss) %>% # moves ss to the left
  filter(ss %in% ss_pca) %>% # filters out participants not in the PCA
  select(-record_number) # takes out record number
  
# combines into one demo df
demo_data <- 
  bind_rows(arm1_screen_demo, arm2_screen_demo) %>%
  mutate(age = ifelse(ss == 24 & is.na(age), 19, age)) # ss 24 was missing age

########################
#                      #
# continuous variables #
#                      #
########################

demo_data_continuous <- 
  demo_data %>%
  select(ss, age, height, weight, bmi) %>%
  pivot_longer(-ss) %>%
  filter(complete.cases(value)) %>%
  group_by(name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value),
    n = n()
    )
demo_data_continuous

########
#      #
# race #
#      #
########

demo_data_race <-
  demo_data %>%
  select(
    ss, 
    native_american = mh3_race___1, 
    asian = mh3_race___2, 
    native_hawaiin_pac_isl = mh3_race___3, 
    black = mh3_race___4, 
    white = mh3_race___5
    ) %>%
  #rowwise() %>%
  mutate(
    multi = ifelse(
      native_american+asian+native_hawaiin_pac_isl+black+white > 1,
      ss,
      NA
      )
    )

# multi race
demo_data_race %>% filter(complete.cases(multi)) %>% summarise(n = n())

# single race (1 person did not indicate a race)
demo_data_race %>% 
  filter(is.na(multi)) %>% 
  select(-multi) %>%
  pivot_longer(-ss) %>%
  group_by(name) %>%
  summarise(sum = sum(value))

################################################################
#                                                              #
# ethnicity (1=hispanic or latino; 2 = not hispanic or latino) #
#                                                              #
################################################################

demo_data %>% count(ethnicity)

#############
#           #
# Diagnoses #
#           #
#############

# Diagnosis Key
diagnos_key <- 
  tibble(
    diagnos = 0:19,
    label = c(
      "None", # 0
      "Painful Bladder Syndrome/Interstitial Cystitis", # 1
      "Chronic Pelvic Pain", # 2 
      "Fibroids", # 3
      "Endometriosis", # 4
      "Ovarian Cysts", # 5
      "Pelvic Inflammatory Disease", # 6
      "Dysmenorrhea", # 7
      "Kidney Stones", # 8
      "Inflammatory Bowel Disease", # 9
      "Irritable Bowel Syndrome", # 10
      "Chronic Constipation", # 11
      "Chronic Diarrhea", # 12
      "Migraine Headaches", # 13
      "Hypertension (High Blood Pressure)", # 14
      "Arthritis", # 15
      "Lower Back Pain", # 16
      "Cancer", # 17
      "Diabetes", # 18
      "Fibromyalgia" # 19
      )
  )

# diagnoses
diagnoses_counts <- 
  demo_data %>%
  select(ss, contains("diagnos")) %>%
  pivot_longer(-ss) %>%
  rename(diagnos = name) %>%
  mutate(
    diagnos = as.numeric(gsub("have_you_ever_been_diagnos___", "", diagnos))
    ) %>%
  filter(value != 0) %>%
  left_join(., diagnos_key, by = "diagnos") %>%
  count(label, diagnos) %>% arrange(diagnos)

# writes for manuscript
# uncomment to save out
#write_csv(diagnoses_counts, file = "../output/diagnoses-counts.csv")

# This is what we have for groups
ss_codes_narrow %>% count(group)

##########
#        #
# parity #
#        #
##########

demo_data %>% 
  select(ss, pregnancies, deliveries, vagbirths) %>%
  filter(is.na(pregnancies)) # one person is NA

parity_data <- 
  demo_data %>% 
  select(ss, pregnancies, deliveries, vagbirths) %>%
  filter(complete.cases(pregnancies)) %>%
  mutate(
    ever_pregnant = ifelse(pregnancies>0, 1, 0),
    one_or_more_delivs = ifelse(deliveries>0, 1, 0)
    )

# n ever pregnant
parity_data %>% count(ever_pregnant)

# n 1 or more deliveries
parity_data %>% count(one_or_more_delivs)



##################################
#                                #
# TAKING A LOOK AT ARM 2 OCP USE #
#                                #
##################################

# reading in OCP notes/subjects
ocp_notes <- read_excel(path = "../data/CRAMPP-phase2-deviations-01-06-2022.xlsx")

# 28 participants were initially randomized into OCPs vs. No OCPs
# however, only 22 were included into the PCA analyses
ocp_notes %>% filter(crampp_id %in% ss_pca) %>% count(completed)
ocp_notes %>% filter(crampp_id %in% ss_pca) %>% count(assignment)
ocp_notes %>% filter(crampp_id %in% ss_pca) %>% count(assignment, completed)


