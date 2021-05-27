# Pressure Pain Thresholds (PPTs) Preprocessing Script
# Matt Kmiecik
# Started 27 May 2021

# Purpose: load raw PPT data and clean/prep for analysis

source("r-prep.R") # Prepare R workspace

# Loads in data ----
#test <- 
  read_excel(path = "../data/ppt.xlsx", sheet = "matt-prep", na = c("MS", "ms")) %>%
  separate(ss, into = c("ss", "session")) %>% # some ss were named with visit
  mutate(
    ss = as.numeric(ss),
    visit = ifelse(is.na(session), visit, session),
    visit = as.numeric(visit),
    test = gsub(" ", "_", test) # removes space after PPTs
    ) %>%
  select(-session) %>%
  mutate(force = gsub("\\*", "_*", force)) %>% # this is to handle the *
  separate(force, into = c("force", "force_notes"), sep = "_") %>%
  mutate(
    force = as.numeric(force), 
    rate = as.numeric(rate),
    duration = as.numeric(duration)
    ) %>%
  # data editing due to notes (see notes)
  mutate(
    force = ifelse(
      ss == 13 & 
      visit == 1 & 
      test == "PPT_ext" & 
      trial == 2 & 
      site == "rShoulder",
    force + 15,
    force
    )
  ) %>%
  # left off with participant 38
  
