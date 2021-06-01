# Subject Codes Preprocessing Script
# Matt Kmiecik
# Started 1 June 2021

# Purpose: load subject codes and clean/prep for analysis

source("r-prep.R") # Prepare R workspace

# Loads in data/prepares ----
ss_codes <-
  read_excel("../data/codes_better_v9.xlsx") %>%
  select(
    ss = subject_id, 
    arm1r,
    arm1ref,
    arm2r, 
    group = FinalGroup, 
    endo = Endo,
    notes1 = `reassignment notes`,
    notes2 = `Group Special Notes`,
    notes3 = othernotes,
    )

# Saving data ----
save(ss_codes, file = "../output/ss-codes.RData") # RData
write_csv(ss_codes, file = "../output/ss-codes.csv") # CSV 

rm(ss_codes) # cleans script object(s)