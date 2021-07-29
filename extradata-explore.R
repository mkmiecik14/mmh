# Extra Data Explore
# Matt Kmiecik
# Started 14 July 2021

# Purpose: This script explores data that was not included in the initial PCA
# analyses, but is interesting to see how the PCA results predict these measures

source("r-prep.R") # Prepare R workspace

# Loads in data ----
load("../output/ss-codes.RData") # subject codes
load("../output/redcap-bsi-data.RData") # BSI
load("../output/redcap-cmsi-data.RData") # CMSI
load("../output/redcap-menstrual-data.RData") # menstrual data
load("../output/redcap-gupi-data.RData") # GUPI
load("../output/redcap-ic-data.RData") # ICSI + ICPI
load("../output/redcap-rome-data.RData") # ROME
load("../output/redcap-promis-global-data.RData") # PROMIS global
load("../output/redcap-promis-pb-data.RData") # PROMIS pain behav (PB)
load("../output/redcap-promis-pi-data.RData") # PROMIS pain interference (PI)

# BSI
redcap_bsi_data
