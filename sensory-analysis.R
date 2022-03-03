# Sensory Analysis
# Matt Kmiecik
# Started 03 MARCH 2022

#* Purpose: This script explores the added variability that is explained by
#* including supraspinal measures of visual and auditory stimulation
#* over and above traditional QST and bladder sensitivity (i.e., bladder test)

source("r-prep.R") # Prepares R workspace

# Loading data ----
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results
load("../output/ss-codes.RData") # subject codes
load("../output/pca-data-all.rda") # pca data

# Calculating pelvic pain outcome variable as an average of:
# 1) urination_pain_last_week
# 2) bowel_mov_pain_last_week
# 3) mens_nonmens_pain_week

# narrows data
pelvic_pain_data <- 
  complete_extra_annual_data %>% 
  select(
    ss, 
    year, 
    timestamp, 
    days_from_baseline, 
    urination_pain_last_week, 
    bowel_mov_pain_last_week, 
    mens_nonmens_pain_week
  )

# long-format
pelvic_pain_data_long <- 
  pelvic_pain_data %>%
  select(-timestamp) %>%
  pivot_longer(cols = c(-ss, -year, -days_from_baseline))

# narrow down the participants in PCA results
# PCA subject numbers
pca_ss <- as.numeric(rownames(pca_res$Fixed.Data$ExPosition.Data$fi)) 

# retains only those in PCA
pelvic_pain_data_pca_ss <- pelvic_pain_data %>% filter(ss %in% pca_ss)
length(unique(pelvic_pain_data_pca_ss$ss)) == length(pca_ss) # same ss

###############
#             #
# AVERAGE VAS #
#             #
###############

pelvic_pain_data_pca_ss %>% filter(!complete.cases(.))

# computes average here
pelvic_pain_avg <- 
  pelvic_pain_data_pca_ss %>%
  filter(complete.cases(.)) %>% # removes those subjects with missing data
  pivot_longer(c(-ss, -year, -timestamp, -days_from_baseline)) %>%
  group_by(ss, year, timestamp, days_from_baseline) %>%
  summarise(pelvic_pain = mean(value), n = n()) %>%
  ungroup()

pelvic_pain_avg %>% filter(n != 3) # all have three observations/timepoint
pelvic_pain_avg %>% filter(is.na(pelvic_pain)) # no missing here

##############
#            #
# Predictors #
#            #
##############

# z score each measure that was used in the PCA for easy combination
#  at this point all measures are pointing in the same direction
pca_data_z <- 
  pca_data_keep %>% 
  mutate(across(-ss, ~as.numeric(scale(.x)))) # computes z scores

# checking work
apply(pca_data_z, 2, mean) # means should all be zero
apply(pca_data_z, 2, sd) # sd should all be 1

# creates predictors
pca_data_z_sum <- 
  pca_data_z %>% 
  mutate(
    supra = aud_mean + aud_slope + vis_mean + vis_slope, # 4
    bladder = bs_pain + fs_pain + fu_pain + mt_pain + fs_urg + fu_urg + 
      mt_urg + bladder_sharp + bladder_pressing + bladder_dull + bladder_prickling, # 11
    qst = after_pain_12 + after_pain_5 + after_pain_6 + after_pain_7 + 
      after_pain_forehead + after_pain_rhip + after_pain_rknee + 
      after_pain_rshoulder + coldpain_resid + cpm_lknee + ppt_N_rForehead + 
      ppt_N_rHip + ppt_N_rKnee + ppt_N_rShoulder + ppt_N_12 + ppt_N_5 + 
      ppt_N_6 + ppt_N_7 + vag_sharp + vag_pressing + vag_dull + vag_prickling + 
      ts_mean + ts_slope + ts_max #25
  )



