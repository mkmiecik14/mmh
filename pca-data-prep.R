# PCA Data Preparation
# Matt Kmiecik
# Started 10 June 2021

# Purpose: to prepare the data for PCA analysis
# This includes:
#   - Data in wide format
#   - Preservation NAs (for later removal of ss listwise)
#   - Correcting elements discovered during exploration ( see *-explore.R )
#   - Variables for supplemental projections or coloring
#   - greater score indicates greater pain, greater unpleas, worse outcome

# Kevin:
# PPTS (both ext and int) will hang together (some bladder onto this factor)
# second factor - after pain ratings and visual + audio
# CPM and TS will load elsewhere

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/ss-codes.RData")            # subject codes
load("../output/bladder-data.RData")        # bladder data
load("../output/redcap-ppt-data.RData")     # PPT data from redcap
load("../output/ppt-data.RData")            # PPT data
load("../output/auditory-behav-data.RData") # auditory data
load("../output/vis-behav-data.RData")      # visual data

# This data frame helps ensure the same number of participants for each data
# element and helps with *_joins()
ss_codes_ss <- 
  ss_codes %>% 
  filter(group %nin% c("KID", "EXCLUDE")) %>% 
  select(ss)

###############
#             #
# VISUAL DATA #
#             #
###############

# Intercepts = average unpleasantness rating
# Slopes = rate of change

# Level 1 mods
lvl1_vis_mod <- 
  vis_behav_data %>% 
  nest_by(ss) %>%
  mutate(mod = list(lm(rating ~ 1 + scale(stim, scale = FALSE), data = data)))

# Level 1 ests
lvl1_vis_est <-
  lvl1_vis_mod %>%
  summarise(broom::tidy(mod)) %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scalestim, scale = FALSE", "stim_mc", term)
  ) %>%
  ungroup()

# Wide prep
vis_pca_data <- 
  left_join(ss_codes_ss, lvl1_vis_est, by = "ss") %>%
  pivot_wider(id_cols = ss, names_from = term, values_from = estimate) %>%
  rename(vis_mean = Intercept, vis_slope = stim_mc) %>%
  select(-`NA`) # getting rid of mysterious NA column

save(vis_pca_data, file = "../output/vis-pca-data.RData") # RData
write_csv(vis_pca_data, "../output/vis-pca-data.csv")     # CSV
rm(lvl1_vis_mod, lvl1_vis_est, vis_pca_data) # removes this section's objects

#################
#               #
# AUDITORY DATA #
#               #
#################

# Level 1 mods
lvl1_aud_mod <- 
  aud_edata %>% 
  nest_by(ss) %>%
  mutate(mod = list(lm(rating ~ 1 + scale(stim, scale = FALSE), data = data)))

# Level 1 ests
lvl1_aud_est <-
  lvl1_aud_mod %>%
  summarise(broom::tidy(mod)) %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scalestim, scale = FALSE", "stim_mc", term)
  ) %>%
  ungroup()

# Wide prep
aud_pca_data <- 
  left_join(ss_codes_ss, lvl1_aud_est, by = "ss") %>%
  pivot_wider(id_cols = ss, names_from = term, values_from = estimate) %>%
  rename(aud_mean = Intercept, aud_slope = stim_mc) %>%
  select(-`NA`) # getting rid of mysterious NA column

save(aud_pca_data, file = "../output/aud-pca-data.RData") # RData
write_csv(aud_pca_data, "../output/aud-pca-data.csv")     # CSV
rm(lvl1_aud_mod, lvl1_aud_est, aud_pca_data) # removes this section's objects

#######
#     #
# PPT #
#     #
#######

# The PPT data from redcap need to be cleaned for subject ID numbers, as the 
# record numbers do not line up from arm 1 and arm 2

# First step is to align record numbers with ss id's across arms 1 and 2
arm1_temp <- 
  redcap_ppt_data %>% 
  filter(redcap_event_name %in% "assessment_visit_1_arm_1") %>%
  left_join(., ss_codes, by = c("record_number" = "arm1r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm2r)
arm2_temp <- 
  redcap_ppt_data %>% 
  filter(redcap_event_name %in% "assessment_visit_1_arm_2") %>%
  left_join(., ss_codes, by = c("record_number" = "arm2r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm1r)

# this should now have correct record numbers to ss ids
redcap_ppt_data_ss <- 
  # combines arm1 and arm2 while removing ss that are KIDS or EXCLUDE (see above)
  left_join(ss_codes_ss, bind_rows(arm1_temp, arm2_temp), by = "ss")

# External Thresholds
ext_N_pca_data <- 
  left_join(ss_codes_ss, ppt_data, by = "ss") %>%
  filter(test == "PPT_ext", visit == 1) %>%
  group_by(ss, site) %>%
  # averages across two trials while mult by -1: greater value = more sensitive
  summarise(m = mean(force, na.rm = TRUE)*-1) %>% 
  ungroup() %>%
  pivot_wider(id_cols = ss, names_from = site, values_from = m) %>%
  rename_with(~paste0("ppt_N_", .), .cols = !ss)

# Internal Thresholds
int_N_pca_data <-
  left_join(ss_codes_ss, ppt_data, by = "ss") %>%
  filter(test == "PPT_int", visit == 1) %>%
  group_by(ss, site) %>%
  # averages across two trials while mult by -1: greater value = more sensitive
  summarise(m = mean(force, na.rm = TRUE)*-1) %>%
  ungroup() %>%
  pivot_wider(id_cols = ss, names_from = site, values_from = m) %>%
  rename_with(~paste0("ppt_N_", .), .cols = !ss)

save(ext_N_pca_data, file = "../output/ext-N-pca-data.RData")     # RData
write_csv(ext_N_pca_data, "../output/ext-N-pca-data.csv")         # CSV
save(int_N_pca_data, file = "../output/int-N-pca-data.RData")     # RData
write_csv(int_N_pca_data, "../output/int-N-pca-data.csv")         # CSV
rm(arm1_temp, arm2_temp, ext_N_pca_data, int_N_pca_data ) # removes this section's objects


###############################
#                             #
# CONDITIONED PAIN MODULATION #
#                             #
###############################

#! use left knee as there are more participants with this measurement than 
# left shoulder
cpm_pca_data <- 
  left_join(ss_codes_ss, ppt_data, by = "ss") %>%
  filter(
    test %in% "CPM", # narrows down to CPM only 
    visit == 1, # baseline visit only
    trial == 1 | trial == 2, # trial 1 and 2 to calc diff
    site %in% "lKnee" # left knee only as was done for everyone
    ) %>%
  pivot_wider(id_cols = ss, names_from = trial, values_from = force) %>%
  # calculation of the CPM effect; multiplied by -1 for PCA (higher = worse CPM)
  mutate(cpm_lknee = (`2` - `1`)*-1) %>%
  select(ss, cpm_lknee)

save(cpm_pca_data, file = "../output/cpm-pca-data.RData") # RData
write_csv(cpm_pca_data, "../output/cpm-pca-data.csv")     # CSV
rm(cpm_pca_data) # removes this section's objects


######################
#                    #
# TEMPORAL SUMMATION #
#                    #
######################

conv_table <- tibble(name = letters, trial = 0:25) # conversion table for trial# 

# Long chaining to subtract out baseline, perform MLM, and calc max trial
ts_mod <- 
  redcap_ppt_data_ss %>%
  select(
    ss, 
    pt5a,
    pt5b,
    pt5c,
    pt5d,
    pt5e,
    pt5f,
    pt5g,
    pt5h,
    pt5i,
    pt5j,
    pt5k
  ) %>%
  pivot_longer(!ss) %>%
  mutate(name = gsub("pt5", "", name)) %>%
  left_join(., conv_table, by = "name") %>% # conversion to trial number
  pivot_wider(id_cols = ss, names_from = trial, values_from = value) %>%
  mutate(across(.cols = `1`:`10`, ~ .x - `0`)) %>% # subtracts out baseline
  select(-`0`) %>% # removes baseline column
  pivot_longer(!ss, names_to = "trial") %>%
  mutate(trial = as.numeric(trial)) %>%
  filter(complete.cases(value)) %>% # easier to gain max trial and models
  nest_by(ss) %>%
  mutate(
    mod = list(lm(value ~ 1 + scale(trial, scale = FALSE), data = data)), # multilevel models
    ts_max = max(data$trial) # max trial
  )

# Estimates
ts_est <- 
  ts_mod %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("[\\(\\)]", "", term),
    term = gsub("Intercept", "ts_mean", term),
    term = gsub("scaletrial, scale = FALSE", "ts_slope", term)
  ) %>%
  pivot_wider(id_cols = ss, names_from = term, values_from = estimate)

# Temporal Summation data
ts_pca_data <-
  left_join(ss_codes_ss, ts_est, by = "ss") %>%
  left_join(., ts_mod %>% select(ss, ts_max), by = "ss")

save(ts_pca_data, file = "../output/ts-pca-data.RData") # RData
write_csv(ts_pca_data, "../output/ts-pca-data.csv")     # CSV
rm(conv_table, ts_mod, ts_est, ts_pca_data) # removes this section's objects

#####################
#                   #
# BLADDER TASK DATA #
#                   #
#####################

# First step is to align record numbers with ss id's across arms 1 and 2
arm1_temp <- 
  bladder_data %>% 
  filter(redcap_event_name %in% "assessment_visit_1_arm_1") %>%
  left_join(., ss_codes, by = c("record_number" = "arm1r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm2r)
arm2_temp <- 
  bladder_data %>% 
  filter(redcap_event_name %in% "assessment_visit_1_arm_2") %>%
  left_join(., ss_codes, by = c("record_number" = "arm2r")) %>%
  mutate(ss = ss.y) %>%
  select(-ss.x, -ss.y, -arm1ref, -arm1r)

# this should now have correct record numbers to ss ids
bladder_data_ss <- 
  # combines arm1 and arm2 while removing ss that are KIDS or EXCLUDE (see above)
  left_join(ss_codes_ss, bind_rows(arm1_temp, arm2_temp), by = "ss")

# Extracting specific bladder task data measures
bladder_pca_data <- 
  bladder_data_ss %>%
  select(
    ss, 
    bs_pain = bt3_baselinepain, 
    bs_vol = bt4_baselinevol,
    #fs_time = bt10a_fstime,
    fs_urg = bt10a_fsurg,
    fs_pain = bt10a_fspain,
    fs_vol = bt10a_fsvol,
    #fu_time = bt10b_futime,
    fu_urg = bt10b_fuurg,
    fu_pain = bt10b_fupain,
    fu_vol = bt10b_fuvol,
    #mt_time = bt10c_mttime,
    mt_urg = bt10c_mturg,
    mt_pain = bt10c_mtpain,
    mt_vol = bt12a_mtvol
  )

save(bladder_pca_data, file = "../output/bladder-pca-data.RData") # RData
write_csv(bladder_pca_data, "../output/bladder-pca-data.csv")     # CSV
rm(arm1_temp, arm2_temp, bladder_pca_data) # removes this section's objects
# bladder_data_ss not removed because it is used during McGill descriptors

###################
#                 #
# AFTER PAIN PPTs #
#                 #
###################

# External and Internal
after_pain_long <- 
  redcap_ppt_data_ss %>%
  select(
    ss,
    bl_rshoulder = pt2a_shoulder,
    bl_rhip = pt2b_hip,
    bl_rknee = pt2c_knee,
    bl_forehead = pt2d_forehead,
    bl_vaginal = pt2e_vag,
    t1_rshoulder = pt3a1,
    t1_rhip = pt3b1,
    t1_rknee = pt3c1,
    t1_forehead = pt3d1,
    t2_rshoulder = pt3a2,
    t2_rhip = pt3b2,
    t2_rknee = pt3c2,
    t2_forehead = pt3d2,
    t1_12 = pt3e1,
    t1_5 = pt3f1,
    t1_6 = pt3g1,
    t1_7 = pt3h1,
    t2_12 = pt3e2,
    t2_5 = pt3f2,
    t2_6 = pt3g2,
    t2_7 = pt3h2
  ) %>%
  # subtracting out baseline from each trial (external PPTs)
  mutate(
    t1_rshoulder = t1_rshoulder - bl_rshoulder,
    t1_rhip =  t1_rhip - bl_rhip,
    t1_rknee =  t1_rknee - bl_rknee,
    t1_forehead = t1_forehead - bl_forehead,
    t2_rshoulder =  t2_rshoulder - bl_rshoulder,
    t2_rhip = t2_rhip - bl_rhip,
    t2_rknee = t2_rknee - bl_rknee,
    t2_forehead = t2_forehead - bl_forehead
  ) %>%
  # subtracting out baseline from each trial (internal PPTs)
  mutate(across(t1_12:t2_7, ~ .x - bl_vaginal)) %>%
  select(-bl_vaginal, -bl_rshoulder, -bl_rhip, -bl_rknee, -bl_forehead) %>% # removes baseline
  pivot_longer(!ss) %>%
  separate(name, into = c("time", "site"))

# Averages pain across trials
after_pain_sum <- 
  after_pain_long %>% 
  group_by(ss, site) %>% 
  summarise(after_pain = mean(value, na.rm = TRUE)) %>%
  ungroup()

# Widens data
afterpain_pca_data <-
  after_pain_sum %>% 
  pivot_wider(
    id_cols = ss, 
    names_from = site,
    names_prefix = "after_pain_",
    values_from = after_pain
    )

save(afterpain_pca_data, file = "../output/afterpain-pca-data.RData") # RData
write_csv(afterpain_pca_data, "../output/afterpain-pca-data.csv")     # CSV
rm(after_pain_long, after_pain_sum, afterpain_pca_data) # removes this section's objects


######################
#                    #
# McGill Descriptors #
#                    #
######################

# Bladder task
bladder_mcgill_ss <- 
  bladder_data_ss %>%
  select(
    ss,
    bladder_sharp = bt11a_sharp, 
    bladder_pressing = bt11b_pressing,
    bladder_dull = bt11d_dull,
    bladder_prickling = bt11d_prickling
    )

# Internal PPTs
vaginal_mcgill_ss <- 
  redcap_ppt_data_ss %>%
  select(
    ss,
    vag_sharp = pt3_5a_sharp,
    vag_pressing = pt3_5b_pressing,
    vag_dull = pt3_5c_dull,
    vag_prickling = pt3_5d_prickling
  ) 

# McGill PCA data
mcgill_pca_data <- inner_join(bladder_mcgill_ss, vaginal_mcgill_ss, by = "ss")

save(mcgill_pca_data, file = "../output/mcgill-pca-data.RData") # RData
write_csv(mcgill_pca_data, "../output/mcgill-pca-data.csv")     # CSV
rm(bladder_mcgill_ss, vaginal_mcgill_ss, mcgill_pca_data, bladder_data_ss) # removes this section's objects


#############
#           #
# COLD PAIN #
#           #
#############

# Selects variables of interest
coldpain_data <- 
  redcap_ppt_data_ss %>%
  select(ss, watertemp = pt_watertemp, coldpain = pt4b_cpmwater)

# removes cases listwise for modeling
coldpain_data_ss <- coldpain_data %>% filter(complete.cases(.))

# Corrects for effect of water temperature (see explore script) by extracting
# residuals from linear model
coldpain_mod <- lm(coldpain ~ 1 + watertemp, data = coldpain_data_ss)
coldpain_resids <- 
  tibble(
    ss = coldpain_data_ss$ss, # subject numbers
    coldpain_resid = broom::augment(coldpain_mod)$.resid # residuals
  )

# Coldpain data for PCA
coldpain_pca_data <- 
  ss_codes_ss %>% 
  left_join(., coldpain_data, by = "ss") %>%
  left_join(., coldpain_resids, by = "ss") %>%
  select(-watertemp) # don't need this for PCA

save(coldpain_pca_data, file = "../output/coldpain-pca-data.RData") # RData
write_csv(coldpain_pca_data, "../output/coldpain-pca-data.csv")     # CSV
# removes this section's objects
rm(
  coldpain_data, 
  coldpain_data_ss, 
  coldpain_mod, 
  coldpain_resids, 
  coldpain_pca_data
  ) 

# Remaining script objects to remove
rm(
  ss_codes, 
  ss_codes_ss, 
  aud_edata, 
  bladder_data, 
  ppt_data, 
  redcap_ppt_data, 
  redcap_ppt_data_ss,
  vis_behav_data
)



