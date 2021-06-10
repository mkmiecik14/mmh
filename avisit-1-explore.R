# Redcap Data Exploration
# Matt Kmiecik
# Started 8 June 2021

# Purpose: perform data quality checks from the redcap data (see avisit-1-prepro.R)

source("r-prep.R") # Prepare R workspace

# Loads in data ----
load("../output/bladder-data.RData") # bladder data
load("../output/redcap-ppt-data.RData") # PPT data from redcap
load("../output/ss-codes.RData") # subject codes

# Bladder data ----

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
  bind_rows(arm1_temp, arm2_temp) %>% # combines arm1 and arm2
  filter(group %in% c("HC", "PBS", "DYSB", "DYS", "PAIN")) # eliminates NAs and EXCLUDE

# Looking at pain, urgency, volume, time for:
#   baseline (0)
#   first sensation (1)
#   first urge (2)
#   maximum tolerance (3)

bladder_task_data <- 
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
  ) %>%
  pivot_longer(!ss) %>%
  separate(name, into = c("time", "meas")) %>%
  filter(complete.cases(value)) %>%
  mutate(
    time = as.factor(time),
    time = fct_relevel(time, c("bs", "fs", "fu", "mt"))
    )

# summary stats
bladder_task_data_sum <- 
  bladder_task_data %>%
  group_by(time, meas) %>%
  summarise(
    M = mean(value), 
    N = n(), 
    SD = sd(value), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# bladder test plot
pj <- position_jitter(width = .1, height = 0)
pn <- position_nudge(x = .2, y = 0)
ggplot(bladder_task_data, aes(time, value)) +
  geom_point(alpha = 1/3, position = pj) +
  #geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) + # found a bug here
  geom_point(data = bladder_task_data_sum, aes(y = M), position = pn) +
  geom_errorbar(data = bladder_task_data_sum, aes(ymin = LL, ymax = UL, y = M), width = .05, position = pn) +
  geom_path(data = bladder_task_data_sum, aes(y = M, group = 1), position = pn, linetype = 2, alpha = 2/3) +
  theme_classic() +
  labs(x = "Time", y = "Value", caption = "95% CI error bars.") +
  facet_wrap(~meas, nrow = 3, scales = "free")

# Next take a look at McGill descriptors for bladder task

# Subject-wise
bladder_mcgill_ss <- 
  bladder_data_ss %>%
  select(ss, starts_with("bt11")) %>%
  pivot_longer(!ss) %>%
  separate(name, into = c("var", "mcgill")) %>%
  filter(complete.cases(value))

# Summary
bladder_mcgill_sum <- 
  bladder_mcgill_ss %>%
  group_by(mcgill) %>%
  summarise(
    M = mean(value), 
    N = n(), 
    SD = sd(value), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# McGill plot!
pj <- position_jitter(width = .1)
ggplot(bladder_mcgill_ss, aes(mcgill, value, color = mcgill)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_flat_violin(aes(fill = mcgill), position = position_nudge(x = .2, y = 0), alpha = 1/3) +
  geom_boxplot(position = position_nudge(x = -.2, y = 0), width = .1) +
  labs(x = "McGill Descriptor", y = "Rating") +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  scale_fill_manual(values = ghibli_palettes$PonyoMedium) +
  theme_minimal() +
  theme(legend.position = "none")

# another idea is to look at the timed ratings too!

# PPT Data from Redcap ----

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

# this should now have correct record numers to ss ids
redcap_ppt_data_ss <- 
  bind_rows(arm1_temp, arm2_temp) %>% # combines arm1 and arm2
  filter(group %in% c("HC", "PBS", "DYSB", "DYS", "PAIN")) # eliminates NAs and EXCLUDE

# Temporal Summation NRS ratings
conv_table <- tibble(name = letters, trial = 0:25)
ts_ss <- 
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
  left_join(., conv_table, by = "name") %>%
  filter(complete.cases(value))

# Summary
ts_sum <- 
  ts_ss %>% 
  group_by(trial) %>%
  summarise(
    M = mean(value), 
    N = n(), 
    SD = sd(value), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# NRS ratings plot
pj <- position_jitter(width = .3, height = .3)
ggplot(ts_ss, aes(factor(trial), value)) +
  geom_point(alpha = 1/3, position = pj)

# Summary plot
ggplot(ts_sum, aes(factor(trial), M, group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2) +
  geom_line() +
  geom_text(aes(label = N), position = position_nudge(y = .5)) +
  coord_cartesian(ylim = c(0, 6)) +
  theme_minimal()

# Max trial
ts_max_ss <-
  ts_ss %>%
  group_by(ss) %>%
  summarise(max_ts = max(trial)) %>%
  ungroup()

# Histogram - most subjects reach final trial
ggplot(ts_max_ss, aes(max_ts)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  labs(x = "Trial", y = "Frequency") +
  theme_minimal()

# Temporal Summation MLM 
ggplot(ts_ss, aes(trial, value, group = ss)) +
  stat_smooth(
    method = "lm",
    geom = "line", 
    alpha = 1/2, 
    se = FALSE
    ) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:10, minor_breaks = NULL) +
  theme_classic()

# First step is to subtract baseline from everyone
ts_ss_blcorrected <- 
  ts_ss %>%
  pivot_wider(id_cols = ss, names_from = c(name, trial), values_from = value) %>%
  mutate(across(b_1:k_10, ~ .x - a_0)) %>% # subtracts baseline from each trial
  select(-a_0) %>% # gets rid of baseline column
  pivot_longer(!ss) %>% 
  separate(name, into = c("name", "trial")) %>%
  mutate(trial = as.numeric(trial)) %>%
  filter(complete.cases(value))

# level 1 models
lvl1_ts_mod <-
  ts_ss_blcorrected %>%
  nest_by(ss) %>%
  mutate(mod = list(lm(value ~ 1 + scale(trial, scale = FALSE), data = data)))

# level 1 estimates
lvl1_ts_est <- 
  lvl1_ts_mod %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scaletrial, scale = FALSE", "trial_mc", term)
  )

# Participants with too little data (i.e., only 1 trial:
lvl1_ts_est %>% filter(!complete.cases(estimate))
ts_ss_blcorrected %>% filter(ss %in% c(15,85,316))
# be sure to remove these

lvl1_ts_est_clean <- 
  lvl1_ts_est %>% 
  filter(ss %nin% c(15, 85, 316)) # participants excluded for only 1 trial

# Intercepts
ggplot(lvl1_ts_est_clean %>% filter(term %in% "Intercept"), aes(estimate)) + 
  geom_histogram(binwidth = .5) +
   coord_cartesian(xlim = c(0, 10))

# Slopes
ggplot(lvl1_ts_est_clean %>% filter(term %in% "trial_mc"), aes(estimate)) + 
  geom_histogram(binwidth = .2)

# These participants had high slopes!
lvl1_ts_est_clean %>% filter(term %in% "trial_mc", estimate > 1.5)
ts_ss_blcorrected %>% filter(ss %in% c(16,67,129,226))


# After pain ratings for PPTs ----
ppt_pain <- 
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
  pivot_longer(!ss) %>%
  separate(name, into = c("time", "site")) %>%
  mutate(time = factor(time), time = fct_relevel(time, c("bl", "t1", "t2"))) %>%
  filter(complete.cases(value))

# External PPTs
ppt_pain_ext <-
  ppt_pain %>%
  filter(site %in% c("rshoulder", "rhip", "rknee", "forehead")) %>%
  pivot_wider(id_cols = ss, names_from = c(time, site), values_from = value) %>%
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
  select(-bl_rshoulder, -bl_rhip, -bl_rknee, -bl_forehead) %>%
  pivot_longer(!ss) %>%
  separate(name, into = c("time", "site")) %>%
  filter(complete.cases(value))

pd <- position_dodge(width = .3)
ggplot(ppt_pain_ext, aes(time, value, group = ss)) +
  geom_point(alpha = 1/3, position = pd) +
  geom_line(position = pd) +
  facet_wrap(~site)

# Averaging trials 1 and 2 together (both were already corrected for baseline)
ppt_pain_ext_ss <- 
  ppt_pain_ext %>% 
  group_by(ss, site) %>%
  summarise(m = mean(value), n = n()) %>%
  ungroup()
ppt_pain_ext_ss %>% filter(n < 2) # everyone included has two measures here

# Summary
ppt_pain_ext_sum <-
  ppt_pain_ext_ss %>%
  group_by(site) %>%
  summarise(
    M = mean(m), 
    N = n(), 
    SD = sd(m), 
    SEM = SD/sqrt(N), 
    LL = as.numeric(t.test(m, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(m, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# Summary plot for NRS external sites (corrected for baseline)
pj <- position_jitter(width = .1, height = .1)
pn <- position_nudge(x = .3, y = 0)
ggplot(ppt_pain_ext_ss, aes(site, m, color = site)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_point(data = ppt_pain_ext_sum, aes(y=M), position = pn) +
  geom_errorbar(
    data = ppt_pain_ext_sum, 
    aes(y=M, ymin = LL, ymax = UL), 
    position = pn, 
    width = .2
    ) +
  labs(x = "Site", y = "Mean NRS (baesline adjusted)", caption = "95% CI error bars.") +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  theme_minimal() +
  theme(legend.position = "none")

# Vaginal afterpain (corrected for baseline)
# Internal PPTs
ppt_pain_int <- 
  ppt_pain %>%
  filter(site %in% c("vaginal", "12", "5", "6", "7")) %>%
  pivot_wider(id_cols = ss, names_from = c(time, site), values_from = value) %>%
  mutate(across(t1_12:t2_7, ~ .x - bl_vaginal)) %>% # subtracting away baseline
  select(-bl_vaginal) %>% # removes baseline
  pivot_longer(!ss) %>%
  separate(name, into = c("time", "site")) %>%
  filter(complete.cases(value))

# Averaging trials 1 and 2 together (both were already corrected for baseline)
ppt_pain_int_ss <- 
  ppt_pain_int %>% 
  group_by(ss, site) %>%
  summarise(m = mean(value), n = n()) %>%
  ungroup()
ppt_pain_int_ss %>% filter(n < 2) # two participants do not have 2 trials

# Summary
ppt_pain_int_sum <-
  ppt_pain_int_ss %>%
  group_by(site) %>%
  summarise(
    M = mean(m), 
    N = n(), 
    SD = sd(m), 
    SEM = SD/sqrt(N), 
    LL = as.numeric(t.test(m, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(m, conf.level = 0.95)$conf.int[2])
  ) %>%
  ungroup()

# Summary plot for NRS external sites (corrected for baseline)
pj <- position_jitter(width = .1, height = .1)
pn <- position_nudge(x = .3, y = 0)
ggplot(ppt_pain_int_ss, aes(site, m, color = site)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_point(data = ppt_pain_int_sum, aes(y=M), position = pn) +
  geom_errorbar(
    data = ppt_pain_int_sum, 
    aes(y=M, ymin = LL, ymax = UL), 
    position = pn, 
    width = .2
  ) +
  labs(x = "Site", y = "Mean NRS (baesline adjusted)", caption = "95% CI error bars.") +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  theme_minimal() +
  theme(legend.position = "none")


# vaginal mcgill
vaginal_mcgill_ss <- 
  redcap_ppt_data_ss %>%
  select(
    ss,
    sharp = pt3_5a_sharp,
    pressing = pt3_5b_pressing,
    dull = pt3_5c_dull,
    prickling = pt3_5d_prickling
    ) %>%
  pivot_longer(!ss) %>%
  filter(complete.cases(value))
  
# Summary
vaginal_mcgill_sum <- 
  vaginal_mcgill_ss %>%
  group_by(name) %>%
  summarise(
    M = mean(value), 
    N = n(), 
    SD = sd(value), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[2])
  ) %>%
  ungroup()

# McGill plot!
pj <- position_jitter(width = .1)
ggplot(vaginal_mcgill_ss, aes(name, value, color = name)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_flat_violin(aes(fill = name), position = position_nudge(x = .2, y = 0), alpha = 1/3) +
  geom_boxplot(position = position_nudge(x = -.2, y = 0), width = .1) +
  labs(x = "McGill Descriptor", y = "Rating") +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  scale_fill_manual(values = ghibli_palettes$PonyoMedium) +
  theme_minimal() +
  theme(legend.position = "none")

# Cold pain ----

# Extracts data (pain and water temp)
coldpain_data <- 
  redcap_ppt_data_ss %>%
  select(ss, watertemp = pt_watertemp, coldpain = pt4b_cpmwater)

# Long format
coldpain_data_ss <- 
  coldpain_data %>%
  pivot_longer(!ss) %>%
  filter(complete.cases(value))

# Summary data
coldpain_data_sum <- 
  coldpain_data_ss %>%
  group_by(name) %>%
  summarise(
    M = mean(value), 
    N = n(), 
    SD = sd(value), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[2])
  )

# Histogram of cold pain
ggplot(coldpain_data_ss %>% filter(name %in% "coldpain"), aes(value)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  labs(x = "Cold Pain NRS", y = "Frequency")

# Histogram of water temperature
ggplot(coldpain_data_ss %>% filter(name %in% "watertemp"), aes(value)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Water Temperature (Celsius)", y = "Frequency")

# Wide data that rids of missing data (list-wise) for correlation
coldpain_data_wide <- coldpain_data %>% filter(complete.cases(.))
  
# Scatter plot
ggplot(coldpain_data_wide, aes(watertemp, coldpain)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic()

# Correlation
cor.test(x = coldpain_data_wide$watertemp, coldpain_data_wide$coldpain)

# Calculating Residuals
cold_mod <- lm(coldpain ~ 1 + watertemp, data = coldpain_data_wide)
cold_mod_aug <- broom::augment(cold_mod)
ggplot(cold_mod_aug, aes(.fitted)) + geom_histogram(binwidth = .2) # resid histo
