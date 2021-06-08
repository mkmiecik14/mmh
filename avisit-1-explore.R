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

ggplot(ts_max_ss, aes(max_ts)) +
  geom_histogram(binwidth = 1)

# run MLM for TS (be sure to cap at 6)


# after pain ratings during PPTs




