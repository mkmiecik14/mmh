# Pressure Pain Thresholds (PPTs) Visualization Script
# Matt Kmiecik
# Started 1 June 2021

# Purpose: load raw PPT data and clean/prep for analysis

source("r-prep.R") # Prepare R workspace

# Load data ----
load("../output/ss-codes.RData") # subject codes
load("../output/ppt-data.RData") # PPT data

# Exploration ----
# Adding groups to remove the kid's data
ppt_data_grps <- 
  ppt_data %>% 
  left_join(., ss_codes, by = "ss") %>% # adds groups
  filter(group %nin% c("KID")) # removes kids

# Examining if there are missing subjects that can be salvaged in a different
# data sheet

# First, let's examine external PPTs (PPT_ext)
ppt_ext_ss <- 
  ppt_data_grps %>% 
  filter(test == "PPT_ext", visit == 1) %>%
  group_by(ss, site) %>%
  summarise(
    m = mean(force, na.rm = TRUE), 
    n = n(), 
    n_na = sum(is.na(force)),
    n_mean = n - n_na) %>%
  ungroup()

# only 1 subject has missing values for both trials:
ppt_ext_ss %>% count(n_mean)

# Dealing with n participants
length(unique(ppt_ext_ss$ss))

# Examining distributions
ggplot(ppt_ext_ss, aes(m)) +
  geom_histogram(binwidth = 4) +
  labs(x = "Force (Newtons)", y = "Frequency") + 
  facet_wrap(~site)

# Raincloud plots
pj <- position_jitter(width = .1)
ggplot(ppt_ext_ss, aes(site, m, color = site, fill = site)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(shape = 1, position = pj, alpha = 2/3) +
  geom_boxplot(aes(fill = NULL), width = .2, position = position_nudge(x = -.25, y = 0)) +
  scale_fill_manual(values = ghibli_palettes$MononokeMedium) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Site", y = "Force (Newtons)") +
  theme_minimal() +
  theme(legend.position = "none")

# Means and 95% CIs
ppt_ext_sum <- 
  ppt_ext_ss %>%
  filter(complete.cases(m)) %>% # removes missing values
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

# Mean plots with 95% CI
pj <- position_jitter(width = .1)
ggplot(ppt_ext_sum, aes(site, M, color = site)) +
  geom_point(data = ppt_ext_ss, aes(y = m), shape = 1, position = pj, alpha = 1/3) +
  geom_point() +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Site", y = "Mean Force (Newtons)", caption = "95% CI error bars.") +
  theme_minimal() +
  theme(legend.position = "none")
# Given the non-overlapping CIs, I would keep these measures separate for PCA
  

# Internal PPTs ----
ppt_int_ss <- 
  ppt_data_grps %>% 
  filter(test == "PPT_int", visit == 1) %>%
  group_by(ss, site) %>%
  summarise(
    m = mean(force, na.rm = TRUE), 
    n = n(), 
    n_na = sum(is.na(force)),
    n_mean = n - n_na) %>%
  ungroup()

# summary of missing vs. 1 vs. 2 data points
ppt_int_ss %>% count(n_mean)

# Dealing with n participants
length(unique(ppt_ext_ss$ss))

# Examining distributions
ggplot(ppt_int_ss, aes(m)) +
  geom_histogram(binwidth = 3) +
  labs(x = "Force (Newtons)", y = "Frequency") + 
  facet_wrap(~site)

# Raincloud plots
pj <- position_jitter(width = .1)
ggplot(ppt_int_ss, aes(site, m, color = site, fill = site)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(shape = 1, position = pj, alpha = 2/3) +
  geom_boxplot(aes(fill = NULL), width = .2, position = position_nudge(x = -.25, y = 0)) +
  scale_fill_manual(values = ghibli_palettes$MononokeMedium) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Site", y = "Force (Newtons)") +
  theme_minimal() +
  theme(legend.position = "none")

# Means and 95% CIs
ppt_int_sum <- 
  ppt_int_ss %>%
  filter(complete.cases(m)) %>% # removes missing values
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

# Mean plots with 95% CI
pj <- position_jitter(width = .1)
ggplot(ppt_int_sum, aes(site, M, color = site)) +
  geom_point(data = ppt_int_ss, aes(y = m), shape = 1, position = pj, alpha = 1/3) +
  geom_point() +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Site", y = "Mean Force (Newtons)", caption = "95% CI error bars.") +
  theme_minimal() +
  theme(legend.position = "none")

# CPM ----
# Earlier participants will have different CPM procedures
# Most participants did the following:
# Trial 1 (baseline): left shoulder and left knee PPT
# Trial 2: left knee PPT after 20s ice water bath, removed hand, then did left shoulder PPT
# Trial 3: left shoulder and left knee PPT after 5 min
# Trial 4: left shoulder and left knee PPT after 10 min

cpm_ss <- ppt_data_grps %>% filter(test == "CPM", visit == 1)

cpm_ss %>% count(trial, site) # trial counts
cpm_ss %>% filter(is.na(force)) # surprisingly few NAs

# Examining distributions
# histograms
ggplot(cpm_ss, aes(force)) +
  geom_histogram(binwidth = 4) +
  labs(x = "Force (Newtons)", y = "Frequency") + 
  facet_grid(site~trial)

# density plots
ggplot(cpm_ss, aes(force, color = site, fill = site)) +
  geom_density(alpha = 1/2) +
  labs(x = "Force (Newtons)", y = "Density") + 
  facet_wrap(~trial, nrow = 1) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom")

# summary stats
cpm_sum <- 
  cpm_ss %>%
  group_by(trial, site) %>%
  summarise(
    M = mean(force, na.rm = TRUE), 
    N = sum(!is.na(force)), 
    SD = sd(force, na.rm = TRUE),
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# Summary data of all trials
pd <- position_dodge(width = .2) 
ggplot(cpm_sum, aes(trial, M, group = site, color = site)) +
  geom_point(data = cpm_ss, aes(y = force), position = pj, shape = 1, alpha = 1/3) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2, position = pd) +
  geom_path(position = pd) +
  theme_minimal() +
  labs(x = "Trial", y = "Mean Force (Newtons)", caption = "95% CI error bars.") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Let's just focus on trial 2 - trial 1 difference
# eliminate anyone without repeated measures here
cpm_ss_wide <-
  cpm_ss %>% 
  filter(trial == 1 | trial == 2) %>%
  select(ss, test, trial, site, force) %>%
  pivot_wider(id_cols = ss, names_from = c(site, trial), values_from = force) %>%
  mutate(lKnee = lKnee_2 - lKnee_1, lShoulder = lShoulder_2 - lShoulder_1)

# converts back to long to simplify drops
cpm_ss_long <- 
  cpm_ss_wide %>%
  select(ss, lKnee, lShoulder) %>%
  pivot_longer(
    cols = c(lKnee, lShoulder), 
    names_to = "site", 
    values_to = "diff_force"
    ) %>%
  filter(complete.cases(diff_force)) # elimiates missing data
  
# seperate dfs for knee and shoulder
cpm_lKnee <- cpm_ss_long %>% filter(site %in% "lKnee")          # lKnee
cpm_lShoulder <- cpm_ss_long %>% filter(site %in% "lShoulder")  # lShoulder

# LEFT KNEE SUBJECT-WISE
cpm_ss_lKnee <-
  cpm_ss %>% 
  filter(
    site %in% "lKnee",
    ss %in% cpm_lKnee$ss, # only ss with complete knee data
    trial == 1 | trial == 2 # trial 1 vs. 2
    )

# LEFT KNEE SUMMARY
cpm_ss_lKnee_sum <- 
  cpm_ss_lKnee %>%
  group_by(trial) %>%
  summarise(
    M = mean(force), 
    N = n(), 
    SD = sd(force), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# Raw data - left knee
pd <- position_dodge(width = .1) 
ggplot(cpm_ss_lKnee_sum, aes(trial, M, group = 1)) +
  geom_point(data = cpm_ss_lKnee, aes(y = force, group = ss), shape = 16, position = pd, color = ghibli_palettes$MononokeMedium[3]) +
  geom_path(data = cpm_ss_lKnee, aes(y = force, group = ss), position = pd, color = ghibli_palettes$MononokeMedium[3], alpha = 1/3) +
  labs(x = "Trial", y = "Force") +
  scale_x_continuous(breaks = c(1, 2)) +
  theme_classic()

# Mean CPM effect - left knee
pd <- position_dodge(width = .1) 
ggplot(cpm_ss_lKnee_sum, aes(trial, M)) +
  geom_bar(stat = "identity", color = "black", fill = ghibli_palettes$MononokeLight[3], width = .3) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .1, color = "black") +
  labs(x = "Trial", y = "Force", caption = "95% CI error bars.") +
  scale_x_continuous(breaks = c(1, 2)) +
  theme_classic()


# LEFT SHOULDER SUBJECT-WISE
cpm_ss_lShoulder <-
  cpm_ss %>% 
  filter(
    site %in% "lShoulder",
    ss %in% cpm_lShoulder$ss, # only ss with complete knee data
    trial == 1 | trial == 2 # trial 1 vs. 2
  )

# LEFT KNEE SUMMARY
cpm_ss_lShoulder_sum <- 
  cpm_ss_lShoulder %>%
  group_by(trial) %>%
  summarise(
    M = mean(force), 
    N = n(), 
    SD = sd(force), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[2])
  ) %>%
  ungroup()

# Raw data - left shoulder
pd <- position_dodge(width = .1) 
ggplot(cpm_ss_lShoulder_sum, aes(trial, M, group = 1)) +
  geom_point(data = cpm_ss_lShoulder, aes(y = force, group = ss), shape = 16, position = pd, color = ghibli_palettes$MononokeMedium[3]) +
  geom_path(data = cpm_ss_lShoulder, aes(y = force, group = ss), position = pd, color = ghibli_palettes$MononokeMedium[3], alpha = 1/3) +
  labs(x = "Trial", y = "Force") +
  scale_x_continuous(breaks = c(1, 2)) +
  theme_classic()

# Mean CPM effect - left knee
pd <- position_dodge(width = .1) 
ggplot(cpm_ss_lShoulder_sum, aes(trial, M)) +
  geom_bar(stat = "identity", color = "black", fill = ghibli_palettes$MononokeLight[3], width = .3) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .1, color = "black") +
  labs(x = "Trial", y = "Force", caption = "95% CI error bars.") +
  scale_x_continuous(breaks = c(1, 2)) +
  theme_classic()

# Temporal Summation ----
ts_ss <- 
  ppt_data_grps %>%
  filter(test %in% "TS", visit == 1) %>%
  filter(complete.cases(force)) # elimiates trials with missing/max out

# plotting TS trajectories
pd <- position_dodge(width = .2) 
ggplot(ts_ss, aes(as.factor(trial), force, group = ss)) +
  geom_point(position = pd, shape = 16, alpha = 1/3) +
  geom_path(position = pd, alpha = 1/3) +
  labs(x = "Trial", y = "Force (Newtons)") +
  theme_classic()

# Raincloud
ggplot(
  ts_ss %>% mutate(trial = as.factor(trial)), 
  aes(trial, force, group = trial, color = trial, fill = trial)
  ) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(shape = 1, position = pj, alpha = 2/3) +
  geom_boxplot(aes(fill = NULL), width = .2, position = position_nudge(x = -.25, y = 0)) +
  coord_flip() +
  scale_fill_grey(start = .8, end = .2) +
  scale_color_grey(start = .8, end = .2) +
  labs(x = "Site", y = "Force (Newtons)") +
  theme_minimal() +
  theme(legend.position = "none")

# Histogram
ggplot(ts_ss, aes(force)) +
  geom_histogram(binwidth = 3) +
  facet_wrap(~trial)
  
# TS summary by trial
ts_sum <- 
  ts_ss %>%
  group_by(trial) %>%
  summarise(
    M = mean(force), 
    N = n(), 
    SD = sd(force), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(force, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# summary plot
ggplot(ts_sum, aes(trial, M)) +
  geom_point(
    data = ts_ss, 
    aes(y = force), 
    shape = 1, 
    position = pj, 
    color = ghibli_palettes$YesterdayMedium[2], 
    alpha = 1/3
    ) +
  geom_bar(
    stat = "identity", 
    color = "black", 
    fill = ghibli_palettes$YesterdayMedium[2], 
    alpha = 1/2
    ) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2) +
  labs(x = "Trial", y = "Mean Force (Newtons)", caption = "95% CI error bars.") +
  theme_classic()

# Linear modeling to look at the intercept and slope
# models
ts_mod <- 
  ts_ss %>%
  nest_by(ss) %>%
  mutate(mod = list(lm(force ~ 1 + scale(trial, scale = FALSE), data = data)))

# estimates
ts_est <- 
  ts_mod %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scaletrial, scale = FALSE", "trial_mc", term)
    )

# Intercepts
ggplot(ts_est %>% filter(term %in% "Intercept"), aes(estimate)) +
  geom_histogram(binwidth = 4)

# Slopes
ggplot(ts_est %>% filter(term %in% "trial_mc"), aes(estimate)) +
  geom_histogram()

# Participants with outlier slopes
outlier_slopes <- 
  ts_est %>% filter(term %in% "trial_mc", estimate < -1 | estimate > 1)

# Plotting outlier slopes
ts_ss %>% 
  filter(ss %in% outlier_slopes$ss) %>%
  ggplot(., aes(as.factor(trial), force, group = ss)) +
  geom_point(position = pd, shape = 16, alpha = 1/3) +
  geom_path(position = pd, alpha = 1/3) +
  labs(x = "Trial", y = "Force (Newtons)") +
  theme_classic()
  
# Estimating mean intercept and slope
ts_lvl2_mod <- 
  ts_est %>%
  nest_by(term) %>%
  mutate(mod = list(lm(estimate ~ 1, data = data)))

# Level 2 estimates
ts_lvl2_mod %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    source = unique(ts_est$term),
    term = gsub("[\\(\\)]", "", term),
  )

# Slopes 
ggplot(ts_ss, aes(trial, force, group = ss)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", alpha = 1/3)

# Examining the difference between the first and 10th or final rating
# see Hellman et al., 2020
ts_diff <- 
  ts_ss %>% 
  group_by(ss) %>%
  filter(trial == min(trial) | trial == max(trial)) %>% # filters out inter-trials
  summarise(
    ts = -diff(force), # positive values indicate increased sensitivity 
    min = min(trial), 
    max = max(trial)
    ) %>% 
  ungroup()

ts_diff %>%
  summarise(
    M = mean(ts), 
    N = n(), 
    SD = sd(ts), 
    SEM = SD/sqrt(N), 
    LL = as.numeric(t.test(ts, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(ts, conf.level = 0.95)$conf.int[2])
    )

ts_diff %>% count(max)

