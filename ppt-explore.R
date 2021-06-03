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

