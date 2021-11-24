# API ICSI PLUS EXPLORE
# Matt Kmiecik
# Started 23 NOV 2021

#* Purpose: This script explores and analyzes the annual data that was 
#* preprocessed in `api-icsiplus-prepro.R`. Combining several different measures
#* of pelvic/nonpelvic pain main increase varibility over time

source("r-prep.R") # Prepares R workspace

# Loading data ----
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results

###############
#             #
# Exploration #
#             #
###############

# First, exploring one option of pelvic pain. A z-score combining:
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

# Histogram
ggplot(pelvic_pain_data_long, aes(value)) +
  geom_histogram(binwidth = 5) +
  facet_grid(name~year)

# Density plot
ggplot(pelvic_pain_data_long, aes(value, color = name)) +
  geom_density(size = .7) +
  facet_wrap(~year) +
  theme_classic() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

# calculating Z-scores
# first, narrow down the participants in PCA results
pca_ss <- as.numeric(rownames(pca_res$Fixed.Data$ExPosition.Data$fi)) # PCA subject numbers

# retains only those in PCA
pelvic_pain_data_pca_ss <- pelvic_pain_data %>% filter(ss %in% pca_ss)

length(unique(pelvic_pain_data_pca_ss$ss)) == length(pca_ss) # same ss

# calculates z scores long format
pelvic_pain_data_pca_ss_z_long <- 
  pelvic_pain_data_pca_ss %>%
  filter(complete.cases(.)) %>% # removes participants with missing data
  pivot_longer(cols = c(-ss, -year, -timestamp, -days_from_baseline)) %>%
  group_by(year, name) %>%
  mutate(z_value = as.numeric(scale(value))) %>% # zscore
  ungroup()

# proof that z-scores were calculated
pelvic_pain_data_pca_ss_z_long %>%
  group_by(year, name) %>%
  summarise(m = mean(value), m_z = mean(z_value), sd = sd(value), sd_z = sd(z_value)) %>%
  ungroup()

# z-scores in wide format
pelvic_pain_data_pca_ss_z_wide <- 
  pelvic_pain_data_pca_ss_z_long %>%
  pivot_wider(
    id_cols = c(ss, year, timestamp, days_from_baseline), 
    names_from = name, 
    values_from = c(value, z_value)
    ) %>%
  # summed z-score
  mutate(
    pelvic_pain = 
      z_value_urination_pain_last_week + 
      z_value_bowel_mov_pain_last_week + 
      z_value_mens_nonmens_pain_week
    )

# histogram
ggplot(pelvic_pain_data_pca_ss_z_wide, aes(pelvic_pain)) +
  geom_histogram(binwidth = .5) +
  facet_wrap(~year)

# density plot
ggplot(
  pelvic_pain_data_pca_ss_z_wide, 
  aes(pelvic_pain, group = year, color = factor(year))
  ) +
  geom_density()

# change over time
ggplot(pelvic_pain_data_pca_ss_z_wide, aes(year, days_from_baseline)) +
  geom_point(position = position_jitter(width = .2, height = 0), alpha = 1/3)

pd <- position_dodge(width = .2)
# suspicious subjects with late year 1
sus_ss <- 
  pelvic_pain_data_pca_ss_z_wide %>%
  filter(year == 1, days_from_baseline > 500)

ggplot(
  pelvic_pain_data_pca_ss_z_wide %>% filter(ss %in% sus_ss$ss), 
  aes(year, days_from_baseline, group = ss)
  ) +
  geom_point(position = pd, alpha = 1/3) +
  geom_path(position = pd, alpha = 1/3)

# longitudinal plot 1
ggplot(pelvic_pain_data_pca_ss_z_wide, aes(year, pelvic_pain)) +
  geom_point(position = pd, alpha = 1/3, aes(group = ss)) +
  geom_path(position = pd, alpha = 1/3, aes(group = ss)) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# longitudinal plot 2
ggplot(pelvic_pain_data_pca_ss_z_wide, aes(year, pelvic_pain)) +
  geom_point(position = pd, alpha = 1/3, aes(group = ss)) +
  geom_line(
    stat ="smooth", 
    method = "lm", 
    formula = y ~ x,
    alpha = 1/3,
    aes(group = ss)
    ) +
  coord_cartesian(ylim = c(-5, 15))

#############################
#                           #
# modeling with pelvic_pain #
#                           #
#############################

# factor scores for the rows (subjects)
fi <- 
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss))

fi_pp_baseline <-
  left_join(fi, pelvic_pain_data_pca_ss_z_wide %>% filter(year == 0), by = "ss") %>%
  select(ss, year, timestamp, days_from_baseline, pelvic_pain, V1:V40)

# Q1 MODEL
q1_mod <- lm(pelvic_pain ~ 1 + V1 + V2 + V3, data = fi_pp_baseline)
summary(q1_mod)     # regression results
performance(q1_mod) # looks at performance metrics
check_model(q1_mod) # checks assumptions

# QUESTION 2: which PC best predicts longitudinal change?
fi_pp_longitudinal <- 
  pelvic_pain_data_pca_ss_z_wide %>% 
  group_by(ss) %>% 
  mutate(timepoints = n()) %>% # computes time points
  ungroup() %>%
  left_join(., fi, by = "ss")

# Mixed-effects modeling
library(lme4)
library(broomExtra)
library(lmerTest)

# Data for lmer - includes participants with at least 2 timepoints for slopes
lmer_data <- fi_pp_longitudinal %>% filter(timepoints > 1) # greater than 1 tp
length(unique(lmer_data$ss)) # n=171, 29 lost to follow-up

# Maximal model - checking to see if random intercepts and slopes converges
max_mod <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 + year | ss), 
    data = lmer_data, 
    REML = TRUE
    )
summary(max_mod)
performance(max_mod)
check_model(max_mod)

min_mod <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(min_mod)
performance(min_mod)
check_model(min_mod)

anova(min_mod, max_mod) # best to model random slopes

# Full model - includes additional fixed effects
full_mod <- 
  lmer(
    pelvic_pain ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(full_mod)
performance(full_mod)
check_model(full_mod)

# Visualizing results
full_mod_aug <- augment(full_mod)

ggplot(full_mod_aug %>% filter(ss < 94), aes(year, pelvic_pain , group = ss)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_line(aes(y = .fitted), linetype = 2) +
  facet_wrap(~ss)

#####################################################
#                                                   #
# computing new z-scores based on baseline m and sd #
#                                                   #
#####################################################

# re-computing the z-scores
year_0 <- pelvic_pain_data_pca_ss %>% filter(complete.cases(.), year == 0)

# Mean & SD at Year 0 for urination pain
m_urination <- mean(year_0$urination_pain_last_week)
sd_urination <- sd(year_0$urination_pain_last_week)
# Mean & SD at Year 0 for bowel mov pain
m_bowel <- mean(year_0$bowel_mov_pain_last_week)
sd_bowel <- sd(year_0$bowel_mov_pain_last_week)
# Mean & SD at Year 0 for mens and nonmens pain
m_mens_nonmens <- mean(year_0$mens_nonmens_pain_week)
sd_mens_nonmens <- sd(year_0$mens_nonmens_pain_week)

# computes new z scores, which scales the other measures by m and sd of BASELINE
# the combined z score is "pelvic_pain"
pelvic_pain_data_pca_ss_new_z <- 
  pelvic_pain_data_pca_ss %>% 
  filter(complete.cases(.)) %>%
  mutate(
    z_urination_pain_last_week = (urination_pain_last_week - m_urination)/sd_urination,
    z_bowel_mov_pain_last_week = (bowel_mov_pain_last_week - m_bowel)/sd_bowel,
    z_mens_nonmens_pain_week = (mens_nonmens_pain_week - m_mens_nonmens)/sd_mens_nonmens,
    pelvic_pain = z_urination_pain_last_week+z_bowel_mov_pain_last_week+z_mens_nonmens_pain_week
  )

# histogram
ggplot(pelvic_pain_data_pca_ss_new_z, aes(pelvic_pain)) +
  geom_histogram(binwidth = .5) +
  facet_wrap(~year)

# density plot
ggplot(
  pelvic_pain_data_pca_ss_new_z, 
  aes(pelvic_pain, group = year, color = factor(year))
) +
  geom_density()

# longitudinal plot 1
ggplot(pelvic_pain_data_pca_ss_new_z, aes(year, pelvic_pain)) +
  geom_point(position = pd, alpha = 1/3, aes(group = ss)) +
  geom_path(position = pd, alpha = 1/3, aes(group = ss)) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

# longitudinal plot 2
ggplot(pelvic_pain_data_pca_ss_new_z, aes(year, pelvic_pain)) +
  geom_point(position = pd, alpha = 1/3, aes(group = ss)) +
  geom_line(
    stat ="smooth", 
    method = "lm", 
    formula = y ~ x,
    alpha = 1/3,
    aes(group = ss)
  ) +
  coord_cartesian(ylim = c(-5, 15))

# new data with corrected z
fi_pp_newz <- 
  pelvic_pain_data_pca_ss_new_z %>% 
  left_join(., fi, by = "ss") %>%
  group_by(ss) %>%
  mutate(timepoints = n()) %>% # calculates timepoints
  ungroup()

# Q1 MODEL
q1_mod_newz <- 
  lm(pelvic_pain ~ 1 + V1 + V2 + V3, data = fi_pp_newz %>% filter(year == 0))
summary(q1_mod_newz)     # regression results
summary(q1_mod) # is exactly the same as above, which is expected
performance(q1_mod_newz) # looks at performance metrics
check_model(q1_mod_newz) # checks assumptions

# Data for lmer - includes participants with at least 2 timepoints for slopes
lmer_data_newz <- fi_pp_newz %>% filter(timepoints > 1) # greater than 1 tp
length(unique(lmer_data_newz$ss)) # n=171, 29 lost to follow-up

# Maximal model - checking to see if random intercepts and slopes converges
max_mod_newz <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 + year | ss), 
    data = lmer_data_newz, 
    REML = TRUE
  )
summary(max_mod)
performance(max_mod)
check_model(max_mod)

min_mod_newz <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 | ss), 
    data = lmer_data_newz, 
    REML = TRUE
  )
summary(min_mod_newz)
performance(min_mod_newz)
check_model(min_mod_newz)

anova(min_mod_newz, max_mod_newz) # best to model random slopes

# Full model - includes additional fixed effects
full_mod_newz <- 
  lmer(
    pelvic_pain ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
    data = lmer_data_newz, 
    REML = FALSE
  )
summary(full_mod_newz) # summary 
performance(full_mod_newz) # model performance
check_model(full_mod_newz) # checks assumptions
anova(max_mod_newz, full_mod_newz) # compares against model without moderators

# plots the observed and fitted slopes for each subject
full_mod_newz_aug <- augment(full_mod_newz)
min(full_mod_newz_aug$pelvic_pain)
max(full_mod_newz_aug$pelvic_pain)
these_ss <- unique(full_mod_newz_aug$ss)

# my own function to plot fitted and observed slopes
my_cowplot <-
  function(x = 1:25){
  ggplot(full_mod_newz_aug %>% filter(ss %in% these_ss[x]), aes(year, pelvic_pain)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    geom_line(aes(y = .fitted), linetype = 3, size = 1, color = rdgy_pal[3]) +
    facet_wrap(~ss) +
    coord_cartesian(ylim = c(-2, 11)) +
    theme_minimal()  
  }

my_cowplot(1:25)
my_cowplot(26:50)
my_cowplot(51:75)
my_cowplot(76:100)
my_cowplot(101:125)
my_cowplot(126:150)
my_cowplot(151:171)


# V1
# simple slopes
v1_simp_slopes <- 
  interact_plot(
    full_mod_newz, 
    pred = year, 
    modx = V1, 
    plot.points = TRUE, 
    interval = TRUE
  )
v1_simp_slopes

# my attempt
pj <- position_jitter(width = .2, height = 0)
ggplot(v1_simp_slopes$data, aes(year, pelvic_pain, group = modx_group)) +
  geom_point(data = full_mod_newz_aug, aes(group = 1), alpha = 1/3, position = pj) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 1/3) +
  geom_line(aes(linetype = modx_group)) +
  scale_linetype_manual(values = c(1,2,1)) +
  theme_classic() +
  theme(legend.position = "bottom")
  
# Johnson-Neyman plot
sim_slopes(full_mod_newz, pred = year, modx = V1, jnplot = TRUE)

# V3 
interact_plot(full_mod_newz, pred = year, modx = V3, plot.points = TRUE, interval = TRUE)
sim_slopes(full_mod_newz, pred = year, modx = V3, jnplot = TRUE)


# TRYING LESS TIME POINTS
# Data for lmer
lmer_data_newz_2year <- 
  fi_pp_newz %>% 
  filter(year %in% c(0:2)) %>% # years 0 1 2 (up to 2 year follow-up)
  group_by(ss) %>%
  mutate(timepoints = n()) %>% # recalculates timepoints
  ungroup()
lmer_data_newz_2year_incl <- lmer_data_newz_2year %>% filter(timepoints>1)
length(unique(lmer_data_newz_2year_incl$ss)) 

# Maximal model - checking to see if random intercepts and slopes converges
max_mod_newz_2year <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 + year | ss), 
    data = lmer_data_newz_2year_incl, 
    REML = TRUE
  )
summary(max_mod_newz_2year)
performance(max_mod_newz_2year)
check_model(max_mod_newz_2year)

min_mod_newz_2year <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 | ss), 
    data = lmer_data_newz_2year, 
    REML = TRUE
  )
summary(min_mod_newz_2year)
performance(min_mod_newz_2year)
check_model(min_mod_newz_2year)

anova(min_mod_newz, max_mod_newz) # best to model random slopes

# Full model - includes additional fixed effects
full_mod_newz_2year <- 
  lmer(
    pelvic_pain ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
    data = lmer_data_newz_2year_incl, 
    REML = FALSE
  )
summary(full_mod_newz_2year) # summary 
performance(full_mod_newz_2year) # model performance
check_model(full_mod_newz_2year) # checks assumptions
anova(max_mod_newz, full_mod_newz) # compares against model without moderators

# Modeling whether QST provides any insight to pelvic pain development over and 
# above standard measures

one_year_follow <-
  fi_pp_newz %>%
  filter(year %in% c(0,1)) %>% # switch this to 2 for 2 year outcome  
  group_by(ss) %>%
  mutate(timepoints = n()) %>%
  ungroup() %>%
  filter(timepoints>1) %>%
  select(ss, year, pelvic_pain, timepoints, V1, V2, V3)

one_year_follow_wide <- 
  one_year_follow %>%
  pivot_wider(
    id_cols = ss, 
    values_from = pelvic_pain, 
    names_from = year, 
    names_prefix = "pp_"
    ) %>%
  left_join(., fi, by = "ss") %>%
  filter(complete.cases(.))

one_year_mod <- lm(pp_1 ~ 1 + pp_0 + V1 + V2 + V3, data = one_year_follow_wide)
summary(one_year_mod)

