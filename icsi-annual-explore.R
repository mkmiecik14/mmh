# ICSI Annuals Exploration and Modeling Script
# Matt Kmiecik
# 28 OCTOBER 2021

# Purpose: to visualize and inspect the annual ICSI data and model trajectories

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/complete-icsi-data.Rdata")

# Exploration ----

# Computing basic summary stats
icsi_sum <- 
  complete_icsi_data %>%
  group_by(year) %>%
  summarise(
    m = mean(days_from_baseline, na.rm = TRUE),
    med = median(days_from_baseline, na.rm = TRUE),
    min = min(days_from_baseline, na.rm = TRUE),
    max = max(days_from_baseline, na.rm = TRUE),
    n = n(),
    sd = sd(days_from_baseline, na.rm = TRUE),
    sem = sd/sqrt(n)
    ) %>%
  ungroup()
# plot
pj <- position_jitter(width = .1, height = .1)
pn <- position_nudge(x = .2, y = 0)
ggplot(icsi_sum, aes(year, m)) +
  geom_point(
    data = complete_icsi_data, 
    aes(y = days_from_baseline), 
    alpha = 1/3,
    position = pj
    ) +
  geom_bar(stat = "identity", fill = "white", color = "black", width = 1/2, position = pn, alpha = 1/3) +
  geom_errorbar(aes(ymin = m-sd, ymax = m+sd, width = .4)) +
  theme_classic()
# boxplot
ggplot(complete_icsi_data, aes(year, days_from_baseline, group = year)) +
  geom_point(
    alpha = 1/3,
    position = pj
  ) +
  geom_boxplot(position = pn, width = .2) 


# might want to take a look at participants with high values for annual 1
greater_than_3_months <-
  complete_icsi_data %>% 
  filter(year == 1, days_from_baseline > 365+(3*30)) # greater than 3 months 
greater_than_3_months_ss <- 
  complete_icsi_data %>% 
  filter(ss %in% greater_than_3_months$ss)
# What was determined from examining the df above:
# participants 78, 82, 90, 158, 244 were sent year 1 annual at year 2 and then 
# sent another one quickly after (< 4 months later); rather than deleting this 
# extra survey, it was kept for more data points
# therefore, analysis should proceed with days since baseline, NOT YEAR

#########
#       #
# PLOTS #
#       #
#########

timepoints_ss <- complete_icsi_data %>% count(ss)
complete_icsi_data_n <- complete_icsi_data %>% left_join(., timepoints_ss, by = "ss")


pd <- position_dodge(width = .1)
ggplot(complete_icsi_data_n, aes(timestamp, icsi, group = ss, color = factor(n))) +
  #geom_point(position = pd) +
  geom_line(position = pd) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Time", y = "ICSI Score") +
  theme_classic()

ggplot(complete_icsi_data_n, aes(days_from_baseline, icsi, group = ss, color = factor(n))) +
  geom_line(position = pd) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Days since baseline", y = "ICSI Score") +
  theme_classic()

ggplot(complete_icsi_data_n, aes(days_from_baseline, icsi, group = ss)) +
  geom_smooth(method = "lm", se = FALSE, size = 1/2, color = "grey") +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "red") +
  labs(x = "Days since baseline", y = "ICSI Score") +
  theme_classic()

ggplot(complete_icsi_data_n, aes(days_from_baseline, icsi)) +
  geom_point(alpha = 1/3, position = position_jitter(width = .1, height = .2)) +
  #geom_smooth(method = "lm", se = FALSE, size = 1/2, color = "grey") +
  #geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "red") +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  stat_smooth(
    aes(group = ss), 
    method = "lm", 
    formula = y ~ x + I(x^2), 
    size = 1/2, 
    se = FALSE, 
    color = "black", 
    alpha = 1/3
    ) +
  labs(x = "Days since baseline", y = "ICSI Score") +
  theme_classic()

###################
#                 #
# Linear Modeling #
#                 #
###################

# Modeling individuals with at least 2 time points:
# data:
lvl1_data <- complete_icsi_data_n %>% filter(n > 1) # at least 2 time points

# mods
lvl1_mods <- 
  lvl1_data %>% 
  nest_by(ss) %>%
  mutate(
    mod = list(lm(icsi ~ 1 + days_from_baseline, data = data)),
    mod_mc = list(lm(icsi ~ 1 + scale(days_from_baseline, scale = FALSE), data = data))
    )

# estimates
lvl1_ests <- 
  lvl1_mods %>%
  summarise(broom::tidy(mod)) %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scale(days_from_baseline, scale = FALSE)", "days_mc", term)
  ) %>%
  ungroup()

# visualizing intercepts (average ICSI score)
ggplot(lvl1_ests %>% filter(term == "Intercept"), aes(estimate)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Intercept (mean ICSI score)", y = "Frequency") +
  theme_minimal()

# visualizing slopes
ggplot(lvl1_ests %>% filter(term == "days_from_baseline"), aes(estimate)) +
  geom_histogram(binwidth = .001) +
  labs(x = "Slope", y = "Frequency") +
  theme_minimal()

# taking a look at these "outlier" slopes
# gathers summary stats
lvl1_ests %>% 
  group_by(term) %>%
  summarise(m = mean(estimate), n = n(), sd = sd(estimate), sem = sd/sqrt(n)) %>%
  ungroup()

# These are "outlier" slopes
big_slopes <-
  lvl1_ests %>% 
  filter(term == "days_from_baseline", abs(estimate) > .000559 + 2*.00335) # > 2SD
# subjects with "outlier slopes"
big_slopes_ss <- complete_icsi_data_n %>% filter(ss %in% big_slopes$ss)

# these look fine to me!
ggplot(big_slopes_ss, aes(days_from_baseline, icsi, group = ss)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ss)

#################
#               #
# MAIN ANALYSES #
#               #
#################

# QUESTION 1: Which PC best predicts ICSI at baseline?
# ICSI ~ PC1 + PC2 + PC3
load("../output/mmh-res.RData") # loads PCA results

# factor scores for the rows (subjects)
fi <- 
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss))

fi_icsi_baseline <-
  left_join(fi, complete_icsi_data_n %>% filter(year == 0), by = "ss") %>%
  select(ss, year, timestamp, icsi, V1:V40)

# Q1 MODEL
q1_mod <- lm(icsi ~ 1 + V1 + V2 + V3, data = fi_icsi_baseline)
summary(q1_mod)     # regression results
performance(q1_mod) # looks at performance metrics
check_model(q1_mod) # checks assumptions


# QUESTION 2: which PC best predicts longitudinal change?
# ICSI average (intercept) ~ PC1 + PC2 + PC3
# ICSI slope (days_mc) ~ PC1 + PC2 + PC3
fi_icsi_longit <- 
  left_join(fi, lvl1_ests %>% select(ss, term, estimate), by = "ss") %>%
  select(ss, estimate, term, V1:V40) %>%
  filter(complete.cases(.)) # filters out participants w/o longitudinal data
length(unique(fi_icsi_longit$ss)) # total sample size is 171 (lost 29 ss to follow-up)

# models level 2
lvl2_mod <-
  fi_icsi_longit %>% 
  nest_by(term) %>%
  mutate(
    mod1 = list(lm(estimate ~ 1 + V1 + V2 + V3, data = data)),
    mod2 = list(lm(estimate ~ 1, data = data))
    )

# full mlm
lvl2_est <- 
  lvl2_mod %>% 
  summarise(broom::tidy(mod1, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup() %>%
  mutate(source = rep(lvl2_mod$term, each = 4))

# just intercepts
lvl2_est_int <- 
  lvl2_mod %>% 
  summarise(broom::tidy(mod2, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup() %>%
  mutate(source = rep(lvl2_mod$term, each = 1))


# Mixed-effects modeling
library(lme4)
library(broomExtra)
library(languageR)
library(lmerTest)
# https://featuredcontent.psychonomic.org/putting-ps-into-lmer-mixed-model-regression-and-statistical-significance/
# lmer(Reaction ~ 1 + Days + (1 + Days | Subject), sleepstudy)

mem_data <- 
  complete_icsi_data_n %>% # HERE YOU CAN CHOOSE TO FILTER DOWN TO THOSE SUBJECTS WITH > 1 TIMEPOINT
  left_join(., fi, by = "ss") %>%
  filter(complete.cases(.))
length(unique(mem_data$ss))

maximal_mod <- lmer(icsi ~ 1 + days_from_baseline + (1 + days_from_baseline | ss), data = mem_data, REML = TRUE) # look up REML
summary(maximal_mod)
rand_int_mod <- lmer(icsi ~ 1 + days_from_baseline + (1 | ss), data = mem_data, REML = TRUE) # look up REML
summary(rand_int_mod)
rand_slope_mod <- lmer(icsi ~ 1 + days_from_baseline + (0 + days_from_baseline | ss), data = mem_data, REML = TRUE)
summary(rand_slope_mod)

test <- mem_data %>% mutate(days_from_baseline = days_from_baseline/365)
maximal_mod_test <- lmer(icsi ~ 1 + days_from_baseline + (1 + days_from_baseline | ss), data = test, REML = TRUE) # look up REML
summary(maximal_mod)
rand_int_mod_test <- lmer(icsi ~ 1 + days_from_baseline + (1 | ss), data = test, REML = TRUE) # look up REML
summary(rand_int_mod_test)
rand_slope_mod <- lmer(icsi ~ 1 + days_from_baseline + (0 + days_from_baseline | ss), data = mem_data, REML = TRUE)
summary(rand_slope_mod)

anova(maximal_mod, rand_int_mod)

rand_int_mod_aug <- rand_int_mod_test %>% augment()

ggplot(rand_int_mod_aug %>% filter(ss < 50), aes(days_from_baseline, icsi)) +
  geom_line(aes(y = .fitted), color = "blue") + 
  geom_point() +
  facet_wrap(~ss, ncol = 5)

ests_wide <- fi_icsi_longit %>% pivot_wider(id_cols = ss, values_from = estimate, names_from = term)
cor.test(ests_wide$Intercept, ests_wide$days_from_baseline)
sd(ests_wide$Intercept)
sd(ests_wide$days_from_baseline)
ggplot(ests_wide, aes(Intercept, days_from_baseline)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

summary(lmer(icsi ~ 1 + days_from_baseline + (1 + days_from_baseline | ss), data = mem_data %>% filter(n > 5), REML = TRUE)) # look up REML


# running simple model first
mod1 <- lmer(icsi ~ 1 + days_from_baseline + (1 + days_from_baseline | ss), data = lvl1_data, REML = TRUE) # look up REML
summary(mod1)
#write_csv(lvl1_data, file = "for-ed.csv")

lvl1_data_mc %>% group_by(year) %>% summarise(m = mean(icsi), n= n())

mod2 <- lmer(icsi ~ 1 + days_from_baseline + (1 | ss) + (0+days_from_baseline|ss), data = lvl1_data_mc)
summary(mod2)

anova(mod2, mod1)

lmer(icsi ~ 1 + days_from_baseline + (1 | ss), data = lvl1_data)

head(lme4::ranef(mod1))
test <- head(ranef(mod1))
cor(as.matrix(test$ss))

ggplot(lvl1_data %>% filter(ss < 90), aes(days_from_baseline, icsi)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ss)

## DIRECTLY COMPAIRING THE SAME PARTICIPANTS!
# Data for mixed effects modeling
lvl1_data_fi <- 
  lvl1_data %>%
  group_by(ss) %>%
  mutate(days_mc = as.numeric(scale(days_from_baseline, scale = FALSE))) %>%
  ungroup() %>%
  left_join(., fi, by = "ss") %>% 
  filter(complete.cases(.))

length(unique(lvl1_data_fi$ss)) #n=171

mod1 <- lmer(icsi ~ 1 + days_from_baseline + (1 + days_from_baseline | ss), data = lvl1_data_fi, REML = FALSE)
summary(mod1)
ranef(mod1)

lvl1_new_mods <- 
  lvl1_data_fi %>% 
  nest_by(ss) %>%
  mutate(
    mod = list(lm(icsi ~ 1 + days_from_baseline, data = data))
  )

lvl1_new_ests <- 
  lvl1_new_mods %>%
  summarise(broom::tidy(mod)) %>%
  mutate(
    term = gsub("[\\(\\)]", "", term)
  ) %>%
  ungroup()

sing <- lvl1_new_ests %>% filter(is.na(std.error))


ggplot(lvl1_data %>% filter(ss %in% sing$ss), aes(days_from_baseline, icsi)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ss)


sing2 <- lvl1_data %>% filter(n == 2)


lvl2_new_mod <-
  lvl1_new_ests %>% 
  nest_by(term) %>%
  mutate(mod = list(lm(estimate ~ 1, data = data)))

lvl2_est <- 
  lvl2_new_mod %>% 
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup()

















test_lmer <- lmer(
  icsi ~ 1 + 
    days_from_baseline*V1 + 
    days_from_baseline*V2 + 
    days_from_baseline*V3 + 
    (1 + days_from_baseline | ss), 
  data = lvl1_data_fi
  )
summary(test_lmer)
tidy(test_lmer)


test_lmer <- lmer(icsi ~ 1 + days_mc + (1 + days_mc | ss) + V1 + V2 + V3, data = lvl1_data_fi)
test_lmer <- lmer(icsi ~ 1 + days_mc + V1 + V2 + V3 + (1 + days_mc + V1 + V2 + V3 | ss) , data = lvl1_data_fi)

summary(test_lmer)
summary(test_lmer)
anova(test_lmer)


# try to replicate with lmer




# try also with quadratic fit

