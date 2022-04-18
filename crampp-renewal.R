# CRAMPP Renewal Figures
# Matt Kmiecik
# Started 13 APRIL 2022

# Purpose: organize and plot longitudinal data from CRAMPP for grant renewal

# Prepares workspace

source("r-prep.R") # Prepares R workspace

# Loading data
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results
load("../output/ss-codes.RData") # subject codes

# Prepares the annual data
annual_data <-
  left_join(
    complete_extra_annual_data, 
    ss_codes %>% select(ss, group), 
    by = "ss"
  ) %>%
  relocate(group, .after = year) %>%
  filter(group %in% c("HC", "DYS", "DYSB")) # only includes groups of interest

########
#      #
# ICSI #
#      #
########

# Spagetti plot
ggplot(annual_data, aes(year, icsi, group = ss, color = group)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# Modeling ICSI slope for positive/negative trends
icsi_mods <-
  annual_data %>%
  nest_by(ss) %>%
  mutate(icsi_mod = list(lm(icsi ~ 1 + year, data = data)))

# Extracts estimates here
icsi_ests <- 
  icsi_mods %>%
  summarise(broom::tidy(icsi_mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("[[:punct:]]", "", term),
    dir = ifelse(estimate<0, "neg", "pos")
    )

# Identifies who went up or down in ICSI for joining
icsi_up_down <- icsi_ests %>% filter(term == "year", complete.cases(estimate))

# Joins the above info with the main data frame
icsi_data_ss <-
  annual_data %>% 
  select(ss, year, group, icsi) %>%
  left_join(., icsi_up_down, by = "ss")

# Plots slopes
pj <- position_jitter(width = .3, height = .3) 
ggplot(
  icsi_data_ss %>% filter(complete.cases(dir)), 
  aes(year, icsi, group = dir, color = dir)
  ) +
  geom_point(alpha = .3, position = pj) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year", y = "ICSI") +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 16, 2), minor_breaks = NULL) +
  theme_bw() +
  facet_wrap(~group)

# Computes summary statistics
icsi_data_sum <- 
  icsi_data_ss %>%
  filter(complete.cases(icsi, dir)) %>%
  group_by(year, group, dir) %>%
  summarise(
    m = mean(icsi),
    sd = sd(icsi),
    n = n(),
    sem = sd/sqrt(n)
  )

# Plots summary statistics
ggplot(icsi_data_sum, aes(year, m, color = dir)) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .2) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 16, 2), minor_breaks = NULL) +
  labs(x = "Year", y = "Mean ICSI", caption = "SEM error bars.") +
  theme_bw() +
  facet_wrap(~group)


###################
#                 #
# Pelvic Pain VAS #
#                 #
###################

pp_data_wide <-
  annual_data %>%
  select(
    ss, 
    year, 
    group, 
    urination_pain_last_week, 
    bowel_mov_pain_last_week, 
    mens_nonmens_pain_week
    ) %>%
  rowwise() %>%
  mutate(
    pelvic_pain = mean(
      c(urination_pain_last_week, bowel_mov_pain_last_week, mens_nonmens_pain_week)
      )
    ) %>%
  ungroup()

# Model these data using regression for each participant
pp_mods <- 
  pp_data_wide %>%
  nest_by(ss) %>%
  mutate(
    pp_mod = list(lm(pelvic_pain ~ 1 + year, data = data)),
    ur_mod = list(lm(urination_pain_last_week ~ 1 + year, data = data)),
    bowel_mod = list(lm(bowel_mov_pain_last_week ~ 1 + year, data = data)),
    mens_nonmens_mod = list(lm(mens_nonmens_pain_week ~ 1 + year, data = data))
      )
# remember that you have more models to play with above ^^^

# Extracts model estimates
pp_ests <- 
  pp_mods %>%
  summarise(broom::tidy(pp_mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("[[:punct:]]", "", term),
    dir = ifelse(estimate<0, "neg", "pos")
  )

# Identifies who went up or down in pelvic pain for joining
pp_up_down <- pp_ests %>% filter(term == "year", complete.cases(estimate))

# Joins the above info with the main data frame
pp_data_ss <-
  pp_data_wide %>% 
  select(ss, year, group, pelvic_pain) %>%
  left_join(., pp_up_down, by = "ss")

# Computes summary statistics
pp_data_sum <- 
  pp_data_ss %>%
  filter(complete.cases(pelvic_pain, dir)) %>%
  group_by(year, group, dir) %>%
  summarise(
    m = mean(pelvic_pain),
    sd = sd(pelvic_pain),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# Plots summary statistics
ggplot(pp_data_sum, aes(year, m, color = dir)) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .2) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(breaks = seq(0, 50, 10), minor_breaks = NULL) +
  labs(x = "Year", y = "Mean Pelvic Pain (0-100 VAS)", caption = "SEM error bars.") +
  theme_bw() +
  facet_wrap(~group)

# Looking at pelvic pain VAS without directionality
pp_data_group_ss <-
  pp_data_wide %>%
  filter(complete.cases(pelvic_pain)) %>%
  group_by(year, group) %>%
  summarise(
    m = mean(pelvic_pain),
    sd = sd(pelvic_pain),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# Plots summary statistics
ggplot(pp_data_group_ss, aes(year, m, group = group, color = group)) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .1) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(breaks = seq(0, 50, 10), minor_breaks = NULL) +
  scale_color_jco() +
  labs(x = "Year", y = "Mean Pelvic Pain (0-100 VAS)", caption = "SEM error bars.") +
  theme_bw()

# Looking at individual pelvic pain metrics
# converts to long format
pp_data_long <- pp_data_wide %>% pivot_longer(c(-ss, -year, -group))

# computes summary stats in long format
pp_data_all_sum <- 
  pp_data_long %>%
  filter(complete.cases(value)) %>%
  group_by(year, group, name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# plots pelvic pain individual components
ggplot(
  pp_data_all_sum %>% filter(name %nin% "pelvic_pain"), 
  aes(year, m, group = group, color = group)
  ) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .1) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_y_continuous(breaks = seq(0, 50, 10), minor_breaks = NULL) +
  scale_color_jco() +
  labs(x = "Year", y = "Mean Pelvic Pain (0-100 VAS)", caption = "SEM error bars.") +
  theme_bw() +
  facet_wrap(~name)

# Looking at all variables
annual_data_long <- 
  annual_data %>%
  select(
    ss,
    group,
    year, 
    cramp_pain_no_nsaid, 
    bsi, 
    urination_pain_last_week, 
    bowel_mov_pain_last_week, 
    mens_nonmens_pain_week,
    icsi,
    avg_pain,
    global_health
    ) %>%
  pivot_longer(c(-ss, -group, -year))

annual_data_sum <-
  annual_data_long %>%
  filter(complete.cases(value)) %>%
  group_by(year, group, name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# plots all vars
ggplot(
  annual_data_sum, 
  aes(year, m, group = group, color = group)
) +
  geom_path() +
  geom_point() +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .1) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  #coord_cartesian(ylim = c(0, 50)) +
  #scale_y_continuous(breaks = seq(0, 50, 10), minor_breaks = NULL) +
  scale_color_jco() +
  labs(x = "Year", y = "Measure Dependent Value", caption = "SEM error bars.") +
  theme_bw() +
  facet_wrap(~name, scales = "free")
  
  














