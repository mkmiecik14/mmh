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


