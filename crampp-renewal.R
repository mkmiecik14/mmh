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

# Spaghetti plot
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
  ungroup() %>%
  filter(year < 3) # comment out if you want all the years

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


# This function extracts model estimates from a df with cols as mods
extract_ests <- 
  function(mods = x, model = y){
    ests <- 
      mods %>%
      summarise(broom::tidy({{model}})) %>%
      ungroup() %>%
      mutate(
        term = gsub("[[:punct:]]", "", term),
        dir = ifelse(estimate<0, "neg", "pos")
      )
    return(ests)
  }

# Extracts model estimates
pp_ests <- extract_ests(mods = pp_mods, model = pp_mod) 
ur_ests <- extract_ests(mods = pp_mods, model = ur_mod) 
bowel_ests <- extract_ests(mods = pp_mods, model = bowel_mod)  
mens_nonmens_ests <- extract_ests(mods = pp_mods, model = mens_nonmens_mod)  

# Identifies who went up or down in pelvic pain for joining
pp_up_down <- pp_ests %>% filter(term == "year", complete.cases(estimate))
ur_up_down <- ur_ests %>% filter(term == "year", complete.cases(estimate))
bowel_up_down <- bowel_ests %>% filter(term == "year", complete.cases(estimate))
mens_nonmens_up_down <- 
  mens_nonmens_ests %>% filter(term == "year", complete.cases(estimate))

# Joins the above info with the main data frame
pp_data_ss <-
  pp_data_wide %>% 
  select(ss, year, group, pelvic_pain) %>%
  left_join(., pp_up_down, by = "ss") %>%
  mutate(meas = pelvic_pain, data = "pelvic_pain") %>%
  select(-pelvic_pain)

ur_data_ss <-
  pp_data_wide %>%
  select(ss, year, group, urination_pain_last_week) %>%
  left_join(., ur_up_down, by = "ss") %>%
  mutate(meas = urination_pain_last_week, data = "ur_pain") %>%
  select(-urination_pain_last_week)

bowel_data_ss <-
  pp_data_wide %>%
  select(ss, year, group, bowel_mov_pain_last_week) %>%
  left_join(., bowel_up_down, by = "ss") %>%
  mutate(meas = bowel_mov_pain_last_week, data = "bowel_pain") %>%
  select(-bowel_mov_pain_last_week)

mens_nonmens_data_ss <-
  pp_data_wide %>%
  select(ss, year, group, mens_nonmens_pain_week) %>%
  left_join(., mens_nonmens_up_down, by = "ss") %>%
  mutate(meas = mens_nonmens_pain_week, data = "mens_nonmens_pain") %>%
  select(-mens_nonmens_pain_week)

# Combines all data into one df
data_ss <- 
  bind_rows(pp_data_ss, ur_data_ss, bowel_data_ss, mens_nonmens_data_ss) %>%
  mutate(
    data = fct_relevel(
      data, 
      c("ur_pain", "bowel_pain", "mens_nonmens_pain", "pelvic_pain")
      )
    )
  

# Computes summary statistics
data_sum <- 
  data_ss %>%
  filter(complete.cases(meas, dir)) %>%
  group_by(year, group, dir, data) %>%
  summarise(
    m = mean(meas),
    sd = sd(meas),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup()

# Plots summary statistics
pd <- position_dodge(width = .5)
ggplot(data_sum, aes(year, m, color = dir)) +
  geom_path(position = pd) +
  geom_point(aes(size = n), position = pd) +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .2, position = pd) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 70)) +
  scale_y_continuous(breaks = seq(0, 70, 10), minor_breaks = NULL) +
  labs(x = "Year", y = "Mean Pain (0-100 VAS)", caption = "SEM error bars.") +
  theme_bw() +
  facet_grid(data~group)

# Focused plot of above
pd <- position_dodge(width = .2)
dysb_worse_plot <- 
  ggplot(
  data_sum %>% filter(dir == "pos", data == "pelvic_pain"), 
  aes(year, m, color = group)
  ) +
  geom_path(position = pd) +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .2, position = pd) +
  geom_point(position = pd) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  #geom_text_repel(aes(label = paste0("n = ", n)), position = pd) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_y_continuous(breaks = seq(0, 40, 10), minor_breaks = NULL, expand = c(0, 0)) +
  labs(x = "Year", y = "Pelvic Pain (0-100 VAS)", caption = "SEM error bars.") +
  theme_bw() +
  scale_color_jco() +
  theme(legend.position = "bottom")
dysb_worse_plot

# uncomment out to save
# ggsave(
#   filename = "../output/dysb-worse-plot.svg",
#   plot = dysb_worse_plot,
#   width = 4.5,
#   height = 4,
#   units = "in"
# )

# table of sample sizes
data_sum %>% 
  select(year, group, dir, data, n) %>%
  split(interaction(.$year, .$dir)) %>%
  map(~pivot_wider(.x, id_cols = data, names_from = group, values_from = n))

# sample sizes of improving/worsening when collapsed across patient group
# for join below
total_sample_sizes <- 
  data_sum %>%
  group_by(year, group, data) %>%
  summarise(sample = sum(n))

total_sample_sizes_wide <-
  total_sample_sizes %>%
  pivot_wider(
    id_cols = c(year, data), 
    names_from = group, 
    values_from = sample,
    names_prefix = "total_"
    )

up_down_n <- 
  data_sum %>%
  pivot_wider(
    id_cols = c(year, dir, data), 
    names_from = group, 
    values_from = n
    ) %>%
  left_join(., total_sample_sizes_wide, by = c("year", "data")) %>%
  mutate(
    perc_DYS = DYS/total_DYS,
    perc_DYSB = DYSB/total_DYSB,
    perc_HC = HC/total_HC
    )



up_down_n %>%
  filter(data == "pelvic_pain") %>%
  group_by(year) %>%
  summarise(DYSB = sum(DYSB))
   

# Computes summary statistics a different way
data_sum_2 <- 
  data_ss %>%
  filter(complete.cases(meas, dir)) %>%
  group_by(year, dir, data) %>%
  summarise(
    m = mean(meas),
    sd = sd(meas),
    n = n(),
    sem = sd/sqrt(n)
  ) %>%
  ungroup() %>%
  left_join(., up_down_n, by = c("year", "dir", "data"))

# Plots summary statistics
pd <- position_dodge(width = .5)
ggplot(data_sum_2, aes(year, m, color = dir)) +
  geom_path(position = pd) +
  geom_point(aes(size = n), position = pd) +
  geom_errorbar(aes(ymin=m-sem, ymax = m+sem), width = .2, position = pd) +
  geom_text_repel(aes(label = paste0(round(perc_HC*100), "%"))) +
  scale_x_continuous(breaks = 0:5, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 70)) +
  scale_y_continuous(breaks = seq(0, 70, 10), minor_breaks = NULL) +
  labs(
    x = "Year", 
    y = "Mean Pain (0-100 VAS)", 
    caption = "SEM error bars. \n Numbers indicate % DYSB."
    ) +
  theme_bw() +
  facet_grid(~data)


# use this as a join to calc percentage above





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
  