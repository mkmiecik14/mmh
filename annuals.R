# CRAMPP Annual Questionnaires
# Matt Kmiecik
# 26 July 2021

# Purpose: determination fo sample size and estimates needed for grant proposal;
# break into other scripts when ready

source("r-prep.R") # Prepare R workspace

# Loads in data/prepares ----

# Subject codes with shortened annual record ids
crampp_codes <- read_excel(path = "../data/crampp-codes.xlsx", sheet = "usethis") 

# Long annual data from ARM 1
long_annual_data <- read_csv(file = "../data/long-annual-data.csv")

# Year 2 annuals from CRAMPP long survey with ROME and ICSI
long_arm1_data <- 
  long_annual_data %>%
  select(
    record_number, 
    annual = redcap_event_name, 
    timestamp = annual_questionnaire_combined060416_timestamp,
    ssid = a99subject_id,
    contains("rome"),
    a99ic1a,
    a99ic1b,
    a99ic1c,
    a99ic1d
    ) %>%
  mutate(
    annual = case_when(
      annual == "year_1_followup_do_arm_1" ~ 1,
      annual == "year_2_followup_arm_1" ~ 2,
      annual == "year_3_followup_arm_1" ~ 3,
      annual == "year_4_followup_arm_1" ~ 4,
      annual == "year_5_followup_arm_1" ~ 5
    )
    ) %>%
  left_join(., crampp_codes, by = c("record_number" = "arm1r")) %>%
  select(ss, record_number, annual:notes3)


long_arm1_icsi <-
  long_arm1_data %>%
  select(ss, annual, contains("ic")) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(c(-ss, -annual)) %>%
  group_by(ss, annual) %>%
  summarise(icsi = sum(value), n = n()) %>%
  ungroup()

# Assessment visit 3 (some participants did annuals at assessment visit 3)
 


# Shortened annual data
short_annual_data <- read_csv(file = "../data/short-annual-data.csv")

short_data <- 
  short_annual_data %>%
  filter(redcap_event_name %nin% "contact_info_arm_1") %>%
  select(
    record_id, 
    annual = redcap_event_name,
    timestamp = shortened_annual_questionnaire_timestamp,
    contains("rome"),
    contains("ic1")
    ) %>%
  mutate(
    annual = case_when(
      annual == "shortened_annual_1_arm_1" ~ 1,
      annual == "shortened_annual_2_arm_1" ~ 2,
      annual == "shortened_annual_3_arm_1" ~ 3,
      annual == "shortened_annual_4_arm_1" ~ 4,
      annual == "shortened_annual_5_arm_1" ~ 5
    )
  ) %>%
  left_join(., crampp_codes, by = c("record_id" = "shortannualsr")) %>%
  select(ss, record_id, annual:notes3)

#! NOTE: find out why there are filled out annuals with no subject number above
  
short_icsi <-
  short_data %>%
  select(ss, annual, contains("ic"), -a99ic1e_sxtotal_dfe15d) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(c(-ss, -annual)) %>%
  group_by(ss, annual) %>%
  summarise(icsi = sum(value), n = n()) %>%
  ungroup()

# Next step is to get the arm2 people
long_annual_arm2 <- read_csv(file = "../data/long-annual-data-arm2.csv")

long_annual_arm2_data <- 
  long_annual_arm2 %>%
  select(
    record_number, 
    annual = redcap_event_name, 
    timestamp = annual_questionnaire_combined060416_timestamp,
    contains("rome"),
    a99ic1a,
    a99ic1b,
    a99ic1c,
    a99ic1d
  ) %>%
  mutate(
    annual = case_when(
      annual == "annual_1_arm_1" ~ 1,
      annual == "annual_2_arm_1" ~ 2,
      annual == "annual_3_arm_1" ~ 3,
      annual == "annual_4_arm_1" ~ 4,
      annual == "annual_5_arm_1" ~ 5
    )
  ) %>%
  left_join(., crampp_codes, by = c("record_number" = "arm2r")) %>%
  select(ss, record_number, annual:notes3)

# looking at ICSI
long_arm2_icsi <- 
  long_annual_arm2_data %>%
  select(ss, annual, contains("ic")) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(c(-ss, -annual)) %>%
  group_by(ss, annual) %>%
  summarise(icsi = sum(value), n = n()) %>%
  ungroup()

# BASELINE

load("../output/redcap-ic-data.RData") # IC data at baseline

baseline_icsi <- 
  redcap_ic_data %>%
  select(ss, ic1a:ic1d) %>%
  filter(complete.cases(.)) %>%
  pivot_longer(-ss) %>%
  group_by(ss) %>%
  summarise(icsi = sum(value), n = n()) %>%
  ungroup() %>%
  mutate(annual = 0) # signals baseline

# combining data for ICSI
annual_icsi <- bind_rows(baseline_icsi, long_arm1_icsi, long_arm2_icsi, short_icsi)
annual_icsi %>%
  group_by(annual) %>%
  summarise(m = mean(icsi), sd = sd(icsi), n = n(), sem = sd/sqrt(n)) %>%
  ungroup()

ggplot(annual_icsi, aes(annual, icsi, group = ss)) +
  geom_point() +
  geom_line()

# for crampp renewal
groupwise_annual_icsi <- 
  annual_icsi %>%
  left_join(., select(crampp_codes, ss, group), by = "ss") %>%
  group_by(annual, group) %>%
  summarise(m = mean(icsi), sd = sd(icsi), n = n(), sem = sd/sqrt(n)) %>%
  ungroup() %>%
  filter(group %in% c("DYS", "DYSB", "HC"))
print(groupwise_annual_icsi)
#write_csv(groupwise_annual_icsi, "../output/crampp-renewal-icsi.csv")

# subject-wise icsi
annual_icsi_ss <- 
  annual_icsi %>%
  left_join(., select(crampp_codes, ss, group), by = "ss")


# Figure for CRAMPP Renewal
pj <- position_jitter(width = .1)
ggplot(groupwise_annual_icsi, aes(annual, m, group = group, color = group)) +
  geom_hline(yintercept = 7, linetype = 2, color = "lightgrey") +
  geom_point(
    data = annual_icsi_ss %>% filter(group %in% c("DYS", "DYSB", "HC")), 
    aes(x = annual, y = icsi), 
    shape = 1, position = pj, alpha = 1/3
    ) +
  geom_point() +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .2) +
  geom_line() +
  theme_classic() +
  #scale_color_manual(values = wes_palettes$Darjeeling2[c(2,3,5)]) +
  scale_color_jco() +
  labs(x = "Year", y = "Mean ICSI Score", caption = "SEM error bars.") +
  theme(legend.position = "bottom")
# ggsave(filename = "../output/crampp-renewal-icsi.svg")

pd <- position_dodge(width = .1)
ggplot(groupwise_annual_icsi, aes(annual, m, group = group, color = group)) +
  geom_hline(yintercept = 7, linetype = 2, color = "lightgrey") +
  # geom_point(
  #   data = annual_icsi_ss %>% filter(group %in% c("DYS", "DYSB", "HC")), 
  #   aes(x = annual, y = icsi), 
  #   shape = 1, position = pj, alpha = 1/3
  # ) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .2, position = pd) +
  geom_line(position = pd) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 10 ,2)) +
  scale_color_jco() +
  labs(x = "Year", y = "Mean ICSI Score") +
  theme(legend.position = "none")

# uncomment to save out
# ggsave(
#   filename = "../output/crampp-renewal-icsi-v2.svg", 
#   width = 2, 
#   height = 1.5, 
#   units = "in"
#   )

icsi_data <- annual_icsi_ss %>% filter(group %in% c("DYS", "DYSB", "HC"))

length(unique(icsi_data$ss))

  
  

# Now we have to determine subject IDs that have complete EEG and QST
load("../output/mmh-res.RData")

# These participants have complete QST because they made it to the PCA analysis (n=200)
complete_qst <- tibble(ss = as.numeric(rownames(pca_res$Fixed.Data$ExPosition.Data$X)))

# These participants have complete EEG because they were in the SSVEP manuscript (n=147)
complete_eeg <- read_csv("../data/ssvep-subjects.csv") %>% mutate(here = 1)

# COMPLETE DATA FOR NRSA
complete_baseline <- 
  left_join(complete_qst, complete_eeg) %>% 
  filter(complete.cases(here)) %>%
  left_join(., crampp_codes, by = "ss")

icsi_wide <- 
  annual_icsi %>% 
  select(ss, annual, icsi) %>%
  filter(annual %in% 0:2) %>%
  pivot_wider(
    id_cols = ss, 
    names_from = annual, 
    values_from = icsi, 
    values_fill = NA, 
    names_prefix = "annual_"
    )

icsi_sample_sizes <- 
  complete_baseline %>%
  select(ss, group) %>%
  left_join(., icsi_wide, by = "ss")


icsi_sample_sizes %>% filter(complete.cases(annual_0)) %>% count(group)
# n = 106 DYS
# n = 27 HC

icsi_sample_sizes %>% filter(complete.cases(annual_1)) %>% count(group)
# n = 71 DYS
# n = 22 HC

icsi_sample_sizes %>% filter(complete.cases(annual_2)) %>% count(group)
# n = 67 DYS with complete year 2 questionnaires
# n = 20 healthy controls

# Looking at change in ICSI
# The cutoff is 6 (see O'Leary et al., 1997):
# "Almost no IC patients score less than 6 on either index, while almost no controls score as high as 6" (p. 59)
icsi_sample_sizes %>% 
  filter(complete.cases(annual_2)) %>%
  mutate(
    group = case_when(
      group == "HC" ~ "HC",
      group == "DYSB" ~ "DYS",
      group == "DYS" ~ "DYS"
    )
    ) %>%
  mutate(
    annual_0_icsi = annual_0 >= 6, 
    annual_2_icsi = annual_2 >=6
    ) %>%
  count(group, annual_0_icsi, annual_2_icsi)

(4+18)/(32+4+13+18) * 100 # percentage of DYS participants that meet criteria for IC

(1+2)/(17+1+2) * 100 # percentage of HCs that meet criteria

icsi_mat <- cbind(yes = c(18+4, 1+2), no = c(32+13, 17))
rownames(icsi_mat) <- c("DYS", "HC")
fisher.test(icsi_mat)

icsi_plot_data <- 
  icsi_sample_sizes %>% 
  filter(complete.cases(annual_2)) %>%
  mutate(
    group = case_when(
      group == "HC" ~ "HC",
      group == "DYSB" ~ "DYS",
      group == "DYS" ~ "DYS"
    )
  ) 

ggplot(icsi_plot_data, aes(annual_2)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~group)
  
icsi_sample_sizes %>% 
  filter(complete.cases(annual_2)) %>%
  mutate(
    group = case_when(
      group == "HC" ~ "HC",
      group == "DYSB" ~ "DYS",
      group == "DYS" ~ "DYS"
    )
  ) %>%
  select(-annual_1) %>%
  pivot_longer(c(-ss, -group)) %>%
  group_by(group, name) %>%
  summarise(m = mean(value), sd = sd(value), n = n()) %>%
  ungroup()






  