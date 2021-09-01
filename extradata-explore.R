# Extra Data Explore
# Matt Kmiecik
# Started 14 July 2021

# Purpose: This script explores data that was not included in the initial PCA
# analyses, but is interesting to see how the PCA results predict these measures

source("r-prep.R") # Prepare R workspace

# Loads in data ----
load("../output/ss-codes.RData")                  # subject codes
load("../output/redcap-bsi-data.RData")           # BSI
load("../output/redcap-cmsi-data.RData")          # CMSI
load("../output/redcap-menstrual-data.RData")     # menstrual data
load("../output/redcap-gupi-data.RData")          # GUPI
load("../output/redcap-ic-data.RData")            # ICSI + ICPI
load("../output/redcap-rome-data.RData")          # ROME
load("../output/redcap-promis-global-data.RData") # PROMIS global
load("../output/redcap-promis-pb-data.RData")     # PROMIS pain behav (PB)
load("../output/redcap-promis-pi-data.RData")     # PROMIS pain interference (PI)

#######
#     #
# BSI #
#     #
#######

# Subject-wise
bsi_ss <- 
  redcap_bsi_data %>%
  filter(complete.cases(.)) %>%
  select(ss:bsi7) %>%
  pivot_longer(-ss) %>%
  group_by(ss) %>%
  summarise(bsi = sum(value), n = n()) %>%
  ungroup()

# Sample-wise
bsi_sum <- 
  bsi_ss %>%
  summarise(
    m = mean(bsi), 
    sd = sd(bsi), 
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(bsi, .025),
    ul = quantile(bsi, .975)
    )

# plot
pj <- position_jitter(width = .1)
ggplot(bsi_ss, aes(x = "BSI", y= bsi)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    fill = "black", 
    alpha = 1/3
    ) + 
  geom_boxplot(
    position = position_nudge(x = -.2, y = 0), 
    width = .1, 
    fill = "black", 
    alpha = 1/3
    ) +
  geom_point(
    data = bsi_sum, 
    aes(x = "BSI", y = m), 
    position = position_nudge(x = .3, y = 0),
    color = rdgy_pal[3]
    ) +
  geom_errorbar(
    data = bsi_sum, 
    aes(y = m, ymin = ll, ymax = ul), 
    position = position_nudge(x = .3, y = 0),
    width = .1,
    color = rdgy_pal[3]
    ) +
  labs(x = NULL, y = "BSI", caption = "95% CI error bars.") + 
  theme_classic()

########
#      #
# CMSI #
#      #
########

# 1 = 3 months during the last year (12 mos)
# 2 = 3 months during your lifetime

# rawest data
cmsi_data <- 
  redcap_cmsi_data %>% 
  select(ss:cmsi_gen41___2) %>%
  pivot_longer(-ss) %>%
  separate(name, into = c("meas", "q", "time")) %>%
  filter(complete.cases(.)) # there are a few participants without subject numbers

cmsi_ss <-
  cmsi_data %>%
  group_by(ss, time) %>%
  summarise(cmsi = sum(value), n = n()) %>%
  ungroup()
# cmsi_ss %>% filter(n<41) # all 41 questions

# PLOT
ggplot(cmsi_ss, aes(time, cmsi)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    fill = "black", 
    alpha = 1/3
    ) +
  labs(x = "Time", y = "CMSI") +
  theme_minimal()


##################
#                #
# MENSTRUAL DATA #
#                #
##################

menstrual_ss <- 
  redcap_menstrual_data %>%
  select(ss:mh23a) %>%
  pivot_longer(-ss) 

menstrual_sum <- 
  menstrual_ss %>%
  filter(complete.cases(value)) %>%
  group_by(name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(value, .025),
    ul = quantile(value, .975)
    )

# PLOT 
ggplot(menstrual_ss, aes(name, value)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    fill = "black", 
    alpha = 1/3
  ) +
  geom_point(
    data = menstrual_sum, 
    aes(y = m),
    width = .1,
    position = position_nudge(x = -.2, y = 0)
    ) +
  geom_errorbar(
    data = menstrual_sum,
    aes(y = m, ymin = ll, ymax = ul),
    width = .1,
    position = position_nudge(x = -.2, y = 0)
    ) +
  labs("Menstrual Pain Question", y = "Pain Rating") +
  theme_minimal()

########
#      #
# GUPI #
#      #
########

# Pain subscale: sum of 1a-d, 2a-d, 3, 4
# Urinary Subscale: sum of 5 and 6
# QOL impact: sum of 7, 8, 9
# Total score = sum of all subscale scores

# subject-wise
gupi_ss <- 
  redcap_gupi_data %>%
  filter(complete.cases(.)) %>%
  mutate(
    pain_subscale = gupi1a+gupi1b+gupi1c+gupi1d+gupi2a+gupi2b+gupi2c+gupi2d+gupi3+gupi4,
    urinary_subscale = gupi5+gupi6,
    qol_subscale = gupi7+gupi8+gupi9,
    gupi_total = pain_subscale+urinary_subscale+qol_subscale
  ) %>%
  select(ss, pain_subscale:gupi_total) %>%
  pivot_longer(-ss)

# sample-wise
gupi_sum <- 
  gupi_ss %>%
  group_by(name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(value, .025),
    ul = quantile(value, .975)
  )

# PLOT
ggplot(gupi_ss, aes(name, value, color = name, fill = name)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    alpha = 1/3
  ) +
  geom_point(
    data = gupi_sum, 
    aes(y = m),
    position = position_nudge(x = -.2, y = 0)
  ) +
  geom_errorbar(
    data = gupi_sum,
    aes(y = m, ymin = ll, ymax = ul),
    width = .1,
    position = position_nudge(x = -.2, y = 0)
  ) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  scale_fill_manual(values = ghibli_palettes$PonyoMedium) +
  theme_classic() +
  labs(x = "GUPI Scales", y = "Rating Score", caption = "95% CI error bars") +
  theme(legend.position = "none")


###############
#             #
# ICSI + ICPI #
#             #
###############

ic_ss <-
  redcap_ic_data %>%
  mutate(icsi = ic1a+ic1b+ic1c+ic1d, icpi = ic2a+ic2b+ic2c+ic2d) %>%
  select(ss, icsi, icpi) %>%
  pivot_longer(-ss)

ic_sum <- 
  ic_ss %>%
  filter(complete.cases(value)) %>%
  group_by(name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(value, .025),
    ul = quantile(value, .975)
  )

# PLOT
ggplot(ic_ss, aes(name, value, color = name, fill = name)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    alpha = 1/3
  ) +
  geom_point(
    data = ic_sum, 
    aes(y = m),
    position = position_nudge(x = -.2, y = 0)
  ) +
  geom_errorbar(
    data = ic_sum,
    aes(y = m, ymin = ll, ymax = ul),
    width = .1,
    position = position_nudge(x = -.2, y = 0)
  ) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  scale_fill_manual(values = ghibli_palettes$PonyoMedium) +
  theme_classic() +
  labs(x = "Iterstitial Cystitis", y = "Rating Score", caption = "95% CI error bars") +
  theme(legend.position = "none")


########
#      #
# ROME #
#      #
########

# Diagnostic criteria for IBS:
# Recurrent abdominal pain or discomfort** at least 3 days/month in last 
# 3 months associated with two or more of criteria 1-3 below:
# Pain or discomfort at least 2-3 days/month (question 1 > 2)
# For women, does pain occur only during menstrual bleeding? (question 2=0 or 2)
# * Criteria fulfilled for the last 3 months with symptom onset at least 
# 6 months prior to diagnosis [ Yes. (question 3=1) ]

# 1) 1. Improvement with defecation
# Pain or discomfort gets better after BM at least sometimes (question 4>0)

# 2) Onset associated with a change in frequency of stool
# Onset of pain or discomfort associated with more stools at least sometimes 
# (question 5>0)
#! OR
# Onset of pain or discomfort associated with fewer stools at least sometimes 
# (question 6>0)

# 3. Onset associated with a change in form (appearance) of stool
# Onset of pain or discomfort associated with looser stools at least sometimes 
# (question 7>0)
#! OR
# Onset of pain or discomfort associated wit harder stools at least sometimes 
# (question 8>0)

rome_init <- 
  redcap_rome_data %>%
  # gets rid of those that didn't answer the first question
  # if they answered 0 on first question, then the rest was not administered
  filter(complete.cases(rome1)) %>%
  select(ss:rome10)

# These participants do not have IBS
rome_no_ibs <- 
  rome_init %>% filter(rome1 <= 2) %>% mutate(ibs = 0, ibs_sub = NA)

# These participants likely will have ibs, but are deeply characterized to make sure
rome_ibs_qual <-
  rome_init %>%
  filter(rome1 > 2) %>%
  mutate(
    # checks longevity
    stage1 = ifelse(rome3 == 1, 1, 0), #! rome2 rule removed to comply with ROME IV
    # 1) improvement with defecation
    stage2 = ifelse(rome4 > 0, 1, 0),
    # 2) onset assoc with a change in freq of stool
    stage3 = ifelse(rome5 > 0 | rome6 > 0, 1, 0),
    # 3) onset assoc with a change in form (appearance) of stool
    stage4 = ifelse(rome7 > 0 | rome8 > 0, 1, 0), 
    # Extra subtyping
    ibs_sub = case_when(
      rome9 > 0 & rome10 == 0 ~ "IBS-C", # IBS-Constipation
      rome9 == 0 & rome10 > 0 ~ "IBS-D", # IBS-Diarrhea
      rome9 > 0 & rome10 > 0 ~ "IBS-M",  # IBS-Mixed
      rome9 == 0 & rome10 == 0 ~ "IBS-U",# IBS-unsubtyped
    )
    ) %>%
  # final determination of ibs status
  mutate(
    ibs = ifelse(stage1 == 1 & (stage2+stage3+stage4)> 1, 1, 0),
    ibs_sub = ifelse(ibs == 0, NA, ibs_sub) # removes subtyping if ibs = 0
    )

# Rome data subject-wise
rome_ss <- 
  bind_rows(
    select(rome_no_ibs, ss, rome1:rome9, ibs, ibs_sub), 
    select(rome_ibs_qual, ss, rome1:rome9, ibs, ibs_sub)
    )

# counts
rome_ss %>% count(ibs) 
rome_ss %>% count(ibs_sub)


#################
#               #
# PROMIS global #
#               #
#################

# Subscales discovered in Hays et al., 2009:

# physical health (gph) - average the following
# global03 (physical health)
# global06 (physical function)
# global07 (pain)
# global08 (fatigue)

# mental health (gmh) - average the following
# global02 (quality of life)
# global04 (mental health)
# global05 (satisfaction with discretionary social activities)
# global10 (emotional problems)

# This was done to compute subscales and to keep in all subjects that did not
# have complete data, as each measure may be able to stand by itself
promis_global_complete <-
  redcap_promis_global_data %>%
  filter(complete.cases(.)) %>%
  # global07 was binned into 5 bins according to Hayes et al., 2009
  mutate(
    global7_bin = case_when(
      global07 == 0 ~ 1,
      between(global07, 1, 3) ~ 2,
      between(global07, 4, 6) ~ 3,
      between(global07, 7, 9) ~ 4,
      global07 == 10 ~ 5
    )
    ) %>%
  rowwise() %>%
  mutate(
    gph = mean(global03, global06, global07_bin, global08, na.rm = FALSE),
    gmh = mean(global02, global04, global05, global10, na.rm = FALSE)
    )

# Subject-wise (will have missing data)
promis_global_ss <-
  redcap_promis_global_data %>%
  left_join(., promis_global_complete %>% select(ss, gph, gmh), by = "ss") %>%
  select(ss:gmh) %>%
  filter(complete.cases(ss)) 

# subject-wise long
promis_global_ss_long <- 
  promis_global_ss %>%
  pivot_longer(-ss) %>%
  filter(complete.cases(value))

# Sample-wise
promis_global_sum <- 
  promis_global_ss_long %>%
  group_by(name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(value, .025),
    ul = quantile(value, .975)
  )

# PLOT
ggplot(promis_global_ss_long, aes(name, value)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    fill = "black",
    alpha = 1/3
  ) +
  geom_point(
    data = promis_global_sum, 
    aes(y=m), 
    position = position_nudge(x = -.3, y = 0),
    color = rdgy_pal[9]
    ) +
  geom_errorbar(
    data = promis_global_sum,
    aes(y = m, ymin=ll, ymax = ul), 
    width = .2,
    position = position_nudge(x = -.3, y = 0),
    color = rdgy_pal[9]
    ) +
  scale_y_continuous(breaks = seq(0,11,1), minor_breaks = NULL) +
  labs(x = "Promis Global Health Questions", y = "Rating") +
  theme_minimal()


#######################################
#                                     #
# PROMIS Pain Behavior + Interference #
#                                     #
#######################################

# Pain behavior subject-wise
promis_pb_ss <-
  redcap_promis_pb_data %>%
  filter(complete.cases(.)) %>%
  mutate(promis_pb = (painbe2+painbe3+painbe8+painbe24+painbe25+painbe37+painbe45)) %>%
  select(ss, promis_pb)

# Pain interference subject-wise
promis_pi_ss <- 
  redcap_promis_pi_data %>%
  filter(complete.cases(.)) %>%
  mutate(promis_pi = (painin3+painin8+painin9+painin10+painin14+painin26)) %>%
  select(ss, promis_pi)

# Combines both
promis_pb_pi_ss <- inner_join(promis_pb_ss, promis_pi_ss, by = "ss")

# Summary
promis_pb_pi_sum <-
  promis_pb_pi_ss %>%
  pivot_longer(-ss) %>%
  group_by(name) %>%
  summarise(
    m = mean(value),
    sd = sd(value),
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(value, .025),
    ul = quantile(value, .975)
  ) %>%
  ungroup()

# PLOT
promis_pb_pi_ss %>% 
  pivot_longer(-ss) %>%
  ggplot(., aes(name, value)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_flat_violin(
    position = position_nudge(x = .2, y = 0), 
    fill = "black",
    alpha = 1/3
  ) +
  geom_point(
    data = promis_pb_pi_sum, 
    aes(y=m), 
    position = position_nudge(x = -.3, y = 0),
    color = rdgy_pal[9]
  ) +
  geom_errorbar(
    data = promis_pb_pi_sum,
    aes(y = m, ymin=ll, ymax = ul), 
    width = .2,
    position = position_nudge(x = -.3, y = 0),
    color = rdgy_pal[9]
  ) +
  labs("PROMIS Scales", y = "Summed Rating", caption = "95% CI error bars.") +
  theme_classic()

#############################################################################
#                                                                           #
# Putting all these measures together for PCA visualization and/or modeling #
#                                                                           #
#############################################################################

# prepro some of these to make wide/ready for concatenation ----

# BSI
bsi_ss_ready <- select(bsi_ss, -n) 

# CMSI
cmsi_ss_ready <-
  cmsi_ss %>% 
  select(-n) %>% 
  pivot_wider(id_cols = ss, names_from = time, values_from = cmsi) %>%
  # 1 = 3 months during the last year (12 mos)
  # 2 = 3 months during your lifetime
  rename(cmsi_3mos_lastyear = `1`, cmsi_3mos_lifetime = `2`)

# menstrual data
menstrual_ss_ready <- 
  menstrual_ss %>% 
  filter(complete.cases(ss)) %>%
  pivot_wider(id_cols = ss, names_from = name, values_from = value)
  
# GUPI
gupi_ss_ready <- gupi_ss %>% pivot_wider(id_cols = ss, names_prefix = "gupi_")

# ICSI + ICPI
ic_ss_ready <- ic_ss %>% filter(complete.cases(ss)) %>% pivot_wider(ss)

# ROME
rome_ss_ready <- rome_ss %>% select(ss, ibs, ibs_sub)

# PROMIS global
promis_global_ss_ready <- promis_global_ss

# PROMIS pain behav (PB) + PROMIS pain interference (PI)
promis_pb_pi_ss_ready <- promis_pb_pi_ss

# Combining all measures here
extra_pca_data <-
  ss_codes %>%
  select(ss) %>%
  left_join(., bsi_ss_ready, by = "ss") %>%
  left_join(., cmsi_ss_ready, by = "ss") %>%
  left_join(., menstrual_ss_ready, by = "ss") %>%
  left_join(., gupi_ss_ready, by = "ss") %>%
  left_join(., ic_ss_ready, by = "ss") %>%
  left_join(., rome_ss_ready, by = "ss") %>%
  left_join(., promis_global_ss_ready, by = "ss") %>%
  left_join(., promis_pb_pi_ss_ready, by = "ss")

# saves out
save(extra_pca_data, file = "../output/extra-data.RData") # RData
write_csv(extra_pca_data, "../output/extra-data.csv")     # CSV
