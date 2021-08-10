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

redcap_rome_data
