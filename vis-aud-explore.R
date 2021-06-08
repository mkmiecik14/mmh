# Visual and Auditory Exploration Script
# Matt Kmiecik
# Started 1 June 2021

# Purpose: explore and visualize visual and auditory data

source("r-prep.R") # Prepare R workspace

# Loads data ----
load("../output/ss-codes.RData") # subject codes
load("../output/auditory-behav-data.RData") # auditory data
load("../output/vis-behav-data.RData") # visual data

# Eliminating kids' data ----
# Note: no need to, proof here:
# vis_behav_data %>% 
#   left_join(., ss_codes, by = "ss") %>%
#   filter(group %nin% c("KID")) # removes kids
# 
# aud_edata %>% 
#   left_join(., ss_codes, by = "ss") %>%
#   filter(group %nin% c("KID")) # removes kids

# Exploring visual data ----

# Histogram
ggplot(vis_behav_data, aes(rating)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~stim)

# Raincloud plot
pj <- position_jitter(width = .1)
ggplot(
  vis_behav_data %>% mutate(stim = as.factor(stim)), 
  aes(stim, rating, group = stim, color = stim, fill = stim)
  ) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(shape = 1, position = pj, alpha = 2/3) +
  geom_boxplot(aes(fill = NULL), width = .2, position = position_nudge(x = -.25, y = 0)) +
  scale_fill_manual(values = ghibli_palettes$MononokeMedium) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Brightness Intensity", y = "Unpleasantness Rating") +
  theme_minimal() +
  theme(legend.position = "none")

# Summary
vis_sum <- 
  vis_behav_data %>%
  group_by(stim) %>%
  summarise(
    M = mean(rating), 
    N = n(), 
    SD = sd(rating), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(rating, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(rating, conf.level = 0.95)$conf.int[2])
    )

# summary plot
pn <- position_nudge(x = .3, y = 0)
pj <- position_jitter(width = .1)
ggplot(vis_sum, aes(stim, M)) +
  geom_point(data = vis_behav_data, aes(y = rating), alpha = 1/3, position = pj) +
  geom_point(position = pn) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2, position = pn) +
  geom_path(position = pn) +
  labs(
    x = "Brightness Intensity", 
    y = "Unpleasantness Rating", 
    caption = "95% CI error bars."
    ) +
  theme_minimal()

# Looking at slopes

# Level 1 mods
lvl1_vis_mod <- 
  vis_behav_data %>% 
  nest_by(ss) %>%
  mutate(mod = list(lm(rating ~ 1 + scale(stim, scale = FALSE), data = data)))

# Level 1 ests
lvl1_vis_est <-
  lvl1_vis_mod %>%
  summarise(broom::tidy(mod)) %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scalestim, scale = FALSE", "stim_mc", term)
  ) %>%
  ungroup()

# slopes are normally distributed; I'd leave it
ggplot(lvl1_vis_est %>% filter(term %in% "stim_mc"), aes(estimate)) +
  geom_histogram(binwidth = .25)

# Auditory ----

# Histogram
ggplot(aud_edata, aes(rating)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~stim)

# Raincloud plot
pj <- position_jitter(width = .1)
ggplot(
  aud_edata %>% mutate(stim = as.factor(stim)), 
  aes(stim, rating, group = stim, color = stim, fill = stim)
) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(shape = 1, position = pj, alpha = 2/3) +
  geom_boxplot(aes(fill = NULL), width = .2, position = position_nudge(x = -.25, y = 0)) +
  scale_fill_manual(values = ghibli_palettes$MononokeMedium) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Loudness Intensity", y = "Unpleasantness Rating") +
  theme_minimal() +
  theme(legend.position = "none")

# Summary
aud_sum <- 
  aud_edata %>%
  group_by(stim) %>%
  summarise(
    M = mean(rating), 
    N = n(), 
    SD = sd(rating), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(rating, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(rating, conf.level = 0.95)$conf.int[2])
  )

# Summary plot
pn <- position_nudge(x = .3, y = 0)
pj <- position_jitter(width = .1)
ggplot(aud_sum, aes(stim, M)) +
  geom_point(data = aud_edata, aes(y = rating), alpha = 1/3, position = pj) +
  geom_point(position = pn) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2, position = pn) +
  geom_path(position = pn) +
  labs(
    x = "Loudness Intensity", 
    y = "Unpleasantness Rating", 
    caption = "95% CI error bars."
  ) +
  theme_minimal()

# Looking at slopes

# Level 1 mods
lvl1_aud_mod <- 
  aud_edata %>% 
  nest_by(ss) %>%
  mutate(mod = list(lm(rating ~ 1 + scale(stim, scale = FALSE), data = data)))

# Level 1 ests
lvl1_aud_est <-
  lvl1_aud_mod %>%
  summarise(broom::tidy(mod)) %>%
  mutate(
    term = gsub("[\\(\\)]", "", term), 
    term = gsub("scalestim, scale = FALSE", "stim_mc", term)
  ) %>%
  ungroup()

# slopes are normally distributed; I'd leave it
ggplot(lvl1_aud_est %>% filter(term %in% "stim_mc"), aes(estimate)) +
  geom_histogram(binwidth = .25)