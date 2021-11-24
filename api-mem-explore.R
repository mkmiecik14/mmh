# API Additional Mixed-Effect Modeling
# Matt Kmiecik
# Started 24 NOV 2021

#* Purpose: This script explores and analyzes the annual data that was 
#* preprocessed in `api-icsiplus-prepro.R`. Herein I look at additional measures
#* that may indicate outcome

source("r-prep.R") # Prepares R workspace

# Loading data ----
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results

###############
#             #
# Exploration #
#             #
###############

# Looking at menstrual pain w/o NSAID and BSI
this_data <- 
  complete_extra_annual_data %>%
  select(
    ss, 
    year, 
    timestamp, 
    days_from_baseline,
    cramp_pain_no_nsaid,
    bsi
  )

# histograms
ggplot(this_data, aes(cramp_pain_no_nsaid)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~year)

ggplot(this_data, aes(bsi)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~year)

# factor scores for the rows (subjects)
fi <- 
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss))

# this data + PCA res
this_data_pca <-
  left_join(this_data, fi, by = "ss") %>%
  filter(complete.cases(V1)) # filters out those w/o PCA res

# only complete BSI data
bsi_data <- 
  this_data_pca %>% 
  filter(complete.cases(bsi)) %>% # filters out those without BSI
  group_by(ss) %>%
  mutate(timepoints = n()) %>%
  ungroup()

# only complete cramp pain no nsaid data
cramp_pain_data <- 
  this_data_pca %>% 
  filter(complete.cases(cramp_pain_no_nsaid)) %>% # filters out those without BSI
  group_by(ss) %>%
  mutate(timepoints = n()) %>%
  ungroup()

################################
#                              #
# modeling the cramp pain data #
#                              #
################################

# Data for lmer - includes participants with at least 2 timepoints for slopes
lmer_data <- cramp_pain_data %>% filter(timepoints > 1) # greater than 1 tp
length(unique(lmer_data$ss)) # n=171, 29 lost to follow-up

# Maximal model - checking to see if random intercepts and slopes converges
max_mod <- 
  lmer(
    cramp_pain_no_nsaid ~ 1 + year + (1 + year | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(max_mod)
performance(max_mod)
check_model(max_mod)

min_mod <- 
  lmer(
    cramp_pain_no_nsaid ~ 1 + year + (1 | ss), 
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
    cramp_pain_no_nsaid ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(full_mod)
performance(full_mod)
check_model(full_mod)
anova(max_mod, full_mod) # compares against model without moderators

# plots the observed and fitted slopes for each subject
full_mod_aug <- augment(full_mod)
min(full_mod_aug$cramp_pain_no_nsaid)
max(full_mod_aug$cramp_pain_no_nsaid)
these_ss <- unique(full_mod_aug$ss)

# my own function to plot fitted and observed slopes
my_cowplot <-
  function(x = 1:25){
    ggplot(full_mod_aug %>% filter(ss %in% these_ss[x]), aes(year, cramp_pain_no_nsaid)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_line(aes(y = .fitted), linetype = 3, size = 1, color = rdgy_pal[3]) +
      facet_wrap(~ss) +
      coord_cartesian(ylim = c(0, 100)) +
      theme_minimal()  
  }

my_cowplot(1:25)
my_cowplot(26:50)
my_cowplot(51:75)
my_cowplot(76:100)
my_cowplot(101:125)
my_cowplot(126:150)
my_cowplot(151:171)

# probing interactions

# V1
interact_plot(full_mod, pred = year, modx = V1, plot.points = TRUE, interval = TRUE)
sim_slopes(full_mod, pred = year, modx = V1, jnplot = TRUE)

# V3
interact_plot(full_mod, pred = year, modx = V3, plot.points = TRUE, interval = TRUE)
sim_slopes(full_mod, pred = year, modx = V3, jnplot = TRUE)

#########################
#                       #
# modeling the BSI data #
#                       #
#########################

# Data for lmer - includes participants with at least 2 timepoints for slopes
lmer_data <- bsi_data %>% filter(timepoints > 1) # greater than 1 tp
length(unique(lmer_data$ss)) # n=171, 29 lost to follow-up

# Maximal model - checking to see if random intercepts and slopes converges
max_mod <- 
  lmer(
    bsi ~ 1 + year + (1 + year | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(max_mod)
performance(max_mod)
check_model(max_mod)

min_mod <- 
  lmer(
    bsi ~ 1 + year + (1 | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(min_mod)
performance(min_mod)
check_model(min_mod)

anova(min_mod, max_mod)

# Full model - includes additional fixed effects
full_mod <- 
  lmer(
    bsi ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
    data = lmer_data, 
    REML = TRUE
  )
summary(full_mod)
performance(full_mod)
check_model(full_mod)
anova(max_mod, full_mod) # compares against model without moderators

# plots the observed and fitted slopes for each subject
full_mod_aug <- augment(full_mod)
min(full_mod_aug$bsi)
max(full_mod_aug$bsi)
these_ss <- unique(full_mod_aug$ss)

# my own function to plot fitted and observed slopes
my_cowplot <-
  function(x = 1:25){
    ggplot(full_mod_aug %>% filter(ss %in% these_ss[x]), aes(year, bsi)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_line(aes(y = .fitted), linetype = 3, size = 1, color = rdgy_pal[3]) +
      facet_wrap(~ss) +
      coord_cartesian(ylim = c(0, 25)) +
      theme_minimal()  
  }

my_cowplot(1:25)
my_cowplot(26:50)
my_cowplot(51:75)
my_cowplot(76:100)
my_cowplot(101:125)
my_cowplot(126:150)
my_cowplot(151:171)

# probing interactions

# V1
interact_plot(full_mod, pred = year, modx = V1, plot.points = TRUE, interval = TRUE)
sim_slopes(full_mod, pred = year, modx = V1, jnplot = TRUE)

# V3
interact_plot(full_mod, pred = year, modx = V3, plot.points = TRUE, interval = TRUE)
sim_slopes(full_mod, pred = year, modx = V3, jnplot = TRUE)

# Visualizing change over time in several of the variables
plot_data <-
  complete_extra_annual_data %>%
  select(ss, year, cramp_pain_no_nsaid, cramp_pain_with_nsaid, bsi:global_health) %>%
  pivot_longer(c(-ss, -year)) 

pj <- position_jitter(width = .2, height = 0)
ggplot(plot_data, aes(year, value)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free") +
  theme_minimal()

pj <- position_jitter(width = .2, height = 0)
ggplot(plot_data %>% filter(ss %in% fi$ss), aes(year, value)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free") +
  theme_minimal()




