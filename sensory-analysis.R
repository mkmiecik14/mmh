# Sensory Analysis
# Matt Kmiecik
# Started 03 MARCH 2022

#* Purpose: This script explores the added variability that is explained by
#* including supraspinal measures of visual and auditory stimulation
#* over and above traditional QST and bladder sensitivity (i.e., bladder test)

source("r-prep.R") # Prepares R workspace

# Loading data ----
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results
load("../output/ss-codes.RData") # subject codes
load("../output/pca-data-all.rda") # pca data

# Calculating pelvic pain outcome variable as an average of:
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

# narrow down the participants in PCA results
# PCA subject numbers
pca_ss <- as.numeric(rownames(pca_res$Fixed.Data$ExPosition.Data$fi)) 

# retains only those in PCA
pelvic_pain_data_pca_ss <- pelvic_pain_data %>% filter(ss %in% pca_ss)
length(unique(pelvic_pain_data_pca_ss$ss)) == length(pca_ss) # same ss

###############
#             #
# AVERAGE VAS #
#             #
###############

pelvic_pain_data_pca_ss %>% filter(!complete.cases(.))

# computes average here
pelvic_pain_avg <- 
  pelvic_pain_data_pca_ss %>%
  filter(complete.cases(.)) %>% # removes those subjects with missing data
  pivot_longer(c(-ss, -year, -timestamp, -days_from_baseline)) %>%
  group_by(ss, year, timestamp, days_from_baseline) %>%
  summarise(pelvic_pain = mean(value), n = n()) %>%
  ungroup()

pelvic_pain_avg %>% filter(n != 3) # all have three observations/timepoint
pelvic_pain_avg %>% filter(is.na(pelvic_pain)) # no missing here

##############
#            #
# Predictors #
#            #
##############

# z score each measure that was used in the PCA for easy combination
#  at this point all measures are pointing in the same direction
pca_data_z <- 
  pca_data_keep %>% 
  mutate(across(-ss, ~as.numeric(scale(.x)))) # computes z scores

# checking work
apply(pca_data_z, 2, mean) # means should all be zero
apply(pca_data_z, 2, sd) # sd should all be 1

# creates predictors
pca_data_z_sum <- 
  pca_data_z %>% 
  mutate(
    supra = aud_mean + aud_slope + vis_mean + vis_slope, # 4
    bladder = bs_pain + fs_pain + fu_pain + mt_pain + fs_urg + fu_urg + 
      mt_urg + bladder_sharp + bladder_pressing + bladder_dull + bladder_prickling, # 11
    qst = after_pain_12 + after_pain_5 + after_pain_6 + after_pain_7 + 
      after_pain_forehead + after_pain_rhip + after_pain_rknee + 
      after_pain_rshoulder + coldpain_resid + cpm_lknee + ppt_N_rForehead + 
      ppt_N_rHip + ppt_N_rKnee + ppt_N_rShoulder + ppt_N_12 + ppt_N_5 + 
      ppt_N_6 + ppt_N_7 + vag_sharp + vag_pressing + vag_dull + vag_prickling + 
      ts_mean + ts_slope + ts_max #25
  )

# widens data for regression analysis
pelvic_pain_avg_wide <- 
  pelvic_pain_avg %>%
  pivot_wider(
    id_cols = ss, 
    names_from = year, 
    names_prefix = "year", 
    values_from = pelvic_pain
    )

# combines sensory data with pelvic pain data in wide format
sensory_data <- 
  pelvic_pain_avg_wide %>% 
  left_join(
    .,
    pca_data_z_sum %>% select(ss, supra, bladder, qst),
    by = "ss"
  ) %>%
  # mean centers for intercept interpretation
  mutate(
    year0_mc = as.numeric(scale(year0, scale = FALSE)),
    supra_mc = as.numeric(scale(supra, scale = FALSE)),
    bladder_mc = as.numeric(scale(bladder, scale = FALSE)),
    qst_mc = as.numeric(scale(qst, scale = FALSE))
  ) %>%
  # z-scores as well
  mutate(
    year0_z = as.numeric(scale(year0)),
    supra_z = as.numeric(scale(supra)),
    bladder_z = as.numeric(scale(bladder)),
    qst_z = as.numeric(scale(qst))
  )

# Visualizes distributions
sensory_data_z_long <- 
  sensory_data %>% 
  select(ss, ends_with("z")) %>% 
  pivot_longer(-ss) %>%
  mutate(name = fct_relevel(name, c("year0_z", "qst_z", "bladder_z", "supra_z")))

pj <- position_jitter(width = .1)
dist_plot <-
  ggplot(sensory_data_z_long, aes(name, value, color = name)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(position = position_nudge(x = .3), width = .2) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = "Predictors", y = "Z-Score") +
  scale_color_manual(values = ghibli_palettes$MononokeMedium[c(3:6)]) + 
  #scale_colour_ghibli_d("MononokeMedium", direction = +1) +
  theme_classic() +
  theme(legend.position = "none")
dist_plot

# Models
year0_mod <- lm(year0 ~ 1 + supra_mc + bladder_mc + qst_mc, data = sensory_data)
summary(year0_mod)
check_model(year0_mod)

year1_mod <- lm(year1 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year1_mod)
check_model(year1_mod)

year2_mod <- lm(year2 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year2_mod)
check_model(year2_mod)

year3_mod <- lm(year3 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year3_mod)
check_model(year3_mod)

year4_mod <- lm(year4 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year4_mod)
check_model(year4_mod)

year5_mod <- lm(year5 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year5_mod)
check_model(year5_mod)

# Zero order correlations
set.seed(14) # sets seed for reproducible boostrapping

# Computes bootstrapped correlations
zero_order_cors <-
  psych::corr.test(
    sensory_data %>% select(year0_mc, supra_mc, bladder_mc, qst_mc),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )
zero_order_cors$ci # the results

# correlation results as tibble for plotting
cor_res <- 
  as_tibble(zero_order_cors$ci, rownames = "var") %>%
  separate(var, into = c("var1", "var2"), sep = "-") %>%
  mutate(var2 = fct_relevel(var2, c("qst_mc", "bladder_mc", "supra_mc")))

cor_plot <-
  ggplot(cor_res %>% filter(var1 == "year0_mc"), aes(var2, r, color = var2)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium[c(4,5,6)]) +
  labs(
    x = "Predictors", 
    y = "r (correlation with baseline pelvic pain)", 
    caption = "95% CI error bars."
    ) +
  theme_classic() +
  theme(legend.position = "none")
cor_plot


# models in a list
mods <- list(year1_mod, year2_mod, year3_mod, year4_mod, year5_mod)

# sample sizes
mods %>%
  map("model") %>%
  map_dfr(~tibble(n = nrow(.x)), .id = "mod")

# computes the partial eta squareds with CI
mods_peta2 <-
  mods %>%
  map(
    ~eta_squared(
      car::Anova(.x, type = 3),
      partial = TRUE, 
      generalized = FALSE, 
      ci = .95, 
      alternative = "two.sided",
      include_intercept = TRUE
    )
  ) %>%
  map_dfr(~as_tibble(.x), .id = "dv_year") %>%
  select(-CI) %>%
  rename(Eta2_partial_CI_low = CI_low, Eta2_partial_CI_high = CI_high)

# computes model estimates
mods_ests <- 
  mods %>% 
  map_dfr(~broom::tidy(.x), .id = "dv_year") %>% 
  rename(Parameter = term) # better for join

# computes standardized regression coefficients with CI
mods_beta <- 
  mods %>% 
  map_dfr(~as_tibble(standardize_parameters(.x)), .id = "dv_year") %>%
  select(-CI) %>%
  rename(beta = Std_Coefficient, beta_CI_low = CI_low, beta_CI_high = CI_high)

# calculates the sums of squares
mods_ss <- 
  mods %>%
  map_dfr(~as_tibble(car::Anova(.x, type = 3), rownames = "Parameter"), .id = "dv_year") %>%
  rename(dfn = Df, SS = `Sum Sq`, F = `F value`, p = `Pr(>F)`)

# reorganizes for the residuals (error)
mods_ss_resids <- 
  mods_ss %>% 
  filter(Parameter == "Residuals") %>%
  rename(SSE = SS, dfd = dfn) %>%
  mutate(MSE = SSE / dfd) %>%
  select(-F, -p, -Parameter)

# includes correct SS with residuals
mods_ss_final <- 
  mods_ss %>% 
  filter(Parameter != "Residuals") %>% 
  left_join(., mods_ss_resids, by = "dv_year")

# combines into one big table
mods_res <-
  mods_ests %>% 
  left_join(., mods_beta, by = c("dv_year", "Parameter")) %>%
  left_join(., mods_peta2, by = c("dv_year", "Parameter")) %>%
  left_join(., mods_ss_final, by = c("dv_year", "Parameter")) %>%
  mutate(Parameter = gsub("[\\(\\)]", "", Parameter)) %>%
  mutate(sig = p.value < .05, Parameter = as.factor(Parameter)) %>%
  mutate(
    Parameter = fct_relevel(
      Parameter, 
      c("Intercept", "year0_mc", "qst_mc", "bladder_mc", "supra_mc")
      )
    )



# PARTIAL ETA SQUARED PLOT
pd <- position_dodge(width = .4)
peta2_plot <- 
  ggplot(
    mods_res %>% filter(Parameter != "Intercept", dv_year < 5), 
    aes(dv_year, Eta2_partial, group = Parameter, color = Parameter)
  ) +
  geom_point(aes(shape = sig), position = pd, size = 2) +
  # geom_errorbar(
  #   aes(ymin = Eta2_partial_CI_low, ymax = Eta2_partial_CI_high), 
  #   width = .2,
  #   position = pd,
  #   alpha = 1/2
  # ) +
  geom_line(position = pd, alpha = 1/2) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium[c(3:6)]) +
  labs(
    x = "Year", 
    y = "Partial Eta^2", 
    caption = "95% CI error bars."
  ) +
  theme_classic() +
  theme(legend.position = "bottom")
peta2_plot #plots

# BETA PLOT
beta_plot <- 
  ggplot(
    mods_res %>% filter(Parameter != "Intercept", dv_year < 5), #  
    aes(dv_year, beta, group = Parameter, color = Parameter)
  ) +
  geom_point(aes(shape = sig), position = pd, size = 2) +
  # geom_errorbar(
  #   aes(ymin = beta_CI_low, ymax = beta_CI_high), 
  #   width = .2,
  #   position = pd
  # ) +
  geom_line(position = pd) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium[c(3:6)]) +
  labs(
    x = "Year", 
    y = "Beta", 
    caption = "95% CI error bars."
  ) +
  coord_cartesian(ylim = c(-.5, .75)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  theme_classic() +
  theme(legend.position = "bottom")
beta_plot #plots

# plots side by side
reg_res_plot <- peta2_plot | beta_plot
reg_res_plot

# 4-panel
sensory_res_plot <- (dist_plot + cor_plot) / (peta2_plot + beta_plot)
sensory_res_plot

# saves out for manuscript
# uncomment out to save
ggsave(
  filename = "../output/sensory-res-plot.svg",
  plot = sensory_res_plot,
  width = 6,
  height = 6,
  units = "in"
  )