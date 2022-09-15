# API ICSI PLUS EXPLORE
# Matt Kmiecik
# Started 23 NOV 2021

#* Purpose: This script explores and analyzes the annual data that was 
#* preprocessed in `api-icsiplus-prepro.R`. Combining several different measures
#* of pelvic/nonpelvic pain main increase variability over time

source("r-prep.R") # Prepares R workspace

# Loading data
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results
load("../output/ss-codes.RData") # subject codes

# First, exploring one option of pelvic pain. A measure combining:
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

# Histogram
ggplot(pelvic_pain_data_long, aes(value)) +
  geom_histogram(binwidth = 5) +
  facet_grid(name~year)

# Density plot
ggplot(pelvic_pain_data_long, aes(value, color = name)) +
  geom_density(size = .7) +
  facet_wrap(~year) +
  theme_classic() +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

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

# longitudinal plot
pj <- position_jitter(width = .2, height = 0)
ggplot(pelvic_pain_avg, aes(year, pelvic_pain)) + # year
  geom_point(alpha = 1/3, position = pj) +
  geom_line(
    stat ="smooth", 
    method = "lm", 
    formula = y ~ x,
    alpha = 1/3,
    aes(group = ss),
    color = "black"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_smooth(method = "lm", se = TRUE, color = rdgy_pal[3]) +
  theme_classic()

# examining collinearity of individual pelvic pain VAS vars
pp_simple <- 
  pelvic_pain_data_pca_ss %>% 
  select(
    ss, 
    year, 
    urination_pain_last_week, 
    bowel_mov_pain_last_week, 
    mens_nonmens_pain_week
    )

pp_simple_cor_res <- 
  pp_simple %>% 
  split(.$year) %>%
  map(~psych::corr.test(
    select(.x, -ss, -year),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )
  )

# correlation results
pp_simple_cor_res %>% map_dfr("ci", .id = "year")
  
# combines with PCA factors
# factor scores for the rows (subjects)
fi <-
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss))
pelvic_pain_avg_fi <- pelvic_pain_avg %>% left_join(., fi, by = "ss")

# LINEAR MIXED EFFECTS MODELING ----

# maximal model
pp_avg_max_mod <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 + year | ss), 
    data = pelvic_pain_avg_fi, 
    REML = TRUE
  )
summary(pp_avg_max_mod) # model summary

# minimal model
pp_avg_min_mod <- 
  lmer(
    pelvic_pain ~ 1 + year + (1 | ss), 
    data = pelvic_pain_avg_fi, 
    REML = TRUE
  )
summary(pp_avg_min_mod) # model summary
anova(pp_avg_min_mod, pp_avg_max_mod) # model fit improved with random slopes

# full model
pelvic_pain_avg_mod <- 
  lmer(
  pelvic_pain ~ 1 + year + year*V1 + year*V3 + (1 + year | ss), 
  data = pelvic_pain_avg_fi, 
  REML = TRUE
)

summary(pelvic_pain_avg_mod) # model summary
performance(pelvic_pain_avg_mod) # model performance
check_model(pelvic_pain_avg_mod) # checks assumptions
anova(pp_avg_max_mod, pelvic_pain_avg_mod) # model fit improved with PCs

# Following up interactions
# Principal component 1
# Johnson-Neyman plot
sim_slopes(pelvic_pain_avg_mod, pred = year, modx = V1, jnplot = TRUE)

# cols
scales::show_col(pal_jco(palette = "default", alpha = 1)(10))
jco_cols <- pal_jco("default", alpha = 1)(10) # from ggsci

pc1_interact <- 
  interact_plot(
    pelvic_pain_avg_mod, 
    pred = year, 
    modx = V1, 
    plot.points = TRUE, 
    interval = TRUE, 
    partial.residuals = FALSE, # controls for V3
    jitter = .1,
    colors = jco_cols, #jco_cols,
    vary.lty = FALSE,
    point.alpha = 1/2
    ) +
  labs(x = "Year", y = "Pelvic Pain") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_classic() + 
  theme(legend.position = "bottom")
pc1_interact


# Principal component 3 
sim_slopes(pelvic_pain_avg_mod, pred = year, modx = V3, jnplot = TRUE)
pc3_interact <- 
  interact_plot(
    pelvic_pain_avg_mod, 
    pred = year, 
    modx = V3, 
    plot.points = TRUE, 
    interval = TRUE, 
    partial.residuals = FALSE, # controls for V3
    jitter = .1,
    colors = jco_cols, #jco_cols,
    vary.lty = FALSE,
    point.alpha = 1/2
  ) +
  labs(x = "Year", y = "Pelvic Pain") +
  coord_cartesian(ylim = c(0, 100)) +
  theme_classic() + 
  theme(legend.position = "bottom")
pc3_interact


# plots the observed and fitted slopes for each subject
this_aug <- augment(pelvic_pain_avg_mod)
min(this_aug$pelvic_pain)
max(this_aug$pelvic_pain)
these_ss <- unique(this_aug$ss)

# my own function to plot fitted and observed slopes
my_cowplot <-
  function(x = 1:25){
    ggplot(this_aug %>% filter(ss %in% these_ss[x]), aes(year, pelvic_pain)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      geom_line(aes(y = .fitted), linetype = 3, size = 1, color = rdgy_pal[3]) +
      facet_wrap(~ss) +
      coord_cartesian(ylim = c(0, 100)) +
      theme_minimal()  
  }

# Uncomment to see these plots
# my_cowplot(1:25)
# my_cowplot(26:50)
# my_cowplot(51:75)
# my_cowplot(76:100)
# my_cowplot(101:125)
# my_cowplot(126:150)
# my_cowplot(151:175)
# my_cowplot(176:200)

# Modeling the simple outcome at year 2 ----
# gets data into wide with fi
pelvic_pain_avg_fi_wide <- 
  pelvic_pain_avg %>% 
  pivot_wider(
    id_cols = ss, 
    names_from = year, 
    values_from = pelvic_pain,
    names_prefix = "year_"
    ) %>%
  left_join(., fi, by = "ss")

# mods
mod1 <- lm(year_2 ~ 1 + year_0, data = pelvic_pain_avg_fi_wide)
summary(mod1)

mod2 <- lm(year_2 ~ 1 + year_0 + V1 + V3, data = pelvic_pain_avg_fi_wide)
summary(mod2)

# Model comparison
anova(mod1, mod2) # ANOVA
modelCompare(mod1, mod2) # delta R2 (uses lmSupport pkg)
# 200-71

# mods - mean centered
mod1_mc <- lm(year_2 ~ 1 + scale(year_0, scale = FALSE), data = pelvic_pain_avg_fi_wide)
summary(mod1_mc)

mod2_mc <- 
  lm(
    year_2 ~ 
      1 + 
      scale(year_0, scale = FALSE) + 
      scale(V1, scale = FALSE) + 
      scale(V3, scale = FALSE), 
    data = pelvic_pain_avg_fi_wide
  )
summary(mod2_mc)

# Model comparison
anova(mod1_mc, mod2_mc) # ANOVA
modelCompare(mod1_mc, mod2_mc) # delta R2 (uses lmSupport pkg)

# FULL MODEL ESTIMATES
mod1_mc_ests <- 
  tidy(mod1_mc, conf.int = TRUE, conf.level = .95) %>%
  mutate(step = 1, term = c("Intercept", "Year0")) %>%
  relocate(step)

mod2_mc_ests <- 
  tidy(mod2_mc, conf.int = TRUE, conf.level = .95) %>%
  mutate(step = 2, term = c("Intercept", "Year0", "V1", "V3")) %>%
  relocate(step)

# combines model estimates
mod12_mc_ests <- bind_rows(mod1_mc_ests, mod2_mc_ests)

# EXTRACTING ALL MODEL SPECIFICS
mod1_mc_int <- 
  lm(year_2 ~ 0 + scale(year_0, scale = FALSE), data = pelvic_pain_avg_fi_wide)

mod1_mc_year0 <- 
  lm(year_2 ~ 1, data = pelvic_pain_avg_fi_wide)

mod2_mc_int<- 
  lm(
  year_2 ~ 
    0 + 
    scale(year_0, scale = FALSE) + 
    scale(V1, scale = FALSE) + 
    scale(V3, scale = FALSE), 
  data = pelvic_pain_avg_fi_wide
)

mod2_mc_year0 <- 
  lm(
    year_2 ~ 
      1 + 
      #scale(year_0, scale = FALSE) + 
      scale(V1, scale = FALSE) + 
      scale(V3, scale = FALSE), 
    data = pelvic_pain_avg_fi_wide
  )

mod2_mc_v1 <- 
  lm(
    year_2 ~ 
      1 + 
      scale(year_0, scale = FALSE) + 
      #scale(V1, scale = FALSE) + 
      scale(V3, scale = FALSE), 
    data = pelvic_pain_avg_fi_wide
  )

mod2_mc_v3 <- 
  lm(
    year_2 ~ 
      1 + 
      scale(year_0, scale = FALSE) + 
      scale(V1, scale = FALSE), 
      #scale(V3, scale = FALSE), 
    data = pelvic_pain_avg_fi_wide
  )

# Sums of squares, PRE, MSE, ETC.
# STEP 1 INTERCEPT
ss_1_int <- 
  as.data.frame(t(unlist(modelCompare(ModelC = mod1_mc_int, ModelA = mod1_mc)))) %>%
  mutate(
    SS = sseC - sseA, 
    MS = SS/nDF, 
    MSE = sseA/dDF,
    step = 1,
    term = "Intercept"
    )
# STEP 1 YEAR0 SLOPE
ss_1_year0 <-
  as.data.frame(t(unlist(modelCompare(ModelC = mod1_mc_year0, ModelA = mod1_mc)))) %>%
  mutate(
    SS = sseC - sseA, 
    MS = SS/nDF, 
    MSE = sseA/dDF,
    step = 1,
    term = "Year0"
  )
# STEP 2 INTERCEPT
ss_2_int <- 
  as.data.frame(t(unlist(modelCompare(ModelC = mod2_mc_int, ModelA = mod2_mc)))) %>%
  mutate(
    SS = sseC - sseA, 
    MS = SS/nDF, 
    MSE = sseA/dDF,
    step = 2,
    term = "Intercept"
  )
# STEP 2 Year0 SLOPE
ss_2_year0 <- 
  as.data.frame(t(unlist(modelCompare(ModelC = mod2_mc_year0, ModelA = mod2_mc)))) %>%
  mutate(
    SS = sseC - sseA, 
    MS = SS/nDF, 
    MSE = sseA/dDF,
    step = 2,
    term = "Year0"
  )
# STEP 2 V1 SLOPE
ss_2_v1 <- 
  as.data.frame(t(unlist(modelCompare(ModelC = mod2_mc_v1, ModelA = mod2_mc)))) %>%
  mutate(
    SS = sseC - sseA, 
    MS = SS/nDF, 
    MSE = sseA/dDF,
    step = 2,
    term = "V1"
  )
# STEP 2 V3 SLOPE
ss_2_v3 <- 
  as.data.frame(t(unlist(modelCompare(ModelC = mod2_mc_v3, ModelA = mod2_mc)))) %>%
  mutate(
    SS = sseC - sseA, 
    MS = SS/nDF, 
    MSE = sseA/dDF,
    step = 2,
    term = "V3"
  )

# COMBINES TOGETHER
mod12_ss <-
  bind_rows(ss_1_int, ss_1_year0, ss_2_int, ss_2_year0, ss_2_v1, ss_2_v3) %>%
  relocate(step, term)

# combines with model estimates
hier_reg_res <-
  left_join(mod12_mc_ests, mod12_ss, by = c("step", "term")) %>%
  # further cleaning
  select(
    step,
    term,
    b = estimate,
    LL = conf.low,
    UL = conf.high,
    SE = std.error,
    SS,
    MSE,
    F = Fstat,
    p = p.value,
    partialeta2 = PRE,
    nDF,
    dDF
  )

# FOR PUBLICATION TABLE
#write_csv(hier_reg_res, file = "../output/hier-reg-res.csv")


# Run the models at each year and look at differences in R^2 ----
# mean centers columns
pelvic_pain_avg_fi_wide_mc <- 
  pelvic_pain_avg_fi_wide %>%
  # mean centers baseline PP (year_0) and all the components
  mutate(across(.cols = c(year_0, V1:V40), .fns = ~as.numeric(scale(.x, scale=FALSE))))

# Runs models
mod_y1 <- lm(year_1 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y2 <- lm(year_2 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y3 <- lm(year_3 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y4 <- lm(year_4 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y5 <- lm(year_5 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)

# Checks models
check_model(mod_y1)
check_model(mod_y2)
check_model(mod_y3)
check_model(mod_y4)
check_model(mod_y5)

check_heteroskedasticity(mod_y1)
check_heteroskedasticity(mod_y2)
check_heteroskedasticity(mod_y3)
check_heteroskedasticity(mod_y4)

check_normality(mod_y1) 
check_normality(mod_y2)
check_normality(mod_y3)
check_normality(mod_y4)

check_outliers(mod_y1)
check_outliers(mod_y2)
check_outliers(mod_y3)
check_outliers(mod_y4)

compare_performance(mod_y1, mod_y2, mod_y3, mod_y4, rank = TRUE)

# saving out models
pca_mods <- list(mod_y1, mod_y2, mod_y3, mod_y4, mod_y5)
#save(pca_mods, file = "../output/pca-mods.rda") # uncomment out to save

# Zero order correlations ----
set.seed(14) # sets seed for reproducible boostrapping

# Computes bootstrapped correlations
zero_order_cors <-
  psych::corr.test(
    pelvic_pain_avg_fi_wide_mc %>% select(year_0, V1, V2, V3),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )
zero_order_cors$ci # the results

# models in a list
mods <- list(mod_y1, mod_y2, mod_y3, mod_y4, mod_y5)

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
  mutate(sig = p.value < .05)

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
  #   ) +
  geom_line(position = pd, alpha = 1/2) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(
    values = c(
      ghibli_palettes$PonyoMedium[c(3, 5, 7)], # ghibli_palettes$PonyoMedium[c(1, 3, 5)],
      ghibli_palettes$PonyoMedium[2]
      )
    ) +
  labs(
    x = "Year", 
    y = "Partial Eta^2", 
    #caption = "95% CI error bars."
    ) +
  theme_classic() +
  coord_cartesian(ylim = c(0, .25)) +
  theme(legend.position = "none")
peta2_plot #plots

# BETA PLOT
beta_plot <- 
  ggplot(
  mods_res %>% filter(Parameter != "Intercept", dv_year < 5), #  
  aes(dv_year, beta, group = Parameter, color = Parameter)
) +
  geom_point(aes(shape = sig), position = pd, size = 2) +
  geom_errorbar(
    aes(ymin = beta_CI_low, ymax = beta_CI_high), 
    width = .2,
    position = pd
  ) +
  geom_line(position = pd) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium[c(1, 3, 5, 6, 7)]) +
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

# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/reg-res-plot-v2.svg",
#   plot = reg_res_plot,
#   width = 5,
#   height = 4.5,
#   units = "in"
#   )

# Saves regression results out to table for manuscript
reg_res_table <- 
  mods_res %>%
  mutate(F_new = statistic^2) %>%
  select(
    Year = dv_year,
    Parameter,
    b = estimate,
    SE = std.error,
    beta,
    beta_CI_low,
    beta_CI_high,
    SS,
    MSE,
    F = F_new,
    dfn,
    dfd,
    p = p.value,
    Eta2_partial,
    Eta2_partial_CI_low,
    Eta2_partial_CI_high,
  ) %>%
  mutate(
    Parameter = case_when(
      Parameter == "year_0" ~ "Baseline Pelvic Pain",
      Parameter == "V1" ~ "PC 1",
      Parameter == "V2" ~ "PC 2",
      Parameter == "V3" ~ "PC 3",
      TRUE ~ as.character(Parameter)
    )
  )

# saves out table for publication
# uncomment to save out
# write_csv(reg_res_table, file = "../output/reg-res-table.csv")

# Pelvic Pain Outcome Variable Descriptive Stats and Plot ----
# subject-wise data
pelvic_pain_avg_ss <- 
  pelvic_pain_avg_fi_wide %>% 
  select(ss, starts_with("year_")) %>%
  pivot_longer(-ss, names_to = "year", values_to = "pelvic_pain") %>%
  mutate(year = as.numeric(regmatches(year, regexpr("\\d", year))))

# summary
pelvic_pain_avg_sum <- 
  pelvic_pain_avg_ss %>%
  filter(complete.cases(pelvic_pain)) %>%
  group_by(year) %>%
  summarise(
    m = mean(pelvic_pain),
    med = median(pelvic_pain),
    sd = sd(pelvic_pain),
    n = n(),
    sem = sd/sqrt(n),
    min = min(pelvic_pain),
    max = max(pelvic_pain),
    ll = quantile(pelvic_pain, .025, na.rm = TRUE),
    ul = quantile(pelvic_pain, .975, na.rm = TRUE)
    ) %>%
  ungroup()

# for publication table
# uncomment to save out
# write_csv(pelvic_pain_avg_sum, file = "../output/pelvic-pain-avg-sum.csv")

# Pelvic Pain Outcome Plot
this_color <- ghibli_palettes$PonyoMedium[2]
pj <- position_jitter(width = .2, height = 0)
pn <- position_nudge(x = .5)

pp_outcome_plot <- 
  ggplot(pelvic_pain_avg_sum %>% filter(year < 5), aes(year, m)) +
  geom_point(
    data = pelvic_pain_avg_ss %>% filter(year < 5), 
    aes(y = pelvic_pain),
    position = pj,
    shape = 1,
    alpha = 1/2,
    color = this_color
    ) +
  geom_point(color = this_color, position = pn, size = 2) +
  geom_errorbar(
    aes(ymin = ll, ymax = ul), 
    width = .2,
    color = this_color,
    position = pn,
    alpha = 1/2
    ) +
  geom_line(color = this_color, position = pn, alpha = 1/2) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Year", 
    y = "Mean Pelvic Pain Outcome (0-100 VAS)", 
    caption = "95%CI error bars."
    ) +
  #scale_color_manual(values = ghibli_palettes$PonyoDark[2]) +
  theme_classic()
pp_outcome_plot

# plots side by side
reg_res_plot_2 <- pp_outcome_plot | peta2_plot | beta_plot
reg_res_plot_2

# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/reg-res-plot-v3.svg",
#   plot = reg_res_plot_2,
#   width = 5.5,
#   height = 4.5,
#   units = "in"
#   )

reg_res_plot_3 <- pp_outcome_plot | peta2_plot
reg_res_plot_3

# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/reg-res-plot-v4.svg",
#   plot = reg_res_plot_3,
#   width = 5.5,
#   height = 4.5,
#   units = "in"
#   )

####################
#                  #
# Sensory analysis #
#                  #
####################

# see script "sensory-analysis.R" for original work

# Calculating pelvic pain outcome variable as an average of:
# 1) urination_pain_last_week
# 2) bowel_mov_pain_last_week
# 3) mens_nonmens_pain_week

# # narrows data
# pelvic_pain_data <- 
#   complete_extra_annual_data %>% 
#   select(
#     ss, 
#     year, 
#     timestamp, 
#     days_from_baseline, 
#     urination_pain_last_week, 
#     bowel_mov_pain_last_week, 
#     mens_nonmens_pain_week
#   )
# 
# # long-format
# pelvic_pain_data_long <- 
#   pelvic_pain_data %>%
#   select(-timestamp) %>%
#   pivot_longer(cols = c(-ss, -year, -days_from_baseline))
# 
# # narrow down the participants in PCA results
# # PCA subject numbers
# pca_ss <- as.numeric(rownames(pca_res$Fixed.Data$ExPosition.Data$fi)) 
# 
# # retains only those in PCA
# pelvic_pain_data_pca_ss <- pelvic_pain_data %>% filter(ss %in% pca_ss)
# length(unique(pelvic_pain_data_pca_ss$ss)) == length(pca_ss) # same ss
# 
# ###############
# #             #
# # AVERAGE VAS #
# #             #
# ###############
# 
# pelvic_pain_data_pca_ss %>% filter(!complete.cases(.))
# 
# # computes average here
# pelvic_pain_avg <- 
#   pelvic_pain_data_pca_ss %>%
#   filter(complete.cases(.)) %>% # removes those subjects with missing data
#   pivot_longer(c(-ss, -year, -timestamp, -days_from_baseline)) %>%
#   group_by(ss, year, timestamp, days_from_baseline) %>%
#   summarise(pelvic_pain = mean(value), n = n()) %>%
#   ungroup()
# 
# pelvic_pain_avg %>% filter(n != 3) # all have three observations/timepoint
# pelvic_pain_avg %>% filter(is.na(pelvic_pain)) # no missing here

##############
#            #
# Predictors #
#            #
##############

load("../output/pca-data-all.rda") # pca data

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
dis_plot_vars <- c("qst_z", "bladder_z", "supra_z")
dist_plot <-
  ggplot(
    sensory_data_z_long %>% filter(name %in% dis_plot_vars), 
    aes(name, value, color = name)
    ) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(position = position_nudge(x = .3), width = .2) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = "Predictors", y = "Z-Score") +
  scale_color_manual(values = ghibli_palettes$MononokeMedium[c(4:6)]) + 
  theme_classic() +
  theme(legend.position = "none")
dist_plot

# PCA distributions
pca_mod_z <- 
  pelvic_pain_avg_fi_wide %>%
  # z-scores all meas
  mutate(across(.cols = c(year_0, V1:V40), .fns = ~as.numeric(scale(.x))))
apply(pca_mod_z, 2, mean) # double checks that everything this z scored
apply(pca_mod_z, 2, sd) # double checks that everything this z scored

# long-format
pca_mod_z_long <- 
  pca_mod_z %>% 
  select(ss, year_0, V1, V2, V3) %>% 
  pivot_longer(-ss) %>%
  mutate(name = fct_relevel(name, c("year_0", "V1", "V2", "V3")))

pj <- position_jitter(width = .1)
pca_dist_vars <- c("V1", "V2", "V3")
pca_dist_plot <-
  ggplot(
    pca_mod_z_long %>% filter(name %in% pca_dist_vars), 
    aes(name, value, color = name)
    ) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(position = position_nudge(x = .3), width = .2) +
  coord_cartesian(ylim = c(-5, 5)) +
  labs(x = "Predictors", y = "Z-Score") +
  scale_color_manual(values = ghibli_palettes$PonyoMedium[c(3, 5, 7)]) +
  theme_classic() +
  theme(legend.position = "none")
pca_dist_plot

# Pelvic Pain Outcome Plot (VERSION 2)
this_color <- ghibli_palettes$PonyoMedium[2]
pj <- position_jitter(width = .1, height = 0)
pn <- position_nudge(x = .3)
pp_outcome_plot_v2 <- 
  ggplot(pelvic_pain_avg_ss %>% filter(year < 5), aes(factor(year), pelvic_pain, group = year)) +
  geom_point(position = pj, alpha = 1/3, color = this_color) +
  geom_boxplot(position = pn, width = .2, color = this_color) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Year", 
    y = "Mean Pelvic Pain Outcome (0-100 VAS)", 
  ) +
  theme_classic()
pp_outcome_plot_v2


# Models
year0_mod <- lm(year0 ~ 1 + supra_mc + bladder_mc + qst_mc, data = sensory_data)
summary(year0_mod)
check_model(year0_mod)

year1_mod <- lm(year1 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year1_mod)
check_model(year1_mod)
check_heteroscedasticity(year1_mod)
check_normality(year1_mod)
check_outliers(year1_mod)

year2_mod <- lm(year2 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year2_mod)
check_model(year2_mod)
check_heteroscedasticity(year2_mod)
check_normality(year2_mod)
check_outliers(year2_mod)


year3_mod <- lm(year3 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year3_mod)
check_model(year3_mod)
check_heteroscedasticity(year3_mod)
check_normality(year3_mod)
check_outliers(year3_mod)

year4_mod <- lm(year4 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year4_mod)
check_model(year4_mod)
check_heteroscedasticity(year4_mod)
check_normality(year4_mod)
check_outliers(year4_mod)

year5_mod <- lm(year5 ~ 1 + year0_mc + supra_mc + bladder_mc + qst_mc, data = sensory_data) 
summary(year5_mod)
check_model(year5_mod)
check_heteroscedasticity(year5_mod)
check_normality(year5_mod)
check_outliers(year5_mod)

# Zero order correlations
# Computes bootstrapped correlations (seed was set earlier in script)
sens_zero_order_cors <-
  psych::corr.test(
    sensory_data %>% select(year0_mc, supra_mc, bladder_mc, qst_mc),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )
sens_zero_order_cors$ci # the results

# correlation results as tibble for plotting
sens_cor_res <- 
  as_tibble(sens_zero_order_cors$ci, rownames = "var") %>%
  separate(var, into = c("var1", "var2"), sep = "-") %>%
  mutate(var2 = fct_relevel(var2, c("qst_mc", "bladder_mc", "supra_mc")))

# plots correlation results
sens_cor_plot <-
  ggplot(sens_cor_res %>% filter(var1 == "year0_mc"), aes(var2, r, color = var2)) +
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
sens_cor_plot

# CORRELATIONS PLOT
# organizes pca cor res
pca_cor_res <- 
  zero_order_cors$ci %>% 
  as_tibble(rownames = "var") %>%
  separate(var, into = c("var1", "var2"), sep = "-")

# combines both correlation plots together 
cor_res <-
  bind_rows(
  pca_cor_res %>% filter(var1 == "year_0"), 
  sens_cor_res %>% filter(var1 == "year0_mc")
  ) %>%
  mutate(
    var2 = fct_relevel(
      var2, 
      c("qst_mc", "bladder_mc", "supra_mc", "V1", "V2", "V3")
      )
    )

# plots correlation results
cor_plot <-
  ggplot(cor_res, aes(var2, r, color = var2)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  scale_color_manual(
    values = c(
      ghibli_palettes$MononokeMedium[c(4,5,6)], 
      ghibli_palettes$PonyoMedium[c(3, 5, 7)]
      )
    ) +
  labs(
    x = "Predictors", 
    y = "r (correlation with baseline pelvic pain)", 
    #caption = "95% CI error bars."
  ) +
  theme_classic() +
  theme(legend.position = "none")
cor_plot





# models in a list
sens_mods <- list(year1_mod, year2_mod, year3_mod, year4_mod, year5_mod)

# sample sizes
sens_mods %>%
  map("model") %>%
  map_dfr(~tibble(n = nrow(.x)), .id = "mod")

# computes the partial eta squareds with CI
sens_mods_peta2 <-
  sens_mods %>%
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
sens_mods_ests <- 
  sens_mods %>% 
  map_dfr(~broom::tidy(.x), .id = "dv_year") %>% 
  rename(Parameter = term) # better for join

# computes standardized regression coefficients with CI
sens_mods_beta <- 
  sens_mods %>% 
  map_dfr(~as_tibble(standardize_parameters(.x)), .id = "dv_year") %>%
  select(-CI) %>%
  rename(beta = Std_Coefficient, beta_CI_low = CI_low, beta_CI_high = CI_high)

# calculates the sums of squares
sens_mods_ss <- 
  sens_mods %>%
  map_dfr(~as_tibble(car::Anova(.x, type = 3), rownames = "Parameter"), .id = "dv_year") %>%
  rename(dfn = Df, SS = `Sum Sq`, F = `F value`, p = `Pr(>F)`)

# reorganizes for the residuals (error)
sens_mods_ss_resids <- 
  sens_mods_ss %>% 
  filter(Parameter == "Residuals") %>%
  rename(SSE = SS, dfd = dfn) %>%
  mutate(MSE = SSE / dfd) %>%
  select(-F, -p, -Parameter)

# includes correct SS with residuals
sens_mods_ss_final <- 
  sens_mods_ss %>% 
  filter(Parameter != "Residuals") %>% 
  left_join(., mods_ss_resids, by = "dv_year")

# combines into one big table
sens_mods_res <-
  sens_mods_ests %>% 
  left_join(., sens_mods_beta, by = c("dv_year", "Parameter")) %>%
  left_join(., sens_mods_peta2, by = c("dv_year", "Parameter")) %>%
  left_join(., sens_mods_ss_final, by = c("dv_year", "Parameter")) %>%
  mutate(Parameter = gsub("[\\(\\)]", "", Parameter)) %>%
  mutate(sig = p.value < .05, Parameter = as.factor(Parameter)) %>%
  mutate(
    Parameter = fct_relevel(
      Parameter, 
      c("Intercept", "year0_mc", "qst_mc", "bladder_mc", "supra_mc")
    )
  )

# write out model results for manuscript table
# uncomment to save out
#write_csv(sens_mods_res, file = "../output/sens-mods-res.csv")


# PARTIAL ETA SQUARED PLOT
pd <- position_dodge(width = .4)
sens_peta2_plot <- 
  ggplot(
    sens_mods_res %>% filter(Parameter != "Intercept", dv_year < 5), 
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
  scale_color_manual(
    values = c(
      ghibli_palettes$PonyoMedium[c(2)], 
      ghibli_palettes$MononokeMedium[c(4:6)]
      )
    ) +
  labs(
    x = "Year", 
    y = "Partial Eta^2", 
    #caption = "95% CI error bars."
  ) +
  theme_classic() +
  coord_cartesian(ylim = c(0, .25)) +
  theme(legend.position = "none")
sens_peta2_plot #plots

# BETA PLOT
sens_beta_plot <- 
  ggplot(
    sens_mods_res %>% filter(Parameter != "Intercept", dv_year < 5), #  
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
sens_beta_plot #plots

# 6 panel figure
model_res_figure <-
  (dist_plot + pca_dist_plot) / 
  (pp_outcome_plot_v2 + cor_plot) / 
  (sens_peta2_plot + peta2_plot)
model_res_figure

# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/model-res-figure-v3.svg",
#   plot = model_res_figure,
#   width = 6,
#   height = 6,
#   units = "in"
#   )

# Saving out data for sensitivity analysis (see sensitivity-analysis.R)
sensitivity_analysis_data <- 
  pelvic_pain_avg_fi_wide %>% left_join(., sensory_data, by = "ss")

# comment out to save out
# save(sensitivity_analysis_data, file = "../output/sensitivity-analysis-data.rda")

# Plotting MMH Quartiles and Pelvic Pain Outcome
mmh_data <- 
  pelvic_pain_avg_fi_wide %>%
  select(ss, year_0:year_4, MMH = V1)

mmh_quants <- quantile(mmh_data$MMH, probs = c(.25, .5, .75))

mmh_data_grouped <-
  mmh_data %>%
  mutate(
    quant = case_when(
      MMH < mmh_quants[1] ~ "< 25%",
      MMH >= mmh_quants[1] & MMH <= mmh_quants[3]  ~ "25-75%",
      MMH > mmh_quants[3] ~ "> 75%"
    )
    )

pj <- position_jitter(width = .1, height = 0)
pn <- position_nudge(x = .2)
mmh_boxplot <- 
  ggplot(mmh_data_grouped, aes(x = "MMH", y = MMH, group = 1, color = quant)) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(position = pn, width = .1) +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(ylim = c(-11, 11)) +
  theme_classic() +
  theme(legend.position = "none")
mmh_boxplot

mmh_data_ss <- 
  mmh_data_grouped %>%
  pivot_longer(cols = c(-ss, -quant, -MMH), names_to = "year") %>%
  filter(complete.cases(value)) %>%
  mutate(year = as.numeric(regmatches(year, regexpr("\\d", year)))) 

mmh_data_sum <-
  mmh_data_ss %>%
  group_by(quant, year) %>%
  summarise(
    m = mean(value), 
    n = n(), 
    sd = sd(value), 
    sem = sd/sqrt(n)
    ) %>%
  ungroup()

pj <- position_jitter(width = .1, y = 0)
pn <- position_nudge(x = .2)
mmh_quants_plot <- 
  ggplot(mmh_data_sum, aes(year, m, group = quant, color = quant)) +
  geom_point(data = mmh_data_ss, aes(y = value), alpha = 1/3, position = pj) +
  geom_errorbar(aes(ymin = m-sem, ymax = m+sem), width = .1, position = pn) +
  geom_point(position = pn) +
  geom_line(position = pn) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = "Year", 
    y = "Pelvic Pain Outcome (0-100 VAS)",
    caption = "SEM error bars."
    ) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
mmh_quants_plot

# mmh plot
mmh_plot <- mmh_boxplot + mmh_quants_plot
mmh_plot

# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/mmh-plot-v1.svg",
#   plot = mmh_plot,
#   width = 4.5,
#   height = 4,
#   units = "in"
#   )



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

######################
#                    #
# OLD CODE | ARCHIVE #
#                    #
######################
# 
# #################################################
# #                                               #
# # computing z-scores based on baseline m and sd #
# #                                               #
# #################################################
# 
# # computing the z-scores
# year_0 <- pelvic_pain_data_pca_ss %>% filter(complete.cases(.), year == 0)
# 
# # Mean & SD at Year 0 for urination pain
# m_urination <- mean(year_0$urination_pain_last_week)
# sd_urination <- sd(year_0$urination_pain_last_week)
# # Mean & SD at Year 0 for bowel mov pain
# m_bowel <- mean(year_0$bowel_mov_pain_last_week)
# sd_bowel <- sd(year_0$bowel_mov_pain_last_week)
# # Mean & SD at Year 0 for mens and nonmens pain
# m_mens_nonmens <- mean(year_0$mens_nonmens_pain_week)
# sd_mens_nonmens <- sd(year_0$mens_nonmens_pain_week)
# 
# # computes z scores, which scales the other measures by m and sd of BASELINE
# # the combined z score is "pelvic_pain"
# pelvic_pain_data_pca_ss_new_z <- 
#   pelvic_pain_data_pca_ss %>% 
#   filter(complete.cases(.)) %>%
#   mutate(
#     z_urination_pain_last_week = (urination_pain_last_week - m_urination)/sd_urination,
#     z_bowel_mov_pain_last_week = (bowel_mov_pain_last_week - m_bowel)/sd_bowel,
#     z_mens_nonmens_pain_week = (mens_nonmens_pain_week - m_mens_nonmens)/sd_mens_nonmens,
#     pelvic_pain = z_urination_pain_last_week+z_bowel_mov_pain_last_week+z_mens_nonmens_pain_week
#   )
# 
# # histogram
# ggplot(pelvic_pain_data_pca_ss_new_z, aes(pelvic_pain)) +
#   geom_histogram(binwidth = .5) +
#   facet_wrap(~year)
# 
# # density plot
# ggplot(
#   pelvic_pain_data_pca_ss_new_z, 
#   aes(pelvic_pain, group = year, color = factor(year))
# ) +
#   geom_density()
# 
# # longitudinal plot 1
# pd <- position_dodge(width = .1)
# ggplot(pelvic_pain_data_pca_ss_new_z, aes(year, pelvic_pain)) +
#   geom_point(position = pd, alpha = 1/3, aes(group = ss)) +
#   geom_path(position = pd, alpha = 1/3, aes(group = ss)) +
#   geom_smooth(method = "lm", se = FALSE, color = "red")
# 
# # longitudinal plot 2
# ggplot(pelvic_pain_data_pca_ss_new_z, aes(year, pelvic_pain)) +
#   geom_point(position = pd, alpha = 1/3, aes(group = ss)) +
#   geom_line(
#     stat ="smooth", 
#     method = "lm", 
#     formula = y ~ x,
#     alpha = 1/3,
#     aes(group = ss)
#   ) +
#   coord_cartesian(ylim = c(-5, 15))
# 
# # Longitudinal plot 3 (with groups)
# # calculates groups here
# baseline_measures <- 
#   complete_extra_annual_data %>% filter(year == 0) %>%
#   left_join(., ss_codes %>% select(ss, group), by = "ss") %>%
#   mutate(
#     dys_status = ifelse(cramp_pain_no_nsaid >= 50, "DYS", "NODYS"),
#     cpp_status = case_when(
#       group %in% c("PBS", "PAIN") ~ "CPP",
#       group %in% c("HC", "DYSB", "DYS") ~ "NOCPP",
#       TRUE ~ as.character(group)
#     )
#   ) %>%
#   relocate(c(group, dys_status, cpp_status), .before = year)
# 
# # combines groups with plot data
# long_data_with_groups <- 
#   pelvic_pain_data_pca_ss_new_z %>%
#   left_join(
#     ., 
#     select(
#       baseline_measures, 
#       ss, 
#       group, 
#       dys_status, 
#       cpp_status, 
#       bs_cramp_pain_no_nsaid = cramp_pain_no_nsaid
#     ),
#     by = "ss"
#   ) 
# 
# # longitudinal plot 3
# # DYS STATUS
# ggplot(long_data_with_groups, aes(year, pelvic_pain)) +
#   geom_point(position = pd, alpha = 1/3, aes(group = ss, color = dys_status)) +
#   geom_line(
#     stat ="smooth", 
#     method = "lm", 
#     formula = y ~ x,
#     alpha = 1/3,
#     aes(group = ss, color = dys_status)
#   ) +
#   geom_line(
#     size = 1.5,
#     stat ="smooth", 
#     method = "lm", 
#     formula = y ~ x,
#     aes(group = dys_status, color = dys_status)
#   ) +
#   coord_cartesian(ylim = c(-5, 15)) +
#   scale_color_brewer(palette = "Set2") +
#   theme_classic()
# 
# # CPP STATUS
# ggplot(long_data_with_groups, aes(year, pelvic_pain)) +
#   geom_point(position = pd, alpha = 1/3, aes(group = ss, color = cpp_status)) +
#   geom_line(
#     stat ="smooth", 
#     method = "lm", 
#     formula = y ~ x,
#     alpha = 1/3,
#     aes(group = ss, color = cpp_status)
#   ) +
#   geom_line(
#     size = 1.5,
#     stat ="smooth", 
#     method = "lm", 
#     formula = y ~ x,
#     aes(group = cpp_status, color = cpp_status)
#   ) +
#   coord_cartesian(ylim = c(-5, 15)) +
#   scale_color_brewer(palette = "Set1") +
#   theme_classic()
# 
# 
# # new data with corrected z
# fi_pp_newz <- 
#   pelvic_pain_data_pca_ss_new_z %>% 
#   left_join(., fi, by = "ss") %>%
#   group_by(ss) %>%
#   mutate(timepoints = n()) %>% # calculates timepoints
#   ungroup()
# 
# # Q1 MODEL
# q1_mod_newz <- 
#   lm(pelvic_pain ~ 1 + V1 + V2 + V3, data = fi_pp_newz %>% filter(year == 0))
# summary(q1_mod_newz)     # regression results
# performance(q1_mod_newz) # looks at performance metrics
# check_model(q1_mod_newz) # checks assumptions
# 
# # Data for lmer - includes participants with at least 2 timepoints for slopes
# lmer_data_newz <- fi_pp_newz %>% filter(timepoints > 1) # greater than 1 tp
# length(unique(lmer_data_newz$ss)) # n=171, 29 lost to follow-up
# 
# # Maximal model - checking to see if random intercepts and slopes converges
# max_mod_newz <- 
#   lmer(
#     pelvic_pain ~ 1 + year + (1 + year | ss), 
#     data = lmer_data_newz, 
#     REML = TRUE
#   )
# summary(max_mod_newz)
# performance(max_mod_newz)
# check_model(max_mod_newz)
# 
# min_mod_newz <- 
#   lmer(
#     pelvic_pain ~ 1 + year + (1 | ss), 
#     data = lmer_data_newz, 
#     REML = TRUE
#   )
# summary(min_mod_newz)
# performance(min_mod_newz)
# check_model(min_mod_newz)
# 
# anova(min_mod_newz, max_mod_newz) # best to model random slopes
# 
# # Full model - includes additional fixed effects
# full_mod_newz <- 
#   lmer(
#     pelvic_pain ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
#     data = lmer_data_newz, 
#     REML = FALSE
#   )
# summary(full_mod_newz) # summary 
# performance(full_mod_newz) # model performance
# check_model(full_mod_newz) # checks assumptions
# anova(max_mod_newz, full_mod_newz) # compares against model without moderators
# 
# # plots the observed and fitted slopes for each subject
# full_mod_newz_aug <- augment(full_mod_newz)
# min(full_mod_newz_aug$pelvic_pain)
# max(full_mod_newz_aug$pelvic_pain)
# these_ss <- unique(full_mod_newz_aug$ss)
# 
# # my own function to plot fitted and observed slopes
# my_cowplot <-
#   function(x = 1:25){
#     ggplot(full_mod_newz_aug %>% filter(ss %in% these_ss[x]), aes(year, pelvic_pain)) +
#       geom_point() +
#       geom_smooth(method = "lm", se = FALSE, color = "black") +
#       geom_line(aes(y = .fitted), linetype = 3, size = 1, color = rdgy_pal[3]) +
#       facet_wrap(~ss) +
#       coord_cartesian(ylim = c(-2, 11)) +
#       theme_minimal()  
#   }
# 
# # Uncommnet to see these plots
# # my_cowplot(1:25)
# # my_cowplot(26:50)
# # my_cowplot(51:75)
# # my_cowplot(76:100)
# # my_cowplot(101:125)
# # my_cowplot(126:150)
# # my_cowplot(151:171)
# 
# ##########################
# #                        #
# # Examining Interactions #
# #                        #
# ##########################
# 
# # Principal component 1
# # simple slopes
# v1_simp_slopes <- 
#   interact_plot(
#     full_mod_newz, 
#     pred = year, 
#     modx = V1, 
#     plot.points = TRUE, 
#     interval = TRUE
#   )
# v1_simp_slopes
# 
# # my attempt
# pj <- position_jitter(width = .2, height = 0)
# ggplot(v1_simp_slopes$data, aes(year, pelvic_pain, group = modx_group)) +
#   geom_point(data = full_mod_newz_aug, aes(group = 1), alpha = 1/3, position = pj) +
#   geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 1/3) +
#   geom_line(aes(linetype = modx_group)) +
#   scale_linetype_manual(values = c(1,2,1)) +
#   theme_classic() +
#   theme(legend.position = "bottom")
# 
# # Johnson-Neyman plot
# sim_slopes(full_mod_newz, pred = year, modx = V1, jnplot = TRUE)
# 
# # V3 
# interact_plot(full_mod_newz, pred = year, modx = V3, plot.points = TRUE, interval = TRUE)
# sim_slopes(full_mod_newz, pred = year, modx = V3, jnplot = TRUE)
# 
# # TRYING LESS TIME POINTS
# # Data for lmer
# lmer_data_newz_2year <- 
#   fi_pp_newz %>% 
#   filter(year %in% c(0:2)) %>% # years 0 1 2 (up to 2 year follow-up)
#   group_by(ss) %>%
#   mutate(timepoints = n()) %>% # recalculates timepoints
#   ungroup()
# lmer_data_newz_2year_incl <- lmer_data_newz_2year %>% filter(timepoints>1)
# length(unique(lmer_data_newz_2year_incl$ss)) 
# 
# # Maximal model - checking to see if random intercepts and slopes converges
# max_mod_newz_2year <- 
#   lmer(
#     pelvic_pain ~ 1 + year + (1 + year | ss), 
#     data = lmer_data_newz_2year_incl, 
#     REML = TRUE
#   )
# summary(max_mod_newz_2year)
# performance(max_mod_newz_2year)
# check_model(max_mod_newz_2year)
# 
# min_mod_newz_2year <- 
#   lmer(
#     pelvic_pain ~ 1 + year + (1 | ss), 
#     data = lmer_data_newz_2year, 
#     REML = TRUE
#   )
# summary(min_mod_newz_2year)
# performance(min_mod_newz_2year)
# check_model(min_mod_newz_2year)
# 
# anova(min_mod_newz, max_mod_newz) # best to model random slopes
# 
# # Full model - includes additional fixed effects
# full_mod_newz_2year <- 
#   lmer(
#     pelvic_pain ~ 1 + year + year*V1 + year*V2 + year*V3 + (1 + year | ss), 
#     data = lmer_data_newz_2year_incl, 
#     REML = FALSE
#   )
# summary(full_mod_newz_2year) # summary 
# performance(full_mod_newz_2year) # model performance
# check_model(full_mod_newz_2year) # checks assumptions
# anova(max_mod_newz, full_mod_newz) # compares against model without moderators
# 
# # Modeling whether QST provides any insight to pelvic pain development over and 
# # above standard measures
# 
# one_year_follow <-
#   fi_pp_newz %>%
#   filter(year %in% c(0,2)) %>% # switch this to 2 for 2 year outcome  
#   group_by(ss) %>%
#   mutate(timepoints = n()) %>%
#   ungroup() %>%
#   filter(timepoints>1) %>%
#   select(ss, year, pelvic_pain, timepoints, V1, V2, V3)
# 
# one_year_follow_wide <- 
#   one_year_follow %>%
#   pivot_wider(
#     id_cols = ss, 
#     values_from = pelvic_pain, 
#     names_from = year, 
#     names_prefix = "pp_"
#   ) %>%
#   left_join(., fi, by = "ss") %>%
#   filter(complete.cases(.))
# 
# one_year_mod <- lm(pp_2 ~ 1 + pp_0 + V1 + V2 + V3, data = one_year_follow_wide)
# summary(one_year_mod)




