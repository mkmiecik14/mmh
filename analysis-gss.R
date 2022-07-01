# GSS Analysis
# Matt Kmiecik
# Started 20 June 2022

#* Purpose: create and analyze the GSS variable and compare it to MMH

# Prep workspace
source("r-prep.R")

# Loads data
load("../output/gss-data-long.rda.") # long format (still need to calc GSS)
load("../output/gss-brief-data.rda") # the gss brief format
load("../output/mmh-res.RData") # pca res
load("../output/ss-codes.RData") # participant codes

# gets factor scores and ss for later
fi <- 
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss)) %>%
  left_join(., ss_codes %>% select(ss, group), by = "ss") %>%
  select(ss, group, V1:V40)

# calculates GSS here and is in wide format
gss_data_wide <-
  gss_data_long %>%
  filter(ss %in% fi$ss) %>%
  pivot_wider(id_cols = ss, names_from = "sub_q", values_from = "sum") %>%
  mutate(
    across(
      .cols = c(pain_sites, sa, sens),
      .fns = ~as.numeric(scale(.x)), # calculation of z scores here
      .names = "{.col}_z"),
    gss = pain_sites_z+sa_z+sens_z # sum of z scores to create "GSS"
    )

# long format after GSS calculation
gss_long <- gss_data_wide %>% pivot_longer(-ss)

# Histogram
ggplot(
  gss_long %>% filter(name %in% c("pain_sites", "sa", "sens")), 
  aes(value)
  ) +
  geom_histogram(binwidth = 1) +
  labs(x = "Sum", y = "Frequency") +
  theme_bw() +
  facet_wrap(~name)

ggplot(
  gss_long %>% filter(name %in% c("pain_sites_z", "sa_z", "sens_z", "gss")), 
  aes(value)
) +
  geom_histogram(binwidth = 1) +
  labs(x = "Sum", y = "Frequency") +
  theme_bw() +
  facet_wrap(~name)

ggplot(gss_brief_data %>% filter(ss %in% fi$ss), aes(gss_brief)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL) 

# BOXPLOTs
ggplot(
  gss_long %>% filter(name %in% c("pain_sites", "sa", "sens")), 
  aes(name, value, fill = name)) +
  geom_boxplot() +
  labs(x = "GSS Component", y = "Sum") +
  theme_bw() +
  scale_fill_manual(values = c(ghibli_palettes$KikiLight[3:5])) +
  theme(legend.position = "none")

# BOXPLOTs
ggplot(
  gss_long %>% filter(name %in% c("pain_sites_z", "sa_z", "sens_z", "gss")), 
  aes(name, value, fill = name)) +
  geom_boxplot() +
  labs(x = "GSS Component", y = "Z-Score") +
  theme_bw() +
  scale_fill_manual(values = c(ghibli_palettes$MononokeLight[3:6])) +
  theme(legend.position = "none")

ggplot(gss_brief_data %>% filter(ss %in% fi$ss), aes("GSS Brief", y = gss_brief)) + 
  geom_boxplot(width = .2)

# Brings in pelvic pain data - - - -
load("../output/complete-extra-annual-data.Rdata") # annual data

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

############################
#                          #
# AVERAGED PELVIC PAIN VAS #
#                          #
############################

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

mod_data <- 
  pelvic_pain_avg %>%
  pivot_wider(
    id_cols = ss, 
    names_from = year, 
    names_prefix = "year", 
    values_from = pelvic_pain
    ) %>%
  left_join(., gss_data_wide, by = "ss") %>%
  left_join(., fi %>% select(ss, V1:V3), by = "ss") %>%
  left_join(., gss_brief_data %>% select(ss, gss_brief), by = "ss") %>%
  mutate(
    across(
      .cols = c(year0, gss, V1, gss_brief), 
      .fns = ~as.numeric(scale(.x, scale = FALSE)),
      .names = "{.col}_mc"
      )
    )

####################
#                  #
# GSS via Z-SCORES #
#                  #
####################

# Decided not to go this route due to correspondence with Andrew Schrepf
# He recommended to use the GSS Brief, that is highly associated with GSS

# # Models
# year0_mod <- lm(year0 ~ 1 + gss_mc + V1_mc, data = mod_data)
# summary(year0_mod)
# check_model(year0_mod)
# 
# year1_mod <- lm(year1 ~ 1 + year0_mc + gss_mc + V1_mc, data = mod_data)
# summary(year1_mod)
# check_model(year1_mod)
# 
# year2_mod <- lm(year2 ~ 1 + year0_mc + gss_mc + V1_mc, data = mod_data)
# summary(year2_mod)
# check_model(year2_mod)
# 
# year3_mod <- lm(year3 ~ 1 + year0_mc + gss_mc + V1_mc, data = mod_data)
# summary(year3_mod)
# check_model(year3_mod)
# 
# year4_mod <- lm(year4 ~ 1 + year0_mc + gss_mc + V1_mc, data = mod_data)
# summary(year4_mod)
# check_model(year4_mod)
# 
# # Zero order correlations
# set.seed(14) # sets seed for reproducible boostrapping
# 
# # Computes bootstrapped correlations
# zero_order_cors <-
#   psych::corr.test(
#     mod_data %>% select(year0_mc, gss_mc, V1_mc),
#     use = "pairwise",
#     method = "pearson", 
#     adjust = "none",
#     ci = TRUE,
#     minlength = 100 # extends the abrreviations
#   )
# zero_order_cors$ci # the results
# 
# # correlation results as tibble for plotting
# cor_res <- 
#   as_tibble(zero_order_cors$ci, rownames = "var") %>%
#   separate(var, into = c("var1", "var2"), sep = "-") %>%
#   mutate(var2 = fct_relevel(var2, c("gss_mc", "V1_mc")))
# 
# cor_plot <-
#   ggplot(cor_res %>% filter(var1 == "year0_mc"), aes(var2, r)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
#   coord_cartesian(ylim = c(-1, 1)) +
#   geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
#   labs(
#     x = "Predictors", 
#     y = "r (correlation with baseline pelvic pain)", 
#     caption = "95% CI error bars."
#   ) +
#   theme_classic() +
#   theme(legend.position = "none")
# cor_plot
# 
# # models in a list
# mods <- list(year1_mod, year2_mod, year3_mod, year4_mod)
# gss_mods <- mods # for saving out and naming consistency
# #save(gss_mods, file = "../output/gss-mods.rda") # uncomment out to save
# 
# # sample sizes
# mods %>%
#   map("model") %>%
#   map_dfr(~tibble(n = nrow(.x)), .id = "mod")
# 
# # computes the partial eta squareds with CI
# mods_peta2 <-
#   mods %>%
#   map(
#     ~eta_squared(
#       car::Anova(.x, type = 3),
#       partial = TRUE, 
#       generalized = FALSE, 
#       ci = .95, 
#       alternative = "two.sided",
#       include_intercept = TRUE
#     )
#   ) %>%
#   map_dfr(~as_tibble(.x), .id = "dv_year") %>%
#   select(-CI) %>%
#   rename(Eta2_partial_CI_low = CI_low, Eta2_partial_CI_high = CI_high)
# 
# # computes model estimates
# mods_ests <- 
#   mods %>% 
#   map_dfr(~broom::tidy(.x), .id = "dv_year") %>% 
#   rename(Parameter = term) # better for join
# 
# # computes standardized regression coefficients with CI
# mods_beta <- 
#   mods %>% 
#   map_dfr(~as_tibble(standardize_parameters(.x)), .id = "dv_year") %>%
#   select(-CI) %>%
#   rename(beta = Std_Coefficient, beta_CI_low = CI_low, beta_CI_high = CI_high)
# 
# # calculates the sums of squares
# mods_ss <- 
#   mods %>%
#   map_dfr(~as_tibble(car::Anova(.x, type = 3), rownames = "Parameter"), .id = "dv_year") %>%
#   rename(dfn = Df, SS = `Sum Sq`, F = `F value`, p = `Pr(>F)`)
# 
# # reorganizes for the residuals (error)
# mods_ss_resids <- 
#   mods_ss %>% 
#   filter(Parameter == "Residuals") %>%
#   rename(SSE = SS, dfd = dfn) %>%
#   mutate(MSE = SSE / dfd) %>%
#   select(-F, -p, -Parameter)
# 
# # includes correct SS with residuals
# mods_ss_final <- 
#   mods_ss %>% 
#   filter(Parameter != "Residuals") %>% 
#   left_join(., mods_ss_resids, by = "dv_year")
# 
# # combines into one big table
# mods_res <-
#   mods_ests %>% 
#   left_join(., mods_beta, by = c("dv_year", "Parameter")) %>%
#   left_join(., mods_peta2, by = c("dv_year", "Parameter")) %>%
#   left_join(., mods_ss_final, by = c("dv_year", "Parameter")) %>%
#   mutate(Parameter = gsub("[\\(\\)]", "", Parameter)) %>%
#   mutate(sig = p.value < .05, Parameter = as.factor(Parameter)) %>%
#   mutate(
#     Parameter = fct_relevel(
#       Parameter, 
#       c("Intercept", "year0_mc", "gss_mc", "V1_mc")
#     )
#   )
# 
# # PARTIAL ETA SQUARED PLOT
# this_colors <- 
#   c(
#     ghibli_palettes$MononokeMedium[3], 
#     ghibli_palettes$KikiMedium[4],
#     ghibli_palettes$PonyoMedium[3]
#     )
# pd <- position_dodge(width = .4)
# peta2_plot <- 
#   ggplot(
#     mods_res %>% filter(Parameter != "Intercept", dv_year < 5), 
#     aes(dv_year, Eta2_partial, group = Parameter, color = Parameter, shape = sig)
#   ) +
#   geom_line(position = pd, alpha = 1/2) +
#   geom_point(position = pd, size = 3) +
#   scale_color_manual(values = this_colors) +
#   scale_shape_manual(values = c(1, 16)) +
#   labs(
#     x = "Year", 
#     y = "Partial Eta^2",
#   ) +
#   theme_classic() +
#   theme(legend.position = "bottom")
# peta2_plot #plots
# 
# # BETA PLOT
# beta_plot <- 
#   ggplot(
#     mods_res %>% filter(Parameter != "Intercept", dv_year < 5), #  
#     aes(dv_year, beta, group = Parameter, color = Parameter)
#   ) +
#   geom_line(position = pd, alpha = 1/2) +
#   geom_point(aes(shape = sig), position = pd, size = 3) +
#   scale_shape_manual(values = c(1, 16)) +
#   scale_color_manual(values = this_colors) +
#   labs(
#     x = "Year", 
#     y = "Beta"
#   ) +
#   coord_cartesian(ylim = c(-.5, .75)) +
#   geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
#   theme_classic() +
#   theme(legend.position = "bottom")
# beta_plot #plots
# 
# # plots side by side
# reg_res_plot <- peta2_plot | beta_plot
# reg_res_plot

####################
#                  #
# GSS BRIEF        #
#                  #
####################

mod_data %>%
  filter(complete.cases(gss_brief)) %>%
  summarise(
    m = mean(gss_brief),
    sd = sd(gss_brief),
    n = n(),
    min = min(gss_brief),
    max = max(gss_brief)
    )

# Models
year0_mod <- lm(year0 ~ 1 + gss_brief_mc + V1_mc, data = mod_data)
summary(year0_mod)
check_model(year0_mod)

year1_mod <- lm(year1 ~ 1 + year0_mc + gss_brief_mc + V1_mc, data = mod_data)
summary(year1_mod)
check_model(year1_mod)

year2_mod <- lm(year2 ~ 1 + year0_mc + gss_brief_mc + V1_mc, data = mod_data)
summary(year2_mod)
check_model(year2_mod)

year3_mod <- lm(year3 ~ 1 + year0_mc + gss_brief_mc + V1_mc, data = mod_data)
summary(year3_mod)
check_model(year3_mod)

year4_mod <- lm(year4 ~ 1 + year0_mc + gss_brief_mc + V1_mc, data = mod_data)
summary(year4_mod)
check_model(year4_mod)

# Zero order correlations
set.seed(14) # sets seed for reproducible boostrapping

# Computes bootstrapped correlations
zero_order_cors <-
  psych::corr.test(
    mod_data %>% select(year0_mc, gss_mc, V1_mc),
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
  mutate(var2 = fct_relevel(var2, c("gss_mc", "V1_mc")))

# calculates VIF across the mods
mods %>% map(~car::vif(.x))

cor_plot <-
  ggplot(cor_res %>% filter(var1 == "year0_mc"), aes(var2, r)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  labs(
    x = "Predictors", 
    y = "r (correlation with baseline pelvic pain)", 
    caption = "95% CI error bars."
  ) +
  theme_classic() +
  theme(legend.position = "none")
cor_plot

# models in a list
mods <- list(year1_mod, year2_mod, year3_mod, year4_mod)
gss_mods <- mods # for saving out and naming consistency
#save(gss_mods, file = "../output/gss-mods.rda") # uncomment out to save

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
      c("Intercept", "year0_mc", "gss_mc", "V1_mc")
    )
  )
# saves out for publication table
# uncomment to save out
#write_csv(mods_res, file = "../output/gss-brief-model-res.csv")

# PARTIAL ETA SQUARED PLOT
this_colors <- 
  c(
    ghibli_palettes$MononokeMedium[3], 
    ghibli_palettes$KikiMedium[4],
    ghibli_palettes$PonyoMedium[3]
  )
pd <- position_dodge(width = .4)
peta2_plot <- 
  ggplot(
    mods_res %>% filter(Parameter != "Intercept", dv_year < 5), 
    aes(dv_year, Eta2_partial, group = Parameter, color = Parameter, shape = sig)
  ) +
  geom_line(position = pd, alpha = 1/2) +
  geom_point(position = pd, size = 3) +
  scale_color_manual(values = this_colors) +
  scale_shape_manual(values = c(1, 16)) +
  labs(
    x = "Year", 
    y = "Partial Eta^2",
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
  geom_line(position = pd, alpha = 1/2) +
  geom_point(aes(shape = sig), position = pd, size = 3) +
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = this_colors) +
  labs(
    x = "Year", 
    y = "Beta"
  ) +
  coord_cartesian(ylim = c(-.5, .75)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  theme_classic() +
  theme(legend.position = "bottom")
beta_plot #plots

# plots side by side
reg_res_plot <- peta2_plot | beta_plot
reg_res_plot


