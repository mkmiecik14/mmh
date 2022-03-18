# Sensitivity Analysis
# Matt Kmiecik
# 16 March 2022

#* Purpose: to perform a sensitivity analysis to demonstrate that shifting the
#* participant groups to proportions normally seen in the literature

source("r-prep.R") # Prepares R workspace

# Loading data
load("../output/complete-extra-annual-data.Rdata") # annual data
load("../output/mmh-res.RData") # PCA results
load("../output/ss-codes.RData") # subject codes
load("../output/sensitivity-analysis-data.rda") # data for sensitivity analysis

# Setting up parallel processing
# https://byuistats.github.io/M335/parallel_furrr.html
library(furrr)
no_cores <- availableCores() - 1 # gets number of cores on computer - 1
plan(multicore, workers = no_cores)

# Data prep
sens_data_all <- 
  sensitivity_analysis_data %>%
  left_join(., ss_codes %>% select(ss, group), by = "ss") %>%
  # converts DYSB -> DYS for prevalence analysis
  mutate(group2 = ifelse(group == "DYSB", "DYS", group)) %>% 
  relocate(c(group, group2), .after = ss)

# selects only data that we need
fi <- 
  sens_data_all %>% 
  select(ss, group2, year_0:year_4, V1:V3, supra, bladder, qst)

# Prevalence rates
# HC
hc_prop <- .5 # 50%
hc_n <- hc_prop * 200
# DYS
dys_prop <- .4 # 40%
dys_n <- dys_prop * 200
# Chronic pain
pain_prop <- .05 # 5%
pain_n <- pain_prop * 200
# PBS
pbs_prop <- .05 # 5%
pbs_n <- pbs_prop * 200

# Current prevalence rates
prev_rates <- 
  fi %>% 
  count(group2, name = "current_n") %>% 
  mutate(current_prop = current_n/200) %>%
  # calculates simulation props and n
  mutate(
    sim_prop = c(.4, .5, .05, .05),
    sim_n = sim_prop*200
    )

# Data sets
hc_data   <- fi %>% filter(group2 == "HC")
dys_data  <- fi %>% filter(group2 == "DYS")
pain_data <- fi %>% filter(group2 == "PAIN")
pbs_data  <- fi %>% filter(group2 == "PBS")

#################
#               #
# BOOTSTRAPPING #
#               #
#################

# Sets the seed for reproducible results
set.seed(1218)

# iterations
iters <- 1:2000

# bootstraps each dataset according to length required (i.e., prevalence)
hc_boot <- 
  iters %>%
  map_dfr(
    ~tibble(
      ss = sample(hc_data$ss, size = hc_n, replace = TRUE)
      ) %>% 
      left_join(., fi, by = "ss"),
    .id = "iter"
    )

dys_boot <- 
  iters %>%
  map_dfr(
    ~tibble(
      ss = sample(dys_data$ss, size = dys_n, replace = TRUE)
    ) %>% 
      left_join(., fi, by = "ss"),
    .id = "iter"
  )

pain_boot <- 
  iters %>%
  map_dfr(
    ~tibble(
      ss = sample(pain_data$ss, size = pain_n, replace = TRUE)
    ) %>% 
      left_join(., fi, by = "ss"),
    .id = "iter"
  )

pbs_boot <- 
  iters %>%
  map_dfr(
    ~tibble(
      ss = sample(pbs_data$ss, size = pbs_n, replace = TRUE)
    ) %>% 
      left_join(., fi, by = "ss"),
    .id = "iter"
  )

# bootstrapped data (combines from bootstrapping)
boot_data <- 
  bind_rows(dys_boot, hc_boot, pain_boot, pbs_boot) %>%
  mutate(iter = as.numeric(iter))

# Unstandardized regressions
boot_data_mods <- 
  boot_data %>%
  group_by(iter) %>%
  # mean centers baseline PP (year_0) and all the PCs and constructs
  mutate(
    across(
      .cols = c(year_0, V1, V2, V3, supra, bladder, qst), 
      .fns = ~as.numeric(scale(.x, scale=FALSE))
      )
    ) %>%
  ungroup() %>%
  nest_by(iter) %>%
  mutate(
    pca_mod_y1 = list(lm(year_1 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    pca_mod_y2 = list(lm(year_2 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    pca_mod_y3 = list(lm(year_3 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    pca_mod_y4 = list(lm(year_4 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    sens_mod_y1 = list(lm(year_1 ~ 1 + year_0 + qst + bladder + supra, data = data)),
    sens_mod_y2 = list(lm(year_2 ~ 1 + year_0 + qst + bladder + supra, data = data)),
    sens_mod_y3 = list(lm(year_3 ~ 1 + year_0 + qst + bladder + supra, data = data)),
    sens_mod_y4 = list(lm(year_1 ~ 1 + year_0 + qst + bladder + supra, data = data))
    ) %>%
  group_by(iter) %>%
  mutate(
    pca_mods = list(c(pca_mod_y1, pca_mod_y2, pca_mod_y3, pca_mod_y4)),
    sens_mods = list(c(sens_mod_y1, sens_mod_y2, sens_mod_y3, sens_mod_y3, sens_mod_y4))
  ) %>%
  ungroup()

# Standardized regressions
boot_data_mods_z <- 
  boot_data %>%
  group_by(iter) %>%
  # z-scores parameters to get standardized reg coefs (betas)
  mutate(
    across(
      .cols = c(year_0, year_1, year_2, year_3, year_4, V1, V2, V3, supra, bladder, qst), 
      .fns = ~as.numeric(scale(.x))
    )
  ) %>%
  ungroup() %>%
  nest_by(iter) %>%
  mutate(
    pca_mod_y1 = list(lm(year_1 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    pca_mod_y2 = list(lm(year_2 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    pca_mod_y3 = list(lm(year_3 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    pca_mod_y4 = list(lm(year_4 ~ 1 + year_0 + V1 + V2 + V3, data = data)),
    sens_mod_y1 = list(lm(year_1 ~ 1 + year_0 + qst + bladder + supra, data = data)),
    sens_mod_y2 = list(lm(year_2 ~ 1 + year_0 + qst + bladder + supra, data = data)),
    sens_mod_y3 = list(lm(year_3 ~ 1 + year_0 + qst + bladder + supra, data = data)),
    sens_mod_y4 = list(lm(year_1 ~ 1 + year_0 + qst + bladder + supra, data = data))
  ) %>%
  group_by(iter) %>%
  mutate(
    pca_mods = list(c(pca_mod_y1, pca_mod_y2, pca_mod_y3, pca_mod_y4)),
    sens_mods = list(c(sens_mod_y1, sens_mod_y2, sens_mod_y3, sens_mod_y3, sens_mod_y4))
  ) %>%
  ungroup()

# Initializations
mod_names <- 
  c("pca_mod_y1", "pca_mod_y2", "pca_mod_y3", "pca_mod_y4",
    "sens_mod_y1", "sens_mod_y2", "sens_mod_y3", "sens_mod_y3", "sens_mod_y4" 
    )
est_storage <- vector(mode = "list", length = length(mod_names))
peta_storage <- est_storage
beta_storage <- est_storage

# unstandardized estimates
for (i in 1:length(mod_names)){
  est_storage[[i]] <-
    boot_data_mods %>% 
      split(.$iter) %>%
      map(mod_names[i]) %>%
      map(1) %>%
      map_dfr(~broom::tidy(.x), .id = "iter") %>%
      mutate(model = mod_names[i])
}
# collapses into one df
ests <- est_storage %>% map_dfr(~.x)

# partial eta-squared
# computes the partial eta squareds with CI
for (i in 1:length(mod_names)){
  peta_storage[[i]] <-
    boot_data_mods %>%
      split(.$iter) %>%
      map(mod_names[i]) %>%
      map(1) %>%
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
      map_dfr(~as_tibble(.x), .id = "iter") %>%
      mutate(model = mod_names[i])
}
# collapses into one df
petas <- peta_storage %>% map_dfr(~.x)

# betas
for (i in 1:length(mod_names)){
  beta_storage[[i]] <-
    boot_data_mods_z %>% 
    split(.$iter) %>%
    map(mod_names[i]) %>%
    map(1) %>%
    map_dfr(~broom::tidy(.x), .id = "iter") %>%
    mutate(model = mod_names[i])
}
# collapses into one df
betas <- beta_storage %>% map_dfr(~.x)

# Saving out results ----
save(
  boot_data_mods, boot_data_mods_z, ests, petas, betas,
  file = "../output/sensitivity-analysis-results.rda"
  )
