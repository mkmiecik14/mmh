# PCA 2 Results visualization
# Matt Kmiecik
# Started 16 SEPTEMBER 2022

# Purpose: to visualize the results from PCA (pca-proc-2.R)

source("r-prep.R")              # Prepares R workspace
load("../output/pca-2-res.RData") # Loads results

#########
#       #
# SCREE #
#       #
#########

scree_data <- 
  tibble(obs_eigs = pca_res$Fixed.Data$ExPosition.Data$eigs) %>%
  mutate(
    obs_sv = sqrt(obs_eigs), # observed singular values 
    perc = pca_res$Fixed.Data$ExPosition.Data$t, # percentage var explained
    comp = 1:n(),
    p = pca_res$Inference.Data$components$p.vals,
    sig = ifelse(p < .001, "sig", "ns")
  )
# saving out data for manuscript
# uncomment to run
# write_csv(scree_data, file = "../output/scree-data.csv") # saves results

# SCREE PLOT
scree_plot <-
  ggplot(scree_data, aes(comp, perc, color = sig)) +
  geom_line(linetype = 2, color = rdgy_pal[8]) +
  geom_point() +
  labs(x = "Component", y = "Percentage Variance Explained", caption = "sig = p<.001") +
  scale_color_manual(values = c(rdgy_pal[9], rdgy_pal[3])) +
  theme_classic() +
  theme(legend.position = "bottom")
scree_plot # prints to screen

# saves out for manuscript 
# uncomment to save out
# ggsave(
#   plot = scree_plot,
#   filename = "scree-figure-v1.svg", EDIT FOR THIS SCRIPT IF YOU PLAN ON SAVING
#   path = "../output/",
#   width = 4, height = 3, units = "in"
# )


################
#              #
# FACTOR PLOTS #
#              #
################

# plot helper fx
pca_furnish <- 
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  )

# Columns (i.e., measures)
fj <- as_tibble(pca_res$Fixed.Data$ExPosition.Data$fj, rownames = "meas")

# Component plots
ggplot(fj, aes(V1, V2)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE, max.overlaps = 15) +
  pca_furnish

# Uncomment when saving out figure
# ggsave(
#   filename = "fs-plot-v2.svg",
#   path = "../output/",
#   width = 6.5, height = 4, units = "in"
# )


# Computing bootstrapping results
critical_val <- 2 # treat like a z score 
boot_res_long <- 
  as_tibble(pca_res$Inference.Data$fj.boots$tests$boot.ratios, rownames = "meas") %>%
  pivot_longer(-meas, values_to = "bsr", names_to = "comp") %>%
  mutate(comp = as.numeric(gsub("V", "", comp))) %>%
  arrange(comp) %>%
  mutate(sig = abs(bsr) > critical_val) %>% # calculates significance (think like t-test)
  mutate(
    meas_new = case_when(
      meas == "after_pain_12" ~ "AP 12",
      meas == "after_pain_5" ~ "AP 5",
      meas == "after_pain_6" ~ "AP 6",         
      meas == "after_pain_7" ~ "AP 7",     
      meas == "after_pain_forehead" ~ "AP Forehead",
      meas == "after_pain_rhip" ~ "AP Hip",
      meas == "after_pain_rknee" ~ "AP Knee",
      meas == "after_pain_rshoulder" ~ "AP Shoulder",
      meas == "aud_mean" ~ "Auditory Mean",             
      meas == "aud_slope" ~ "Auditory Slope",
      meas == "bs_pain" ~ "BS Pain",
      meas == "fs_urg" ~ "FS Urgency",
      meas == "fs_pain" ~ "FS Pain",
      meas == "fu_urg" ~ "FU Urgency", 
      meas == "fu_pain" ~ "FU Pain",
      meas == "mt_urg" ~ "MT Urgency",            
      meas == "mt_pain" ~ "MT Pain",
      meas == "coldpain_resid" ~ "Cold Pain",   
      meas == "cpm_lknee" ~ "CPM",
      meas == "ppt_N_rForehead" ~ "PPT Forehead",
      meas == "ppt_N_rHip" ~ "PPT Hip",
      meas == "ppt_N_rKnee" ~ "PPT Knee",
      meas == "ppt_N_rShoulder" ~ "PPT Shoulder", 
      meas == "ppt_N_12" ~ "PPT 12",   
      meas == "ppt_N_5" ~ "PPT 5",
      meas == "ppt_N_6" ~ "PPT 6",   
      meas == "ppt_N_7" ~ "PPT 7",
      meas == "bladder_sharp" ~ "Bl. Sharp",
      meas == "bladder_pressing" ~ "Bl. Pressing", 
      meas == "bladder_dull" ~ "Bl. Dull",
      meas == "bladder_prickling" ~ "Bl. Prickling",
      meas == "vag_sharp" ~ "Vag. Sharp",
      meas == "vag_pressing" ~ "Vag. Pressing",
      meas == "vag_dull" ~ "Vag. Dull",
      meas == "vag_prickling" ~ "Vag. Prickling",
      meas == "ts_mean" ~ "TS Mean",
      meas == "ts_slope" ~ "TS Slope",
      meas == "ts_max" ~ "TS Max",               
      meas == "vis_mean" ~ "Visual Mean",            
      meas == "vis_slope" ~ "Visual Slope"
    )
  )
  
# Bootstrapped Results
bsr_ggplot <- function(data, comp){
  this_comp <- comp
  this_data <- boot_res_long %>% filter(comp == this_comp) %>% arrange(bsr)
  axisFace <- ifelse(this_data$sig == TRUE, "bold", "plain")
  this_plot <-
    ggplot(this_data, aes(bsr, reorder(meas_new, bsr), fill = sig)) +
    geom_bar(stat = "identity") +
    labs(x = "Bootstrap Ratio", y = "Measure") +
    scale_fill_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
    geom_vline(xintercept = c(-critical_val, critical_val), linetype = 2, alpha = 1.3) +
    scale_x_continuous(breaks = seq(-8, 8, 2), minor_breaks = NULL, limits = c(-9, 9)) +
    theme_minimal() +
    facet_wrap(~comp) +
    theme(legend.position = "none", axis.text.y = element_text(face = axisFace))
  return(this_plot)
}

# all three for manuscript
bsr_plot <- 
  bsr_ggplot(boot_res_long, 1) + 
  bsr_ggplot(boot_res_long, 2) + 
  bsr_ggplot(boot_res_long, 3) 

bsr_plot
# Saves out for manuscript
# uncomment to save out
# ggsave(
#   filename = "../output/pca-2-bsr-plot.svg",
#   plot = bsr_plot,
#   width = 6,
#   height = 5.5,
#   units = "in"
#   )

# Combining factor score plots with coloring
fj_boot_sig <- 
  boot_res_long %>% 
  pivot_wider(id_cols = meas, names_from = comp, values_from = sig, names_prefix = "V")

# Component plots
comp1 <- "V1"
comp2 <- "V2"
fj_colors <-
  fj_boot_sig %>% 
  select(meas, comp1, comp2) %>%
  mutate(
    sig = case_when(
      .data[[comp1]] == TRUE  & .data[[comp2]] == TRUE  ~ "both",
      .data[[comp1]] == FALSE & .data[[comp2]] == FALSE ~ "none",
      .data[[comp1]] == TRUE  & .data[[comp2]] == FALSE ~ comp1,
      .data[[comp1]] == FALSE & .data[[comp2]] == TRUE  ~ comp2
    )
  )

ggplot(fj, aes(.data[[comp1]], .data[[comp2]])) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  pca_furnish


# Combining factor score plots with coloring
fj_long <- 
  fj %>% 
  pivot_longer(-meas, values_to = "fs", names_to = "comp") %>%
  mutate(comp = as.numeric(gsub("V", "", comp))) %>%
  arrange(comp)



# All the data needed for factor scores + bootstrap ratios
fj_data <- left_join(fj_long, boot_res_long, by = c("comp", "meas"))

# FACTOR SCORE PLOTS COLORED BY BSR
fj_bsr_plot <- function(x = c(1, 2)){
  
  # inserting categories
  meas_cats <- 
    tibble(meas = unique(fj_long$meas)) %>%
    mutate(
      cat =
        case_when(
          grepl("after_pain", meas) ~ "ppt_afterpain",
          grepl("aud", meas) ~ "auditory",
          grepl("vis", meas) ~ "vis",
          grepl("ppt", meas) ~ "ppt_threshold",
          grepl("bladder", meas) ~ "bladder_mcgill",
          grepl("vag", meas) ~ "vag_mcgill",
          grepl("bs", meas) ~ "bladder_task",
          grepl("fs", meas) ~ "bladder_task",
          grepl("fu", meas) ~ "bladder_task",
          grepl("mt", meas) ~ "bladder_task",
          grepl("bladder", meas) ~ "bladder",
          grepl("ts", meas) ~ "temporal_sum",
          grepl("cpm", meas) ~ "cpm",
          grepl("coldpain", meas) ~ "cold_pain"
        )
    )
  
  comps_to_plot <- x # only include 2
  data_to_plot <-
    fj_data %>% 
    filter(comp %in% comps_to_plot) %>%
    pivot_wider(id_cols = meas, names_from = comp, values_from = c(fs, bsr, sig)) %>%
    mutate(
      sig = case_when(
        .data[[paste0("sig_",comps_to_plot[1])]] == TRUE & .data[[paste0("sig_",comps_to_plot[2])]] == TRUE ~ "both",
        .data[[paste0("sig_",comps_to_plot[1])]] == FALSE & .data[[paste0("sig_",comps_to_plot[2])]] == FALSE ~ "none",
        .data[[paste0("sig_",comps_to_plot[1])]] == TRUE & .data[[paste0("sig_",comps_to_plot[2])]] == FALSE ~ as.character(comps_to_plot[1]),
        .data[[paste0("sig_",comps_to_plot[1])]] == FALSE & .data[[paste0("sig_",comps_to_plot[2])]] == TRUE ~ as.character(comps_to_plot[2])
      )
    ) %>%
    # joins categories
    left_join(., meas_cats, by = "meas")
  
  # Plot
  this_plot <-
    ggplot(
      data_to_plot, 
      aes(
        .data[[paste0("fs_", comps_to_plot[1])]], 
        .data[[paste0("fs_", comps_to_plot[2])]],
        color = cat,
        shape = sig
      )
    ) +
    pca_furnish +
    geom_vline(xintercept = 0, alpha = 1/3) +
    geom_hline(yintercept = 0, alpha = 1/3) +
    geom_point() +
    coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
    geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE, max.overlaps = 15) +
    scale_color_brewer(palette = "Paired") +
    scale_shape_manual(values = c(15, 17, 19, 1)) +
    theme(legend.position = "bottom") # switch this to "bottom" for legend
  
  return(this_plot)
  
}

# Plot
fj_plot_comps_1_2 <- fj_bsr_plot(x = c(1, 2))
fj_plot_comps_1_2 # plots

fj_plot_comps_2_3 <- fj_bsr_plot(x = c(2, 3))
fj_plot_comps_2_3 # plots

fj_plot_comps_1_3 <- fj_bsr_plot(x = c(1, 3))
fj_plot_comps_1_3  # plots

# Saves out for manuscript (uncomment to save)
# ggsave(
#   filename = "../output/fj-plot-comps-1-2.svg", 
#   plot = fj_plot_comps_1_2,
#   width = 5, 
#   height = 4, 
#   units = "in"
#   )
# 
# ggsave(
#   filename = "../output/fj-plot-comps-2-3.svg", 
#   plot = fj_plot_comps_2_3,
#   width = 5, 
#   height = 4, 
#   units = "in"
# )

# plots all three in one plot with room for legend
fj_1_2_3_plot <-
  (fj_plot_comps_1_2 + fj_plot_comps_1_3) / (fj_plot_comps_2_3 + plot_spacer())
fj_1_2_3_plot
# uncomment to save out
# ggsave(
#   filename = "../output/fj-plot-comps-1-2-3.svg",
#   plot = fj_1_2_3_plot,
#   width = 6,
#   height = 4.5,
#   units = "in"
# )

########################################
#                                      #
# PCA PREDICTION OF FUTURE PELVIC PAIN #
#                                      #
########################################

# Loading data
load("../output/complete-extra-annual-data.Rdata") # annual data

# Pelvic pain outcome is a measure combining:
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

# combines with PCA factors
# factor scores for the rows (subjects)
fi <-
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss))
pelvic_pain_avg_fi <- pelvic_pain_avg %>% left_join(., fi, by = "ss")

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

# mean centers columns
pelvic_pain_avg_fi_wide_mc <- 
  pelvic_pain_avg_fi_wide %>%
  # mean centers baseline PP (year_0) and all the components
  mutate(across(.cols = c(year_0, V1:V36), .fns = ~as.numeric(scale(.x, scale=FALSE))))

# Runs models
mod_y1 <- lm(year_1 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y2 <- lm(year_2 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y3 <- lm(year_3 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)
mod_y4 <- lm(year_4 ~ 1 + year_0 + V1 + V2 + V3, data = pelvic_pain_avg_fi_wide_mc)

# summaries
summary(mod_y1)
summary(mod_y2)
summary(mod_y3)
summary(mod_y4)

# Checks models
check_model(mod_y1)
check_model(mod_y2)
check_model(mod_y3)
check_model(mod_y4)

compare_performance(mod_y1, mod_y2, mod_y3, mod_y4, rank = TRUE)

# models in a list
mods <- list(mod_y1, mod_y2, mod_y3, mod_y4)

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
# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/pca-2-partial-etas.svg",
#   plot = peta2_plot,
#   width = 5,
#   height = 4.5,
#   units = "in"
#   )

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


