# PCA Results visualization
# Matt Kmiecik
# Started 21 June 2021

# Purpose: to visualize the results from PCA (pca-proc.R)

source("r-prep.R")              # Prepares R workspace
load("../output/mmh-res.RData") # Loads results
load("../output/pca-data-all.rda") # Loads PCA data kept and discarded

#########################################
#                                       #
# Descriptive Stats of Variables in PCA #
#                                       #
#########################################

# descriptive table
pca_data_desc <- 
  pca_data_keep %>%
  # converting reverse coded variables back into regular units
  mutate(
    across(
      .cols = c(starts_with("ppt_N_"), "cpm_lknee", "ts_max"), 
      .fns = ~.x*-1
      )
    ) %>%
  pivot_longer(cols = -ss) %>%
  group_by(name) %>%
  summarise(
    M = mean(value),
    LL =  quantile(value, .025),
    UL =  quantile(value, .975),
    SD = sd(value),
    n = n(),
    SEM = SD/sqrt(n),
    Min = min(value),
    Max = max(value)
  ) %>%
  ungroup()

# Saves out for manuscript table
# uncomment to save out
# write_csv(pca_data_desc, file = "../output/pca-data-desc.csv")

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
#   filename = "scree-figure-v1.svg",
#   path = "../output/",
#   width = 4, height = 3, units = "in"
# )


#########################
#                       #                 
# SPLIT-HALF RESAMPLING #
#                       #
#########################
# Heavily inspired by: https://github.com/derekbeaton/Workshops/tree/master/RTC/Apr2017/SplitHalf

# Calculates correlations between split have row factor scores (fi)
sh_cor <-
  1:iters %>%
  map(~abs(diag(cor(shrs_res$first[[.x]]$ExPosition.Data$fi, shrs_res$second[[.x]]$ExPosition.Data$fi)))) %>%
  map_dfr(~as_tibble(.x), .id = "iter") %>%
  group_by(iter) %>%
  mutate(comp = 1:n()) %>%
  ungroup() %>%
  rename(cor = value)

sh_cor_sum <- 
  sh_cor %>%
  group_by(comp) %>%
  summarise(
    m = mean(cor),
    sd = sd(cor),
    n = n(),
    med = median(cor),
    ll = quantile(cor, .025),
    ul = quantile(cor, .975)
  )

pj <- position_jitter(width = .1, height = 0)
ggplot(sh_cor %>% filter(comp<6), aes(comp, cor)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_point(data = sh_cor_sum %>% filter(comp<6), aes(comp, m), color = "red") +
  geom_errorbar(data = sh_cor_sum %>% filter(comp<6), aes(comp, m, ymin = ll, ymax = ul), color = "red", width = .2) +
  theme_classic()


ggplot(sh_cor_sum, aes(comp, m)) +
  geom_point(stat = "identity") +
  geom_point(aes(x = comp, y = med), shape = 17) +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0, alpha = 1/3) +
  geom_line(aes(group = 1)) +
  labs(x = "Component", y = "Mean Correlation", caption  = "95% CI error bars; triangles = median.") +
  theme_minimal()

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
  mutate(sig = abs(bsr) > critical_val) # calculates significance (think like t-test)

# Bootstrapped Results
# COMPONENT 1
this_comp <- 1
this_data <- boot_res_long %>% filter(comp == this_comp) %>% arrange(bsr)
axisFace <- ifelse(this_data$sig == TRUE, "bold", "plain")
comp_1_bsr_plot <-
  ggplot(this_data, aes(bsr, reorder(meas, bsr), fill = sig)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[3])) +
  geom_vline(xintercept = c(-critical_val, critical_val), linetype = 2, alpha = 1.3) +
  scale_x_continuous(breaks = seq(-8, 8, 2), minor_breaks = NULL, limits = c(-9, 9)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))
comp_1_bsr_plot

# COMPONENT 2
this_comp <- 2
this_data <- boot_res_long %>% filter(comp == this_comp) %>% arrange(bsr)
axisFace <- ifelse(this_data$sig == TRUE, "bold", "plain")
comp_2_bsr_plot <- 
  ggplot(this_data, aes(bsr, reorder(meas, bsr), fill = sig)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
  geom_vline(xintercept = c(-critical_val, critical_val), linetype = 2, alpha = 1.3) +
  scale_x_continuous(breaks = seq(-8, 8, 2), minor_breaks = TRUE, limits = c(-9, 9)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))
comp_2_bsr_plot

# COMPONENT 3
this_comp <- 3
this_data <- boot_res_long %>% filter(comp == this_comp) %>% arrange(bsr)
axisFace <- ifelse(this_data$sig == TRUE, "bold", "plain")
comp_3_bsr_plot <- 
  ggplot(this_data, aes(bsr, reorder(meas, bsr), fill = sig)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
  geom_vline(xintercept = c(-critical_val, critical_val), linetype = 2, alpha = 1.3) +
  scale_x_continuous(breaks = seq(-8, 8, 2), minor_breaks = TRUE, limits = c(-9, 9)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))
comp_3_bsr_plot

# all three for manuscript
bsr_plot <- comp_1_bsr_plot + comp_2_bsr_plot + comp_3_bsr_plot
bsr_plot
# Saves out for manuscript
# uncomment to save out
# ggsave(
#   filename = "../output/bsr-plot.svg",
#   plot = bsr_plot,
#   width = 8,
#   height = 7.5,
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
    theme(legend.position = "bottom")
  
  return(this_plot)

}

# Plot
fj_plot_comps_1_2 <- fj_bsr_plot(x = c(1, 2))
fj_plot_comps_1_2 # plots

fj_plot_comps_2_3 <- fj_bsr_plot(x = c(2, 3))
fj_plot_comps_2_3 # plots

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


##############
#            #
# ROW SCORES #
#            #
##############
load("../output/ss-codes.RData")

# gathers row scores and adds group
fi <- 
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss)) %>%
  left_join(., ss_codes %>% select(ss, group), by = "ss") %>%
  select(ss, group, V1:V40)

# Caluclates barycenters
fi_sum_long <- 
  fi %>%
  pivot_longer(cols = c(-ss, -group), names_to = "comp", values_to = "fs") %>%
  group_by(group, comp) %>%
  summarise(m = mean(fs), n = n()) %>%
  ungroup() %>%
  arrange(comp)

# Converts to wide format for easier plotting
fi_sum_wide <- 
  fi_sum_long %>%
  pivot_wider(id_cols = group, names_from = comp, values_from = m)

# FI PLOT
ggplot(fi_sum_wide, aes(V1, V2, color = group)) +
  pca_furnish +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point(data = fi, alpha = 1/3, shape = 16) +
  geom_point(shape = 17, size = 5) +
  geom_text_repel(aes(label = group), segment.alpha = 0, show.legend = FALSE) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  scale_color_manual(values = wes_palettes$Darjeeling2) +
  theme(legend.position = "none")

# Looking at factor plots side-by-side

# FI PLOT
fi_plot <- 
  ggplot(fi_sum_wide, aes(V1, V2, color = group)) +
  pca_furnish +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point(data = fi, alpha = 1/3, shape = 16) +
  geom_point(shape = 17, size = 5) +
  geom_text_repel(aes(label = group), segment.alpha = 0, show.legend = FALSE) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  scale_color_manual(values = wes_palettes$Darjeeling2) +
  theme(legend.position = "none")

fj_plot <- fj_bsr_plot(x = c(1, 2))

fi_plot + fj_plot

###############################
#                             #
# Coloring by external scores #
#                             #
###############################

load("../output/extra-data.RData") # loads extra PCA data

# Trimming data
extra_data_match <- 
  extra_pca_data %>% filter(ss %in% fi$ss) # includes only pca ss

# Z-score procedure
# everything is continuous meas/can be z scored except for ibs and ibs_sub
extra_data_match_cont <- 
  extra_data_match %>%
  select(-ibs, -ibs_sub) # filters out categorical ibs and ibs_sub
  
# Z scoring and organization here
extra_data_match_cont_scaled <- 
  as_tibble(
    scale(extra_data_match_cont %>% select(-ss), center = TRUE, scale = TRUE) # z-score calc
    ) %>%
  mutate(ss = extra_data_match_cont$ss) %>% # adds back ss number
  select(ss, bsi:promis_depression) # reorders cols

# proof that these are z-scores
apply(extra_data_match_cont_scaled, 2, function(x){mean(x, na.rm = TRUE)}) # should be 0
apply(extra_data_match_cont_scaled, 2, function(x){sd(x, na.rm = TRUE)}) # should be 1

# FI PLOT with colors
fi_extra <- left_join(fi, extra_data_match_cont_scaled, by = "ss") # adds z scores

ggplot(fi_extra, aes(V1, V2, color = promis_depression)) +
  pca_furnish +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point(shape = 19, size = 2) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  scale_color_gradientn(
    colours = brewer.pal(n = 11, "BrBG"),
    limits = c(-3, 3),
    breaks = c(-3, 0, 3), labels = c(-3, 0, 3),
    guide = "colourbar",
    name = "Z-Score"
  ) 

# plot here with the ROME groups

# Correlations between extra measures and PCA components
# Prepares correlation data
cor_data <- 
  fi_extra %>%
  select(
    ss,
    V1:V3, # PCs 1-3
    cmsi_3mos_lastyear, cmsi_3mos_lifetime, # cmsi
    mh23, # menstrual pain
    bsi, # somatic symptoms
    gupi_pain_subscale:gupi_gupi_total, # gupi
    icsi, icpi, # ic
    gph, # global physical health
    gmh, # global mental health
    promis_pb, # promis pain behavior
    promis_pi,# promis pain interference
    promis_anxiety, # promis anxiety
    promis_depression # promis depression
    )

set.seed(14) # sets seed for reproducible boostrapping

# Computes bootstrapped correlations
cor_res <-
  psych::corr.test(
    cor_data %>% select(-ss),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )
# round(cor_res$ci,3)

# Cleans up the confidence interval results
cor_ci <- 
  as_tibble(cor_res$ci, rownames = "vars") %>%
  separate(vars, into = c("var1", "var2"), sep = "-") %>%
  mutate(sig = ifelse(p < .05, "sig", "nsig"))

# Correlations plots
ggplot(
  cor_ci %>% filter(var1 %in% c("V1", "V2", "V3")), 
  aes(r, var2, color = sig)
  ) +
  geom_point(size = 2) +
  scale_color_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .2) +
  coord_cartesian(xlim = c(-1, 1)) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = seq(-1, 1, .5), minor_breaks = NULL) + 
  labs(x = "Correlation (r)", y = "Variable 2", caption = "95% CI error bars.") +
  facet_wrap(~var1) +
  theme_minimal() +
  theme(legend.position = "none")

# Correlations plots for manuscript
comps <- c("V1", "V2", "V3")
cor_plot <- 
  ggplot(
    cor_ci %>% filter(var1 %in% comps, var2 %nin% comps), 
    aes(r, var2, color = sig)
    ) +
    geom_point(size = 2) +
    scale_color_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = .2) +
    coord_cartesian(xlim = c(-1, 1)) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_x_continuous(breaks = seq(-1, 1, .5), minor_breaks = NULL) + 
    labs(x = "Correlation (r)", y = "Variable 2", caption = "95% CI error bars.") +
    facet_wrap(~var1) +
    theme_minimal() +
    theme(legend.position = "none")
cor_plot # to view

# Saves out for manuscript
# uncomment to save out
# ggsave(
#   plot = cor_plot,
#   filename = "../output/cor-plot-v2.svg",
#   width = 6.5,
#   height = 5,
#   units = "in"
#   )

# Descriptive stats on the extra variables
cor_data_reg <- 
  extra_data_match %>% 
  select(-starts_with("ibs"), -mh23a, -starts_with("global")) %>%
  left_join(., fi %>% select(ss, V1, V2, V3), by = "ss")
  
# Computes bootstrapped correlations
cor_res_reg <-
  psych::corr.test(
    cor_data_reg %>% select(-ss),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )

# Cleans up the confidence interval results
# Proof that cor plot is the same with z scores
cor_ci_reg <- 
  as_tibble(cor_res_reg$ci, rownames = "vars") %>%
  separate(vars, into = c("var1", "var2"), sep = "-") %>%
  mutate(sig = ifelse(p < .05, "sig", "nsig"))

# Correlations plots
ggplot(
  cor_ci_reg %>% filter(var2 %in% c("V1", "V2", "V3")), 
  aes(r, var1, color = sig)
) +
  geom_point(size = 2) +
  scale_color_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .2) +
  coord_cartesian(xlim = c(-1, 1)) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = seq(-1, 1, .5), minor_breaks = NULL) + 
  labs(x = "Correlation (r)", y = "Variable 2", caption = "95% CI error bars.") +
  facet_wrap(~var2) +
  theme_minimal() +
  theme(legend.position = "none")

# Descriptive statistics on variables entered into correlations
cor_data_reg_desc <- 
  cor_data_reg %>% 
  select(-V1, -V2, -V3) %>% 
  pivot_longer(-ss) %>%
  group_by(name) %>%
  summarise(
    M = mean(value, na.rm = TRUE),
    LL = quantile(value, .025, na.rm = TRUE),
    UL = quantile(value, .975, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    n_start = n(),
    n_na = sum(is.na(value)),
    n_final = n_start-n_na, # true n after na's are dropped
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  ) %>%
  ungroup()

# saves out for manuscript table
# uncomment to save out
#write_csv(cor_data_reg_desc, file = "../output/cor-data-reg-desc.csv")

# Preparing IBS data for Sarah ----
ibs_data <- 
  extra_data_match %>% 
  left_join(., fi, by = "ss") %>%
  relocate(ss, group, ibs, ibs_sub, V1, V2, V3)
#write_csv(ibs_data, file = "../output/ibs-data.csv") # writes out to csv  
