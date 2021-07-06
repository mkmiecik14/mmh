# PCA Results visualization
# Matt Kmiecik
# Started 21 June 2021

# Purpose: to visualize the results from PCA (pca-proc.R)

source("r-prep.R")              # Prepares R workspace
load("../output/mmh-res.RData") # Loads results

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

# SCREE PLOT
ggplot(scree_data, aes(comp, perc, color = sig)) +
  geom_line(linetype = 2, color = rdgy_pal[8]) +
  geom_point() +
  labs(x = "Component", y = "Percentage Variance Explained", caption = "sig = p<.001") +
  scale_color_manual(values = c(rdgy_pal[9], rdgy_pal[3])) +
  theme_classic() +
  theme(legend.position = "bottom")


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
ggplot(fj, aes(V2, V3)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  pca_furnish

# Computing bootstrapping results
critical_val <- 2 # treat like a z score 
boot_res_long <- 
  as_tibble(pca_res$Inference.Data$fj.boots$tests$boot.ratios, rownames = "meas") %>%
  pivot_longer(-meas, values_to = "bsr", names_to = "comp") %>%
  mutate(comp = as.numeric(gsub("V", "", comp))) %>%
  arrange(comp) %>%
  mutate(sig = abs(bsr) > critical_val) # calculates significance (think like t-test)

# Bootstrapped Results
this_comp <- 3
this_data <- boot_res_long %>% filter(comp == this_comp) %>% arrange(bsr)
axisFace <- ifelse(this_data$sig == TRUE, "bold", "plain")
ggplot(this_data, aes(bsr, reorder(meas, bsr), fill = sig)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[8], rdgy_pal[3])) +
  geom_vline(xintercept = c(-critical_val, critical_val), linetype = 2, alpha = 1.3) +
  coord_cartesian(xlim = c(-10, 10)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))

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
     .data[[comp1]] == TRUE & .data[[comp2]] == TRUE ~ "both",
     .data[[comp1]] == FALSE & .data[[comp2]] == FALSE ~ "none",
     .data[[comp1]] == TRUE & .data[[comp2]] == FALSE ~ comp1,
     .data[[comp1]] == FALSE & .data[[comp2]] == TRUE ~ comp2
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
comps_to_plot <- c(2, 3) # only include 2
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
  )
# Plot
ggplot(
  data_to_plot, 
  aes(
    .data[[paste0("fs_", comps_to_plot[1])]], 
    .data[[paste0("fs_", comps_to_plot[2])]],
    color = sig
    )
  ) +
  pca_furnish +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE, max.overlaps = 15) +
  scale_color_manual(values = rev(ghibli_palettes$MononokeMedium)) + 
  theme(legend.position = "bottom")


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
  select(ss, group, V1:V41)

ggplot(fi, aes(V1, V2, color = group)) +
  pca_furnish +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  #geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium) +
  theme(legend.position = "bottom")

# next step is to calculate the barycenters
  

