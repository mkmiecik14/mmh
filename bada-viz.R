# BADA Results visualization
# Matt Kmiecik
# Started 8 July 2021

# Purpose: to visualize the results from PCA (pca-proc.R)

source("r-prep.R")              # Prepares R workspace
load("../output/mmh-res.RData") # Loads results

# Switch if you want to view bada_res_2
this_bada_res <- bada_res_2 # bada_res_2

#########
#       #
# SCREE #
#       #
#########

scree_data <- 
  tibble(obs_eigs = this_bada_res$Fixed.Data$TExPosition.Data$eigs) %>%
  mutate(
    obs_sv = sqrt(obs_eigs), # observed singular values 
    perc = this_bada_res$Fixed.Data$TExPosition.Data$t, # percentage var explained
    comp = 1:n(),
    p = this_bada_res$Inference.Data$components$p.vals,
    sig = ifelse(p < .05, "sig", "ns")
  )

# SCREE PLOT
ggplot(scree_data, aes(comp, perc, color = sig)) +
  geom_line(linetype = 2, color = rdgy_pal[8]) +
  geom_point() +
  labs(x = "Component", y = "Percentage Variance Explained", caption = "sig = p<.05") +
  scale_color_manual(values = c(rdgy_pal[9], rdgy_pal[3])) +
  theme_classic() +
  theme(legend.position = "bottom")

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
fj <- as_tibble(this_bada_res$Fixed.Data$TExPosition.Data$fj, rownames = "meas")

# Component plots
ggplot(fj, aes(V1, V2)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  #coord_cartesian(xlim = c(-.1, .1), ylim = c(-.1, .1)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  pca_furnish

# Computing bootstrapping results
critical_val <- 2 # treat like a z score 
boot_res_long <- 
  as_tibble(this_bada_res$Inference.Data$boot.data$fj.boot.data$tests$boot.ratios, rownames = "meas") %>%
  pivot_longer(-meas, values_to = "bsr", names_to = "comp") %>%
  mutate(comp = as.numeric(gsub("V", "", comp))) %>%
  arrange(comp) %>%
  mutate(sig = abs(bsr) > critical_val) # calculates significance (think like t-test)

# Bootstrapped Results
this_comp <- 2 # component to plot
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

# Attempting to draw 95% confidence ellipsis around groups
fi_boots <- 
  array_tree(this_bada_res$Inference.Data$boot.data$fi.boot.data$boots, margin = 3) %>%
  map_dfr(~as_tibble(.x, rownames = "group"), .id = "iter")

ggplot(fi_boots, aes(V1, V2, color = group, fill = group)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point(alpha = 1/3) +
  stat_ellipse(size = 2, geom = "polygon", alpha = 1/2, level = .95) +
  pca_furnish

# no points
this_pal <- wes_palettes$Darjeeling2
ggplot(fi_boots, aes(V1, V2, color = group, fill = group)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  #geom_point(alpha = 1/3) +
  stat_ellipse(size = 2, geom = "polygon", alpha = 1/2, level = .95) +
  scale_color_manual(values = this_pal) +
  scale_fill_manual(values = this_pal) +
  pca_furnish +
  theme(legend.position = "bottom")

# uncomment when saving out figure
ggsave(
  filename = "bada-res.svg",
  path = "../output/",
  height = 4.5,
  width = 6,
  units = "in"
)
