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
    comp = 1:n()
    )

# Permutation results
perm_res_2 <- 
  perm_res %>% 
  group_by(iter) %>% 
  mutate(comp = 1:n()) %>% # adds component numbers 
  ungroup() %>%
  mutate(eig = value^2) %>%
  rename(sv = value)

# Permutation p-values
perm_res_p <-
  perm_res_2 %>%
  left_join(., scree_data %>% select(comp, obs_eigs), by = "comp") %>%
  mutate(right = obs_eigs > eig) %>%
  group_by(comp) %>%
  summarise(
    right_sum = sum(right), # number of times observed eigs > permuted eigs
    p = ifelse(right_sum == iters, 1/iters, right_sum/iters), # calculates p
    p = ifelse(p != 0, p, 1), # fixes p values from 0 to 1
    sig = ifelse(p<.001, "sig", "ns")
    )

# Putting all scree data together
scree <- scree_data %>% left_join(., perm_res_p, by = "comp")

# SCREE PLOT
ggplot(scree, aes(comp, perc, color = sig)) +
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

pca_data    <- pca_res$Fixed.Data$ExPosition.Data$X # data submitted to PCA
first_fjs   <- shrs_res$first %>% map(~.x$v %*% diag(.x$d))   # first split-half
second_fjs  <- shrs_res$second %>% map(~.x$v %*% diag(.x$d))  # second split-half

# Holds the variance explained (i.e., preallocates)
shrs_hold <- 
  matrix(
    NA,
    nrow = length(first_fjs), # rows are the iterations
    ncol = length(pca_res$Fixed.Data$ExPosition.Data$eigs) # columns are the components
    )

# Calculates the squared correlation between components for each split-half
for(i in 1:length(first_fjs)){
  for(j in 1:ncol(pca_data)){
    # (must square the correlation because svd is arbitrary in direction)
    shrs_hold[i,j] <- cor(first_fjs[[i]][,j], second_fjs[[i]][,j])^2
  }
}

# Calculates summary of SHRS results
shrs_comp <- 
  as_tibble(shrs_hold) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(-iter, names_to = "comp", values_to = "r2") %>%
  mutate(
    comp = gsub("V", "", comp),
    comp = as.numeric(comp)
    ) 

# Calculates summary stats
shrs_r2 <- 
  shrs_comp %>%
  group_by(comp) %>%
  summarise(
    m = mean(r2),
    sd = sd(r2),
    n = n(),
    ll = as.numeric(t.test(r2, conf.level = 0.95)$conf.int[1]), # is not correct
    ul = as.numeric(t.test(r2, conf.level = 0.95)$conf.int[2]), # is not correct
    ll2 = quantile(r2, .025),
    ul2 = quantile(r2, .975)
  )

# Calculates r2
ggplot(shrs_r2, aes(comp, m)) +
  geom_point() +
  geom_errorbar(aes(ymin = ll2, ymax = ul2), width = .2) +
  geom_line() +
  labs(
    x = "Component",
    y = expression("Mean R"^2), 
    caption = "95% CI error bars."
    ) +
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(1, 10, 1), minor_breaks = NULL) +
  theme_minimal()

# Split-Half Resampling Plot
pj <- position_jitter(width = .2, height = 0)
pn <- position_nudge(x = .4, y = 0)
ggplot(shrs_comp %>% filter(comp<11), aes(comp, r2, group = comp)) +
  geom_point(alpha = 1/3, position = pj) +
  geom_point(
    data = shrs_r2 %>% filter(comp<11), 
    aes(y = m),
    position = pn
    ) +
  geom_errorbar(
    data = shrs_r2 %>% filter(comp<11), 
    aes(y = m, ymin = ll2, ymax = ul2), 
    width = .2, 
    position = pn
    ) +
  labs(
    x = "Principal Component", 
    y = expression("R"^2), 
    caption = "95% CI error bars."
    ) +
  scale_x_continuous(breaks = seq(1, 10, 1), minor_breaks = NULL) +
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
  coord_cartesian(xlim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  pca_furnish

# Computing bootstrapping results
boot_res_iters <- 
  boot_res %>% 
  map(~.x$v %*% diag(.x$d)) %>% # calculates fjs for each iteration
  map(~as.matrix(.x) %>% `rownames<-`(colnames(pca_data))) %>% # adds rownames for meas
  map_dfr(~as_tibble(.x, rownames = "meas"), .id = "iter")

boot_res_iters %>% filter(iter == 9) %>%
  ggplot(., aes(V1, V2)) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_point() +
  coord_cartesian(xlim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  pca_furnish

boot_res_long <- 
  boot_res_iters %>%
  pivot_longer(cols = c(-iter, -meas), names_to = "comp", values_to = "fs") %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

fj_obs <- 
  fj %>%
  pivot_longer(-meas, names_to = "comp", values_to = "fs_obs") %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

boot_res_meas <- 
  boot_res_long %>%
  mutate(fs_abs = abs(fs)) %>%
  group_by(meas, comp) %>%
  summarise(
    sd = sd(fs_abs),
    n = n(),
  ) %>%
  ungroup()

exp_boot <- 
  as_tibble(pca_res$Inference.Data$fj.boots$tests$boot.ratios, rownames = "meas") %>%
  pivot_longer(-meas, names_to = "comp", values_to = "bsr_exp") %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

# combined
critical_val <- 1.96 # treat like a z score 
boot_res_final <- 
  boot_res_meas %>%
  left_join(., fj_obs, by = c("comp", "meas")) %>%
  mutate(bsr = fs_obs/sd) %>% 
  left_join(., exp_boot, by = c("comp", "meas")) %>%
  mutate(
    sig_bsr = ifelse(abs(bsr) > critical_val, "p<.05", "p>.05"),
    sig_bsr_obs = ifelse(abs(bsr_exp) > critical_val, "p<.05", "p>.05")
    ) %>%
  arrange(comp, meas)


this_data <- boot_res_final %>% filter(comp == 5) %>% arrange(bsr)
axisFace <- ifelse(this_data$sig_bsr == "p<.05", "bold", "plain")
ggplot(this_data, aes(bsr, reorder(meas, bsr), fill = sig_bsr)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[3], rdgy_pal[8])) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 2, alpha = 1.3) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))

# next step is to determine whether there is a meaningful difference between ExPosition fjs and bootstrapped ones
