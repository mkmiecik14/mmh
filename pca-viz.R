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

pca_data    <- pca_res$Fixed.Data$ExPosition.Data$X # data submitted to PCA
first_fis   <- shrs_res$first %>% map(~.x$u %*% diag(.x$d))   # first split-half
second_fis  <- shrs_res$second %>% map(~.x$u %*% diag(.x$d))  # second split-half

# Preallocates fitted values arrays
first_fis_pred <- 
  second_fis_pred <-
    array(dim = c(nrow(first_fis[[1]]), ncol(first_fis[[1]]), iters))

# Calculates predicted values (see Beaton et al., 2021: https://osf.io/epj8w/)
for(i in 1:iters){
  for(j in 1:ncol(pca_data)){
    # predicts first split fi from second split fi
    first_fis_pred[,j,i] <- lm(first_fis[[i]][,j] ~ 1 + second_fis[[i]][,j])$fitted 
    # predicts second split fi from first split fi
    second_fis_pred[,j,i] <- lm(second_fis[[i]][,j] ~ 1 + first_fis[[i]][,j])$fitted 
  }
}

# Computes the absolute value Spearman correlation between observed and fitted 
# values within each resample (again see Beaton et al., 2021: https://osf.io/epj8w/)
first_fis_cor <- matrix(nrow = iters, ncol = ncol(pca_data)) -> second_fis_cor

for(i in 1:iters){
  for(j in 1:ncol(pca_data)){
    first_fis_cor[i,j] <- abs(cor(first_fis[[i]][,j], first_fis_pred[,j,i], method = "spearman"))
    second_fis_cor[i,j] <- abs(cor(second_fis[[i]][,j], second_fis_pred[,j,i], method = "spearman"))
  }
}

# I'M STILL TRYING TO UNDERSTAND WHAT WENT WRONG!?

test <- tibble(a = first_fis[[1]][,2], b = first_fis_pred[,2,1])
cor(test$a, test$b)
test <- tibble(a = second_fis[[1]][,2], b = second_fis_pred[,2,1])
cor(test$a, test$b)
test_lm <- lm(a~b, data = test)



ggplot(test, aes(a, b)) +
  geom_point() +
  geom_smooth(method = "lm")
 
test <- tibble(a = first_fis[[1]][,1], b = second_fis[[1]][,1])
test_2 <- tibble(first =lm(a~b,data = test)$fitted, second = lm(b~a,data = test)$fitted)
cor(test$a, test_2$first)
cor(test$b, test_2$second)

first_fis_cor_long <- 
  as_tibble(first_fis_cor, rownames = "iter") %>%
  pivot_longer(-iter, names_to = "comp", values_to = "cor") %>%
  mutate(
    iter = as.numeric(iter),
    comp = as.numeric(gsub("V", "", comp))
    )

second_fis_cor_long <- 
  as_tibble(second_fis_cor, rownames = "iter") %>%
  pivot_longer(-iter, names_to = "comp", values_to = "cor") %>%
  mutate(
    iter = as.numeric(iter),
    comp = as.numeric(gsub("V", "", comp))
  )

# Split-half resample results
shr_res <- bind_rows(first_fis_cor_long, second_fis_cor_long, .id = "split")

shr_res %>%
  group_by(split, comp) %>%
  summarise(
    m = mean(cor),
    sd = sd(cor),
    n = n(),
    med = median(cor),
    ll = quantile(cor, .025),
    ul = quantile(cor, .975)
  ) %>%
  ungroup() %>%
  arrange(comp)

# Something is off here...the splits have identical correlations


pj <- position_jitter(width = .1, height = 0)
pjd <- position_jitterdodge(jitter.width = .1, jitter.height = 0, dodge.width = .4)
ggplot(shr_res %>% filter(comp < 6), aes(comp, cor, group = split, color = split)) +
  geom_point(alpha = 1/3, position = pjd) +
  labs(x = "Principal Component", y = "Spearman's Correlation") +
  scale_color_manual(values = c(ghibli_palettes$PonyoMedium[2], ghibli_palettes$PonyoMedium[3])) +
  theme_classic() +
  theme(legend.position = "bottom")



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
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
  pca_furnish

# Computing bootstrapping results
boot_res_iters <- 
  boot_res %>% 
  map(~.x$v %*% diag(.x$d)) %>% # calculates fjs for each iteration
  map(~as.matrix(.x) %>% `rownames<-`(colnames(pca_data))) %>% # adds rownames for meas
  map_dfr(~as_tibble(.x, rownames = "meas"), .id = "iter")

# Just a test to demonstrate the arbitrary nature of axis in back to back PCAs
# boot_res_iters %>% filter(iter == 9) %>%
#   ggplot(., aes(V1, V2)) +
#   geom_vline(xintercept = 0, alpha = 1/3) +
#   geom_hline(yintercept = 0, alpha = 1/3) +
#   geom_point() +
#   coord_cartesian(xlim = c(-10, 10)) +
#   geom_text_repel(aes(label = meas), segment.alpha = 0, show.legend = FALSE) +
#   pca_furnish

# Bootstrapped results long format
boot_res_long <- 
  boot_res_iters %>%
  pivot_longer(cols = c(-iter, -meas), names_to = "comp", values_to = "fs") %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

# Observed factor scores (long format)
fj_obs <- 
  fj %>%
  pivot_longer(-meas, names_to = "comp", values_to = "fs_obs") %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

# Computes standard deviation of bootstrapped fjs
boot_res_meas <- 
  boot_res_long %>%
  mutate(fs_abs = abs(fs)) %>%
  group_by(meas, comp) %>%
  summarise(
    sd = sd(fs_abs),
    n = n(),
  ) %>%
  ungroup()

# calculates bootstrapped ratios
obs_boot <- 
  as_tibble(pca_res$Inference.Data$fj.boots$tests$boot.ratios, rownames = "meas") %>%
  pivot_longer(-meas, names_to = "comp", values_to = "bsr_obs") %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

# combines bootstrapping results
critical_val <- 1.96 # treat like a z score 
boot_res_final <- 
  boot_res_meas %>%
  left_join(., fj_obs, by = c("comp", "meas")) %>%
  mutate(bsr = fs_obs/sd) %>% 
  left_join(., obs_boot, by = c("comp", "meas")) %>%
  mutate(
    sig_bsr = ifelse(abs(bsr) > critical_val, "p<.05", "p>.05"),
    sig_bsr_obs = ifelse(abs(bsr_obs) > critical_val, "p<.05", "p>.05")
    ) %>%
  arrange(comp, meas)


# Bootstrapped Results
this_comp <- 2

# EXPERIMENTAL (means that I computed bootstrap ratios "by hand")
this_data <- boot_res_final %>% filter(comp == this_comp) %>% arrange(bsr)
axisFace <- ifelse(this_data$sig_bsr == "p<.05", "bold", "plain")
plot1 <- 
  ggplot(this_data, aes(bsr, reorder(meas, bsr), fill = sig_bsr)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[3], rdgy_pal[8])) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 2, alpha = 1.3) +
  coord_cartesian(xlim = c(-10, 10)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))

# OBSERVED (means that I computed bootstrap ratios using ExPosition)
this_data <- boot_res_final %>% filter(comp == this_comp) %>% arrange(bsr_obs)
axisFace <- ifelse(this_data$sig_bsr_obs == "p<.05", "bold", "plain")
plot2 <- 
  ggplot(this_data, aes(bsr_obs, reorder(meas, bsr_obs), fill = sig_bsr_obs)) +
  geom_bar(stat = "identity") +
  labs(x = "Bootstrap Ratio (observed)", y = "Measure") +
  scale_fill_manual(values = c(rdgy_pal[3], rdgy_pal[8])) +
  geom_vline(xintercept = c(-1.96, 1.96), linetype = 2, alpha = 1.3) +
  coord_cartesian(xlim = c(-10, 10)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(face = axisFace))

plot1 + plot2

# next step is to determine whether there is a meaningful difference between ExPosition fjs and bootstrapped ones
