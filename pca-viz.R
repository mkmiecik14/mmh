# PCA Results visualization
# Matt Kmiecik
# Started 21 June 2021

# Purpose: to visualize the results from PCA (pca-proc.R)

source("r-prep.R")              # Prepares R workspace
load("../output/mmh-res.RData") # Loads results

# SCREE
scree_data <- 
  tibble(obs_eigs = pca_res$Fixed.Data$ExPosition.Data$eigs) %>%
  mutate(
    obs_sv = sqrt(obs_eigs), # observed singular values 
    perc = pca_res$Fixed.Data$ExPosition.Data$t # percentage var explained
    )



perm_res_2 <- 
  perm_res %>% 
  group_by(iter) %>% 
  mutate(component = 1:n()) %>% # adds component numbers 
  ungroup() %>%
  mutate(eig = value^2) %>%
  rename(sv = value)

ggplot(perm_res_2, aes(eig)) +
  geom_histogram() +
  facet_wrap(~component)
