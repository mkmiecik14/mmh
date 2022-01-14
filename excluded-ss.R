# Excluded Participants
# Matt Kmiecik
# Started 13 January 2022

# Purpose: detail which participants were excluded and why

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/pca-data-all.rda") # participants kept and discarded before PCA
load("../output/ss-codes.RData") # subject codes


# We started out with n=354 because ss_codes without the kids is 354 rows
all_ss <- ss_codes %>% filter(group %nin% c("KID"))
all_ss %>% count(group) # group counts

# one participant is excluded due to pain medication
all_ss %>% filter(group == "EXCLUDE")

# Given that 200 subjects had complete data in PCA
# Here are the remaining subjects (n=153) that had 1 or more missing data points
excluded <- as_tibble(pca_data_discard)

# counting number of missing data per subject
na_counts <-  
  excluded %>% 
  pivot_longer(-ss) %>%
  group_by(ss) %>%
  summarise(nas = sum(is.na(value))) %>%
  ungroup()

# below is the number that were excluded for not having ANY data:
# n=101
no_data_ss <- na_counts %>% filter(nas >= 40)
# saving out (uncomment to save out)
write_csv(no_data_ss, file = "../output/no-data-ss.csv")

# below is the number that were excluded for missing 1 or more data points
# n=52
missing_data_ss <- na_counts %>% filter(nas < 40) %>% arrange(nas)







