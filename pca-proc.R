# Principal Components Analysis Procedure
# Matt Kmiecik
# Started 15 June 2021

# Purpose: perform a PCA and inferential testing

source("r-prep.R") # Prepares R workspace

# Loads in data ----
all_pca_data <- 
  list.files(path = "../output/", pattern = "*-pca-data.csv", full.names = TRUE) %>%
  map_dfc(~ read_csv(file = .x))

# Proof that all ss columns are identical
# # https://stackoverflow.com/questions/31955316/r-how-to-check-if-all-columns-in-a-data-frame-are-the-same
test <- all_pca_data %>% select(starts_with("ss"))
length(unique(as.list(test))) == 1 # should be TRUE if identical

# Cleaning up PCA data
pca_data <- 
  all_pca_data %>% 
  rename(ss = ss...1) %>% # preserves a subject column 
  select(
    -contains("..."), # removes the extra subject columns that were created
    -ends_with("_vol") # removes bladder test volumes
    ) 

# PCA must have complete data from everyone
# therefore, cases are deleted list-wise
pca_data_discard  <- pca_data %>% filter(!complete.cases(.)) # 170 discarded
pca_data_keep     <- pca_data %>% filter(complete.cases(.))  # 183 kept

# Examining why participants were discarded
discarded <- 
  pca_data_discard %>% 
  pivot_longer(cols = !ss) %>% 
  group_by(ss) %>% 
  summarise(n = n(), n_na = sum(is.na(value))) %>%
  arrange(n_na)

try_to_sal <- discarded %>% filter(n_na == 2)
test <- pca_data_discard %>% filter(ss %in% try_to_sal$ss)

# potential fixes
# ss 322 CPM first trial has a note of 40 Newtons

# cold pain resid
# ss53 and ss72

# ppt rforehead
# ss116

# ss59  and ss61 -> ppt 5

# ss88 vag_sharp

# ss62 vaginal_prickling


# ss316 and ss15 ts_slope