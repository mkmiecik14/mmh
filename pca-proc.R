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
pca_data_discard  <- pca_data %>% filter(!complete.cases(.)) # 155 discarded
pca_data_keep     <- pca_data %>% filter(complete.cases(.))  # 198 kept

# Examining why participants were discarded
discarded <- 
  pca_data_discard %>% 
  pivot_longer(cols = !ss) %>% 
  group_by(ss) %>% 
  summarise(n = n(), n_na = sum(is.na(value))) %>%
  arrange(n_na)

try_to_sal <- discarded %>% filter(n_na == 8)
# Subjects confirmed to have no data
ss_nodata <- 
  c(
    15,
    316,
    167,
    170,
    176,
    178,
    188,
    200,
    202,
    219,
    314,
    42,
    53,
    72,
    95,
    192
    )
test <- pca_data_discard %>% filter(ss %in% try_to_sal$ss, ss %nin% ss_nodata)
