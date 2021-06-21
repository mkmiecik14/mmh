# Principal Components Analysis Procedure
# Matt Kmiecik
# Started 15 June 2021

# Purpose: perform a PCA and inferential testing

source("r-prep.R") # Prepares R workspace

# Loads in data ----
all_pca_data <- 
  list.files(
    path = "../output/", 
    pattern = "*-pca-data.csv", 
    full.names = TRUE
    ) %>%
  map_dfc(~ read_csv(file = .x))

# Proof that all ss columns are identical
# https://stackoverflow.com/questions/31955316/r-how-to-check-if-all-columns-in-a-data-frame-are-the-same
#test <- all_pca_data %>% select(starts_with("ss"))
#length(unique(as.list(test))) == 1 # should be TRUE if identical

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
pca_data_discard  <- pca_data %>% filter(!complete.cases(.)) # 153 discarded
pca_data_keep     <- pca_data %>% filter(complete.cases(.))  # 200 kept

# Converts PCA data to matrix
pca_data_mat <- as.matrix(select(pca_data_keep, -ss)) # removes ss id
rownames(pca_data_mat) <- pca_data_keep$ss # adds rownames of ss id

set.seed(1218) # sets seed for reproducible results

# PCA with bootstrapping and permutation testing using ExPosition
iters <- 2000
pca_res <- epPCA.inference.battery(DATA = pca_data_mat, graphs = FALSE, test.iters = iters)

# To have increased flexibility, these analyses are also replicated by hand:

# Bootstrapping
boot_res <- 
  1:iters %>%
  # select rows at random with replacement
  map(~pca_data_mat[sample(1:nrow(pca_data_mat), replace = TRUE),]) %>%
  map(~scale(.)) %>% # mean centers and standardizes (sD = 1) all columns
  map(~svd(.)) # SVD

# Permutation testing
perm_res <-
  1:iters %>%
  map(~apply(pca_data_mat, 2, sample)) %>% # scrambles columns
  map(~scale(.)) %>% # mean centers and standardizes (sD = 1) all columns
  map(~svd(.)) %>% # SVD
  map("d") %>% # extracts singular values
  map_dfr(~as.tibble(.x), .id = "iter") # assembles into long df
  
# Split-half re-sampling function
shrs <- function(x){
  
  # randomizes the rows so that each participant has equal chance of
  # entering split-halves
  this_data <- x[sample(1:nrow(x)),]
  
  # retrieves rows for split
  first_half <- 1:(nrow(this_data)/2) # rows for the first half
  second_half <- max(first_half):nrow(this_data) # rows for the second half
  #! build in an even/odd checker to trim last row for odd numbered subjects
  
  # splits data in half and scales
  first_data  <- scale(this_data[first_half,]) 
  second_data <- scale(this_data[second_half,])
  
  # performs PCA
  first_data_svd  <- svd(first_data)
  second_data_svd <- svd(second_data)
  
  # Results from split half resampling (shrs)
  shrs_res <- tibble(first = list(first_data_svd), second = list(second_data_svd))
  
  return(shrs_res)
}

# Split-half resampling procedure
shrs_res <- 1:iters %>% map_dfr(~shrs(pca_data_mat), .id = "iter")

# Saves out PCA results
save(
  list = c("pca_res", "boot_res", "perm_res", "shrs_res", "iters"), 
  file = "../output/mmh-res.RData"
  )

# Cleans up workspace
rm(
  all_pca_data,
  pca_data,
  pca_data_discard,
  pca_data_keep,
  pca_data_mat,
  pca_res,
  boot_res,
  perm_res,
  shrs_res,
  iters,
)

