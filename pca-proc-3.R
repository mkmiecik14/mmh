# Principal Components Analysis Procedure
# Matt Kmiecik
# Started 16 SEPTEMBER 2022

# Purpose: re-run PCA but removing slope estimates 

source("r-prep.R") # Prepares R workspace

# Loads in data ----
all_pca_data <- 
  list.files(
    path = "../output/", 
    pattern = "*-pca-data.csv", 
    full.names = TRUE
  ) %>%
  map_dfc(~ read_csv(file = .x)) # map_dfc(~ read_csv(file = .x))

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
# pca_data_discard  <- pca_data %>% filter(!complete.cases(.)) # 153 discarded
# pca_data_keep     <- pca_data %>% filter(complete.cases(.))  # 200 kept

# removes the following variables:
# coldpain as it is redundant with coldpain_resid (the better measure)
# audio/visual measures
pca_data_discard <- 
  pca_data %>% 
  filter(!complete.cases(.)) %>% # 153 discarded
  select(-coldpain, -ends_with("slope"))

pca_data_keep <- 
  pca_data %>% 
  filter(complete.cases(.)) %>%  # 200 kept
  select(-coldpain, -ends_with("slope"))


# Converts PCA data to matrix
pca_data_mat <- as.matrix(select(pca_data_keep, -ss)) # removes ss id
rownames(pca_data_mat) <- pca_data_keep$ss # adds rownames of ss id

set.seed(1218) # sets seed for reproducible results

# PCA with bootstrapping and permutation testing using ExPosition
iters <- 2000
pca_res <- 
  epPCA.inference.battery(
    DATA = pca_data_mat, 
    graphs = FALSE, 
    test.iters = iters
  )

# Saves out PCA results
save(
  list = c("pca_res", "iters"), 
  file = "../output/pca-3-res.RData"
)

# Cleans up workspace
rm(
  all_pca_data,
  pca_data,
  pca_data_discard,
  pca_data_keep,
  pca_data_mat,
  pca_res,
  iters
)

