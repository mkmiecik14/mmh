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

# removes coldpain as it is redundant with coldpain_resid (the better measure)
pca_data_discard  <- pca_data %>% select(-coldpain) %>% filter(!complete.cases(.)) # 153 discarded
pca_data_keep     <- pca_data %>% select(-coldpain) %>% filter(complete.cases(.))  # 200 kept

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

# Barycentric Discriminant Analysis (BADA)
load("../output/ss-codes.RData") # loads in subject ids and groups

# Subject ids and groups for BADA
ss_codes_trim <- 
  ss_codes %>% 
  filter(ss %in% rownames(pca_data_mat)) # filters out exclusions

# checks to ensure the rownames are in the same order as pca_data_mat
ss_codes_trim %>% 
  mutate(
    pca_ss = rownames(pca_data_mat),
    pca_check = ifelse(ss == pca_ss, 1, 0)
    ) %>%
  summarise(sum = sum(pca_check)) # sum should == the  # of subjects if identical

# Computes BADA
bada_res <- 
  tepBADA.inference.battery(
    DATA = pca_data_mat,
    scale = FALSE,
    DESIGN = ss_codes_trim$group,
    make_design_nominal = TRUE,
    test.iters = iters,
    graphs = FALSE
    )

# computes another BADA without bladder data
bladder_cols <- 
  c(
    "bs_pain", "fs_urg", "fs_pain", "fu_urg", "fu_pain", "mt_urg", "mt_pain",
    "bladder_sharp", "bladder_pressing", "bladder_dull", "bladder_prickling"
  )
pca_data_nobladder <- pca_data_keep %>% select(-bladder_cols) # removes bladder cols

# Converts PCA data to matrix
pca_data_mat_nobladder <- as.matrix(select(pca_data_nobladder, -ss)) # removes ss id
rownames(pca_data_mat_nobladder) <- pca_data_nobladder$ss # adds rownames of ss id

# BADA procedure
bada_res_2 <- 
  tepBADA.inference.battery(
  DATA = pca_data_mat_nobladder,
  scale = FALSE,
  DESIGN = ss_codes_trim$group,
  make_design_nominal = TRUE,
  test.iters = iters,
  graphs = FALSE
)

# Split-half re-sampling function
# Heavily inspired by https://github.com/derekbeaton/Workshops/tree/master/RTC/Apr2017/SplitHalf
shrs <- function(x){
  
  # indices for first split half
  sh_1_indices <- 
    sort(
      sample(
        nrow(x), # number of participants
        round(nrow(x)*.5), # rounds down for even split
        replace = FALSE # sample without replacement
        )
      )
  
  # indices for second split half
  sh_2_indices <- setdiff(1:nrow(x), sh_1_indices)
  
  # Performs PCA
  pca_sh1 <- epPCA(x[sh_1_indices,], scale = TRUE, center = TRUE, graphs = FALSE)
  pca_sh2 <- epPCA(x[sh_2_indices,], scale = TRUE, center = TRUE, graphs = FALSE)
  
  # splits data in half and scales
  # first_data  <- scale(this_data[first_half,]) 
  # second_data <- scale(this_data[second_half,])
  
  # performs PCA
  # first_data_svd  <- svd(first_data)
  # second_data_svd <- svd(second_data)
  
  # Results from split half resampling (shrs)
  shrs_res <- tibble(first = list(pca_sh1), second = list(pca_sh2))

  return(shrs_res)
}

# Split-half resampling procedure
shrs_res <- 1:iters %>% map_dfr(~shrs(pca_data_mat), .id = "iter")

# Saves out PCA results
save(
  list = c("pca_res", "shrs_res", "bada_res", "bada_res_2", "iters"), 
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
  shrs_res,
  iters,
  bada_res,
  ss_codes,
  ss_codes_trim,
  bada_res_2,
  pca_data_mat_nobladder,
  pca_data_nobladder,
  bladder_cols
)

