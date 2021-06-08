# Auditory Stimulation Task Preprocessing Script
# Matt Kmiecik
# Inspired by: https://github.com/mkmiecik14/ssvep/blob/main/eprime-prepro.R

# Purpose: load raw visual task e-prime data and prepare for analysis

source("r-prep.R") # Prepare R workspace

# Loads data and processes----
aud_edata <- 
  read_delim(
    file = "../data/auditory-eprime-data.txt", 
    delim = "\t"
    ) %>%
  # selects relevant variables
  select(
    ss = Subject,
    date = SessionDate,
    order = Block,
    stim = BlockList,
    rating_0 = RateSc.RESP,
    rating_1 = RateSc1.RESP,
    rating_2 = RateSc2.RESP,
    rating_3 = RateSc3.RESP,
    rating_4 = RateSc4.RESP,
  ) %>%
  mutate(across(starts_with("rating"), as.numeric)) %>% # changes ratings to num
  pivot_longer(
    cols = starts_with("rating"),
    values_to = "rating"
  ) %>%
  filter(complete.cases(rating)) %>% # gets rid of missing values
  select(-name) %>% # gets rid of useless column
  arrange(ss, stim) # arranges for aesthetics
  
# Saves out cleaned and processed data ----
save(aud_edata, file = "../output/auditory-behav-data.RData")
write_csv(aud_edata, file = "../output/auditory-behav-data.csv")

# Cleans workspace ----
rm(aud_edata)

