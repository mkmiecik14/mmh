# Visual Stimulation Task Preprocessing Script
# Matt Kmiecik
# Inspired by: https://github.com/mkmiecik14/ssvep/blob/main/eprime-prepro.R

# Purpose: load raw visual task e-prime data and prepare for analysis

source("r-prep.R") # Prepare R workspace

# Load data ----
vis_edata <- 
  read_delim(
    "../data/visual-eprime-data.txt", # raw data
    delim = "\t", # tab delimited
    guess_max = 2000 # this is necessary due to parsing failures 
  )

# Selecting and cleaning up all the visual eprime data
vis_behav_data <- 
  vis_edata %>%
  select(
    ss = Subject, 
    session = Session, 
    date = SessionDate, 
    time = SessionTime,
    order = Block,
    stim = BlockList,
    rating_1 = RateUnplblock1.RESP,
    rating_2 = RateUnpl1block2.RESP,
    rating_3 = RateUnplblock3.RESP,
    rating_4 = RateUnplblock4.RESP,
    rating_5 = RateUnplblock5.RESP,
  ) %>%
  mutate(date = as.Date(date, format = "%m-%d-%Y")) %>% # converts to date
  distinct(.) %>% # removes repeated rows
  gather(meas, rating, -ss, -session, -date, -time, -order, -stim) %>% # long format
  select(-meas) %>% # unnecessary column
  filter(complete.cases(rating)) %>% # removes "missing" data
  arrange(ss, stim) # orders for better viewing

# Data Editing ----
# The following edits are to raw data that was either entered incorrectly or for
# some other reason
# Subject 354 rating the first stim block an 11 not a 1 (see notes)
vis_behav_data[vis_behav_data$ss == 354 & vis_behav_data$order == 1,]$rating <- 11

# Saves out data ----
save(vis_behav_data, file = "../output/vis-behav-data.RData")
write_csv(vis_behav_data, file = "../output/vis-behav-data.csv")

# Removes R objects created from this script ----
rm(vis_behav_data, vis_edata)