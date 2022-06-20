# GSS Analysis
# Matt Kmiecik
# Started 20 June 2022

#* Purpose: creat and analyze the GSS variable and compare it to MMH

# Prep workspace
source("r-prep.R")

# Loads data
load("../output/gss-data.rda")
load("../output/mmh-res.RData") # pca res
load("../output/ss-codes.RData")

# gets factor scores and ss for later
fi <- 
  as_tibble(pca_res$Fixed.Data$ExPosition.Data$fi, rownames = "ss") %>%
  mutate(ss = as.numeric(ss)) %>%
  left_join(., ss_codes %>% select(ss, group), by = "ss") %>%
  select(ss, group, V1:V40)

# Histogram
ggplot(gss_data, aes(sum)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Sum", y = "Frequency") +
  theme_bw() +
  facet_wrap(~meas)

ggplot(gss_data, aes(meas, sum, fill = meas)) +
  geom_boxplot() +
  labs(x = "GSS Component", y = "Sum") +
  theme_bw() +
  scale_fill_manual(values = c(ghibli_palettes$KikiLight[3:5])) +
  theme(legend.position = "none")

# Computing Z scores for combination - - - -

# wide format
gss_data_wide <-
  gss_data %>% 
  pivot_wider(
    id_cols = ss, 
    names_from = meas, 
    values_from = sum,
    values_fn = length
    )

test <- gss_data_wide %>% filter(is.na(pain_site))

# gss data with the pca subjects only and with complete data 
gss_data_pca <- 
  gss_data_wide %>% 
  filter(ss %in% fi$ss) %>% # pca subjects only
  filter(is.na(pain_site)) # complete data only
