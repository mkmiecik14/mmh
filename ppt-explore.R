# Pressure Pain Thresholds (PPTs) Visualization Script
# Matt Kmiecik
# Started 1 June 2021

# Purpose: load raw PPT data and clean/prep for analysis

source("r-prep.R") # Prepare R workspace

# Load data ----
load("../output/ss-codes.RData") # subject codes
load("../output/ppt-data.RData") # PPT data

# Examining if there are missing subjects that can be salvaged in a different
# data sheet

# First, let's examine external PPTs (PPT_ext)
ppt_ext_ss <- 
  ppt_data %>% 
  filter(test == "PPT_ext", visit == 1) %>%
  group_by(ss, site) %>%
  summarise(
    m = mean(force, na.rm = TRUE), 
    n = n(), 
    n_na = sum(is.na(force)),
    n_mean = n - n_na) %>%
  ungroup()

# only 1 subject has missing values for both trials:
ppt_ext_ss %>% count(n_mean)

# Dealing with 272 subjects
length(unique(ppt_ext_ss$ss))

# Examining distributions
ggplot(ppt_ext_ss, aes(m)) +
  geom_histogram(binwidth = 4) +
  labs(x = "Force (Newtons)", y = "Frequency") + 
  facet_wrap(~site)

# remove kids

# Raincloud plots
pj <- position_jitter(width = .1)
ggplot(ppt_ext_ss, aes(site, m, color = site, fill = site)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(shape = 1, position = pj, alpha = 2/3) +
  geom_boxplot(aes(fill = NULL), width = .2, position = position_nudge(x = -.25, y = 0)) +
  scale_fill_manual(values = ghibli_palettes$MononokeMedium) +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  labs(x = "Site", y = "Force (Newtons)") +
  theme_minimal() +
  theme(legend.position = "none")
  
  



