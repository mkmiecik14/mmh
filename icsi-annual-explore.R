# ICSI Annuals Exploration and Modeling Script
# Matt Kmiecik
# 28 OCTOBER 2021

# Purpose: to visualize and inspect the annual ICSI data and model trajectories

source("r-prep.R") # Prepares R workspace

# Loads data ----
load("../output/complete-icsi-data.Rdata")

# Exploration ----

# Computing basic summary stats
icsi_sum <- 
  complete_icsi_data %>%
  group_by(year) %>%
  summarise(
    m = mean(days_from_baseline, na.rm = TRUE),
    med = median(days_from_baseline, na.rm = TRUE),
    min = min(days_from_baseline, na.rm = TRUE),
    max = max(days_from_baseline, na.rm = TRUE),
    n = n(),
    sd = sd(days_from_baseline, na.rm = TRUE),
    sem = sd/sqrt(n)
    ) %>%
  ungroup()

pj <- position_jitter(width = .1, height = .1)
pn <- position_nudge(x = .2, y = 0)
ggplot(icsi_sum, aes(year, m)) +
  geom_point(
    data = complete_icsi_data, 
    aes(y = days_from_baseline), 
    alpha = 1/3,
    position = pj
    ) +
  geom_bar(stat = "identity", fill = "white", color = "black", width = 1/2, position = pn, alpha = 1/3) +
  geom_errorbar(aes(ymin = m-sd, ymax = m+sd, width = .4)) +
  theme_classic()

ggplot(complete_icsi_data, aes(year, days_from_baseline/365, group = year)) +
  geom_point(
    alpha = 1/3,
    position = pj
  ) +
  geom_boxplot(position = pn) 

# Making sure that each timepoint has more days past than the last
test <- 
  complete_icsi_data %>%
  select(ss, year, days_from_baseline) %>%
  pivot_wider(
    id_cols = ss, 
    names_from = year, 
    values_from = days_from_baseline, 
    names_prefix = "year_"
    ) %>%
  mutate(
    y1_y0 = year_1-year_0,
    y2_y1 = year_2-year_1,
    y3_y2 = year_3-year_2,
    y4_y3 = year_4-year_3,
    y5_y4 = year_1-year_0
    )

test %>% select(ss, y1_y0:y5_y4) %>% pivot_longer(-ss) %>% filter(value < 0)

complete_icsi_data %>% filter(year == 2, days_from_baseline>1000)
complete_icsi_data %>% filter(ss==162) # looks like a year 1 was given at year 2

complete_icsi_data %>% filter(year == 4, days_from_baseline<1200)
complete_icsi_data %>% filter(ss==156) # should be a year 3

# EDIT SUBJECT 109!
complete_icsi_data %>% filter(year == 4, days_from_baseline>1700)
complete_icsi_data %>% filter(ss==109) # ss 109 should get rid of year 4 data and just use year 5

complete_icsi_data %>% filter(year == 5, days_from_baseline>2000)
complete_icsi_data %>% filter(ss==30) # no issue here just took it late


# might want to take a look at participants with high vlaues for annual 1
complete_icsi_data %>% filter(year == 1, days_from_baseline>400)

