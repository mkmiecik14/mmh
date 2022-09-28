# Sensitivity Analysis Explore
# Matt Kmiecik
# 18 March 2022

#* Purpose: visualizes the results from the sensitivity analysis
#* see sensitivity-analysis-proc.R

source("r-prep.R") # Prepares R workspace

# Loading data
load("../output/sensitivity-analysis-results.rda")

# SUMMARY STATS
# partial etas
petas_sum <- 
  petas %>%
  group_by(model, Parameter) %>%
  summarise(
    m = mean(Eta2_partial),
    sd = sd(Eta2_partial),
    n = n(),
    ll = quantile(Eta2_partial, .025, na.rm = TRUE),
    ul = quantile(Eta2_partial, .975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  separate(model, into = c("mod", "delete", "year")) %>%
  select(-delete) %>%
  mutate(year = as.numeric(gsub("y", "", year))) %>%
  mutate(
    mod = fct_relevel(mod, c("sens", "pca")),
    Parameter = fct_relevel(
      Parameter, 
      c("(Intercept)", "year_0", "qst", "bladder", "supra", "V1", "V2", "V3"))
    )

# betas
betas_sum <- 
  betas %>%
  group_by(model, term) %>%
  summarise(
    m = mean(estimate),
    sd = sd(estimate),
    n = n(),
    ll = quantile(estimate, .025, na.rm = TRUE),
    ul = quantile(estimate, .975, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  separate(model, into = c("mod", "delete", "year")) %>%
  select(-delete) %>%
  mutate(year = as.numeric(gsub("y", "", year))) %>%
  mutate(
    mod = fct_relevel(mod, c("sens", "pca")),
    term = fct_relevel(
      term, 
      c("(Intercept)", "year_0", "qst", "bladder", "supra", "V1", "V2", "V3"))
  )

# Partial eta squared plot
pd <- position_dodge(width = .4)
sens_petas_plot <- 
  ggplot(
  petas_sum %>% filter(Parameter != "(Intercept)"), 
  aes(year, m, group = Parameter, color = Parameter)
) +
  geom_point(position = pd, size = 2) +
  # geom_errorbar(
  #   aes(ymin = ll, ymax = ul),
  #   width = .2,
  #   position = pd,
  #   alpha = 1/2
  # ) +
  geom_line(position = pd, alpha = 1/2) +
  scale_shape_manual(values = c(1, 16)) +
  labs(
    x = "Year", 
    y = "Partial Eta^2", 
  ) +
  theme_classic() +
  coord_cartesian(ylim = c(0, .25)) +
  scale_color_manual(
    values = c(
      ghibli_palettes$PonyoMedium[2], 
      ghibli_palettes$MononokeMedium[c(4,5,6)],
      ghibli_palettes$PonyoMedium[c(3,5,7)]
      )
    ) +
  facet_wrap(~mod) +
  theme(legend.position = "none")
sens_petas_plot

# BETAS
pd <- position_dodge(width = .4)
sens_betas_plot <- 
  ggplot(
  betas_sum %>% filter(term != "(Intercept)"), 
  aes(year, m, group = term, color = term)
) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(position = pd, size = 2) +
  # geom_errorbar(
  #   aes(ymin = ll, ymax = ul),
  #   width = .2,
  #   position = pd,
  #   alpha = 1/2
  # ) +
  geom_line(position = pd, alpha = 1/2) +
  labs(
    x = "Year", 
    y = "Beta", 
  ) +
  theme_classic() +
  scale_color_manual(
    values = c(
      ghibli_palettes$PonyoMedium[2], 
      ghibli_palettes$MononokeMedium[c(4,5,6)],
      ghibli_palettes$PonyoMedium[c(3,5,7)]
    )
  ) +
  coord_cartesian(ylim = c(-.6, .6)) +
  scale_y_continuous(breaks = round(seq(-.6, .6, .2), 1)) +
  facet_wrap(~mod) +
  theme(legend.position = "none")
sens_betas_plot 

# Combines plots
sensitivity_analysis_plot <- sens_betas_plot / sens_petas_plot
sensitivity_analysis_plot

# saves out for manuscript
# uncomment out to save
# ggsave(
#   filename = "../output/sensitivity-analysis-plot.svg",
#   plot = sensitivity_analysis_plot,
#   width = 4.2,
#   height = 4,
#   units = "in"
#   )

