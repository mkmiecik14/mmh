# Model comparisons
# Matt Kmiecik
# Started 15 June 2022

# Purpose: to provide a direct comparison of models (pca vs. sensory) to test 
# whether MMH or unimodal QST is a better predictor of outcome

source("r-prep.R") # prepares workspace

# Loads data - - - - 
load("../output/pca-mods.rda") # was created in `api-icsiplus-explore.R`
load("../output/sensory-mods.rda") # was created in `sensory-analysis.R`

# compares the models year by year - - - - 
anova(sensory_mods[[1]], pca_mods[[1]])

test <- sensory_mods[[1]]
test2 <- pca_mods[[1]]

test$model
test2$model

compare_performance(sensory_mods[[4]], pca_mods[[4]])
