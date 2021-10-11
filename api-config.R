# API Configuration
# Matt Kmiecik
# Started: 11 OCTOBER 2021

# Loads packages ----
source("r-prep.R") # Prepares R workspace

# Retrieves API token ----
api_token <- read_lines(file = "C:/Analysis/mmh/private/api-token.txt") # path to private token
api_url <- "https://survey.northshore.org/api/" # Redcap URL

# Load in necessary data for processing ----

# Loads in Excel document with participant groups
# ss_codes <- 
#   read_excel("data/codes_better_v8.xlsx") %>%
#   rename(
#     ss = `Subject ID`, 
#     group = FinalGroup, 
#     endo = Endo, 
#     notes = `reassignment notes`
#   )
# 


# Function for API calls ----
shortened_annuals_api <- 
  function(form = form, record = record){
    postForm(
      uri = api_url,
      token = api_token,
      content = "record",
      format = "json",
      type = "flat",
      "records[0]" = record,
      "forms[0]" = form,
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "false",
      exportDataAccessGroups = "false",
      returnFormat = "json"
    ) %>% 
      fromJSON() # converts from JSON and flattens
  }