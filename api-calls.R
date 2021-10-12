# API Configuration
# Matt Kmiecik
# Started: 11 OCTOBER 2021

# Loads packages ----
source("r-prep.R") # Prepares R workspace

# API setup ----
api_url <- "https://survey.northshore.org/api/" # Redcap URL

# Tokens
arm1_api_token <- 
  read_lines(
    file = "C:/Analysis/mmh/private/EH13-094-OCP-GRANT-api-token.txt" # token
    )


# Function for API calls ----
# PROBLEM IS HERE: https://education.arcus.chop.edu/redcap-r-windows/

# Grabs annuals from EH13-094-OCP-GRANT
arm1_annuals <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "forms[0]" = "annual_questionnaire_combined060416", # form
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  )
# parses .$content
arm1_annuals_parsed <- content(arm1_annuals, as = "parsed")

# Trying to get time stamps of assessment visit (uses the icsi form)
arm1_avisit1_icsi <- 
  POST(
    url = api_url,
    body = list(
      token = arm1_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "forms[0]" = "ic_problem_and_symptoms_indices",
      "events[0]" = "assessment_visit_1_arm_1",
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  )
# parses .$content
arm1_avisit1_icsi_parsed <- content(arm1_avisit1_icsi, as = "parsed")








# EH13-094 Arm #2


# EH13-094 Shortened Annual Follow Up



# Saves out API data ----
# ARM 1 ANNUAL DATA
save(arm1_annuals_parsed, file = "../output/arm1_annuals_parsed.RData")
# ARM 1 ASSESSMENT VISIT 1 ICSI DATA (mainly for time stamps)
save(arm1_avisit1_icsi_parsed, file = "../output/arm1_avisit1_icsi_parsed.RData")
