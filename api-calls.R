# API Configuration
# Matt Kmiecik
# Started: 11 OCTOBER 2021

# Purpose: run this script to refresh Redcap annual data

# Loads packages ----
source("r-prep.R") # Prepares R workspace

# API setup ----
api_url <- "https://survey.northshore.org/api/" # Redcap URL

# Tokens ----
arm1_api_token <- 
  read_lines(
    file = "C:/Analysis/mmh/private/EH13-094-OCP-GRANT-api-token.txt" # token
    )

arm2_api_token <- 
  read_lines(
    file = "C:/Analysis/mmh/private/EH13-094-Arm-2-api-token.txt" # token
  )

shortened_annual_api_token <- 
  read_lines(
    file = "C:/Analysis/mmh/private/EH13-094-Shortened-Annual-api-token.txt" # token
  )

# Function for API calls ----
# PROBLEM IS HERE: https://education.arcus.chop.edu/redcap-r-windows/

#########################################
#                                       #
# Grabs annuals from EH13-094-OCP-GRANT #
#                                       #
#########################################

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

#########################################
#                                       #
# Grabs annuals from EH13-094- Arm 2    #
#                                       #
#########################################

arm2_annuals <- 
  POST(
    url = api_url,
    body = list(
      token = arm2_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "forms[0]" = "annual_questionnaire_combined060416", # form annual_questionnaire_combined060416
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  )
# parses .$content
arm2_annuals_parsed <- content(arm2_annuals, as = "parsed")

# Trying to get time stamps of assessment visit (uses the icsi form)
arm2_avisit1_icsi <- 
  POST(
    url = api_url,
    body = list(
      token = arm2_api_token,
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
arm2_avisit1_icsi_parsed <- content(arm2_avisit1_icsi, as = "parsed")

#######################################
#                                     #
# EH13-094 Shortened Annual Follow Up #
#                                     #
#######################################

short_annuals <- 
  POST(
    url = api_url,
    body = list(
      token = shortened_annual_api_token,
      content = "record",
      format = "csv",
      type = "eav", # for longitudinal output
      "forms[0]" = "shortened_annual_questionnaire", # 
      rawOrLabel = "raw",
      rawOrLabelHeaders = "raw",
      exportCheckboxLabel = "false",
      exportSurveyFields = "true", # reports timestamps etc.
      exportDataAccessGroups = "false",
      returnFormat = "csv"
    )
  )
# parses .$content
short_annuals_parsed <- content(short_annuals, as = "parsed")


# Saves out API data ----

# ARM 1 ANNUAL DATA
save(arm1_annuals_parsed, file = "../output/arm1_annuals_parsed.RData")
# ARM 1 ASSESSMENT VISIT 1 ICSI DATA (mainly for time stamps)
save(arm1_avisit1_icsi_parsed, file = "../output/arm1_avisit1_icsi_parsed.RData")

# ARM 2 ANNUAL DATA
save(arm2_annuals_parsed, file = "../output/arm2_annuals_parsed.RData")
# ARM 2 ASSESSMENT VISIT 1 ICSI DATA (mainly for time stamps)
save(arm2_avisit1_icsi_parsed, file = "../output/arm2_avisit1_icsi_parsed.RData")

# SHORTENED ANNUALS DATA
save(short_annuals_parsed, file = "../output/short_annuals_parsed.RData")


# Cleaning script objects ----
rm(
  arm1_annuals,
  arm1_annuals_parsed,
  arm1_avisit1_icsi,
  arm1_avisit1_icsi_parsed,
  arm2_annuals,
  arm2_annuals_parsed,
  arm2_avisit1_icsi,
  arm2_avisit1_icsi_parsed,
  short_annuals,
  short_annuals_parsed,
  api_url,
  arm1_api_token,
  arm2_api_token,
  shortened_annual_api_token
)



