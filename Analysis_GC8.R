# Project Name: The Global Fund to Fight HTM GC8 Replenishment
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Load packages
library(tidyverse)
library(googlesheets4)

# Link to Google Sheets
sheet_url <- 
  "https://docs.google.com/spreadsheets/d/1tc4cgr_uA36VCEW33GbZVTaPehKALThdjSd2QzBSYgM/edit?gid=406461073#gid=406461073"

# Load OECD data
OECD_Data <- 
  rename(
    read_sheet(
      sheet_url, sheet = "ODA Disbursements 2001-2023"
      )[c("Donor", "TIME_PERIOD", "OBS_VALUE")], c("donor" = "Donor", "year" = "TIME_PERIOD", "oda_spent" = "OBS_VALUE")
    )
