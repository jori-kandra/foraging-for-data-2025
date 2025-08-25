# import packages
library(tidyverse)
library(data.table) # setnames() - map data dictionary onto raw data
library(lubridate) # helper functions to handle date formats
library(openxlsx2) # map data to excel wb

# download raw data
system("wget https://oui.doleta.gov/unemploy/csv/ar539.csv -P input/")

# data dictionary
data_dictionary <- read.csv("input/eta539_var_names.csv")

# raw data
raw_data <- read.csv("input/ar539.csv") |> 
  setnames(old = data_dictionary$dol_code, new = data_dictionary$dol_title) |> 
  mutate(report_date = mdy(report_date))

# initial claims
initial_claims <- raw_data |> 
  mutate(nsa_initial_claims = state_ui_initial_claims + stc_workshare_equivalent_initial_claims) |> 
  select(report_date, state, nsa_initial_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_initial_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  rename(`District of Columbia` = DC) |> 
  select(-PR, -VI)

# continued claims
continued_claims <- raw_data |> 
  mutate(nsa_continued_claims = state_ui_adjusted_continued_weeks_claimed + stc_workshare_equivalent_continued_weeks_claimed) |> 
  select(report_date, state, nsa_continued_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_continued_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  rename(`District of Columbia` = DC) |> 
  select(-PR, -VI)

# export data
wb <- wb_workbook()

wb$
  add_worksheet(sheet = "Initial claims")$
  add_data(x = initial_claims)$
  add_worksheet(sheet = "Continued claims")$
  add_data(x = continued_claims)$
  save(file = "output/state_ui.xlsx")
