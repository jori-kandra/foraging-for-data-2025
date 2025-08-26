# import packages
library(tidyverse)
library(data.table) # setnames() - map data dictionary onto raw data
library(lubridate) # helper functions to handle date formats
library(openxlsx2) # create excel wbs

# download raw data
system("wget https://oui.doleta.gov/unemploy/csv/ar539.csv -P input/")

# import data dictionary
data_dictionary <- read.csv("input/eta539_var_names.csv")

# load raw data
raw_data <- read.csv("input/ar539.csv") |> 
  setnames(old = data_dictionary$dol_code, new = data_dictionary$dol_title) |> 
  mutate(report_date = mdy(report_date),
         reflect_week_ending = mdy(reflect_week_ending))


# initial claims
initial_claims <- raw_data |> 
  mutate(nsa_initial_claims = state_ui_initial_claims + stc_workshare_equivalent_initial_claims) |> 
  select(report_date, state, nsa_initial_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_initial_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  select(-PR, -VI) |>
  rename(`District of Columbia` = DC)


# continued claims
continued_claims <- raw_data |> 
  mutate(nsa_continued_claims = state_ui_adjusted_continued_weeks_claimed + stc_workshare_equivalent_continued_weeks_claimed) |> 
  select(reflect_week_ending, state, nsa_continued_claims) |> 
  pivot_wider(id_cols = reflect_week_ending, names_from = state, values_from = nsa_continued_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  select(-PR, -VI) |>
  rename(`District of Columbia` = DC)

# write excel workbook
wb <- wb_workbook()

wb$
  add_worksheet(sheet = "Initial claims")$
  add_data(x = initial_claims)$
  add_worksheet(sheet = "Continued claims")$
  add_data(x = continued_claims)$
  save(file = "output/state_ui.xlsx")
