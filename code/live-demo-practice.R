# import packages
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx2)

# download raw data
system("wget -N https://oui.doleta.gov/unemploy/csv/ar539.csv -P input/")

# read in data dictionary
eta.539_var_names <- read.csv("input/eta539_var_names.csv")

# read in the data
eta.539_raw <- read.csv("input/ar539.csv") |> 
  setnames(old = eta.539_var_names$dol_code, new = eta.539_var_names$dol_title) |> 
  mutate(report_date = mdy(report_date),
         reflect_week_ending = mdy(reflect_week_ending))

# NSA initial claims
initial_claims <- eta.539_raw |> 
  mutate(nsa_initial_claims = state_ui_initial_claims + stc_workshare_equivalent_initial_claims) |> 
  select(state, report_date, nsa_initial_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_initial_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  rename(`District of Columbia` = DC) |> 
  select(-PR, -VI) |> 
  arrange(report_date)

# continued claims
continued_claims <- eta.539_raw |> 
  mutate(nsa_continued_claims = state_ui_adjusted_continued_weeks_claimed + stc_workshare_equivalent_continued_weeks_claimed) |> 
  select(state, report_date, nsa_continued_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_continued_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  rename(`District of Columbia` = DC) |> 
  select(-PR, -VI) |> 
  arrange(report_date)

# assign workbook object
wb <- wb_workbook()

wb$
  add_worksheet(sheet = "Initial claims")$
  add_data(x = initial_claims)$
  add_worksheet(sheet = "Continued claims")$
  add_data(x = continued_claims)$
  save("output/state_ui.xlsx")

