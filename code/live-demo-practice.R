library(tidyverse)
library(data.table) # setnames(), map DOL data dictionary to the raw data
library(lubridate) # data helper functions to recast messy data as date type
library(openxlsx2) # mapping data and setting formatting for excel wb

# download the raw data from DOL
system("wget -N https://oui.doleta.gov/unemploy/csv/ar539.csv -P input/")

eta.539_var_names <- read.csv("input/eta539_var_names.csv")
eta.539_old_names <- eta.539_var_names$dol_code
eta.539_new_names <- eta.539_var_names$dol_title

# raw data
eta.539_raw <- read.csv("input/ar539.csv") |> 
  setnames(old = eta.539_old_names, new = eta.539_new_names) |> 
  mutate(report_date = mdy(report_date),
         reflect_week_ending = mdy(reflect_week_ending))

# initial claims
initial_claims <- eta.539_raw |> 
  mutate(nsa_initial_claims = state_ui_initial_claims + stc_workshare_equivalent_claims) |> 
  select(state, report_date, nsa_initial_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_initial_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  select(-PR, -VI) |> 
  rename(`District of Columbia` = DC)

continued_claims <- eta.539_raw |> 
  mutate(nsa_continued_claims = state_ui_adjusted_continued_weeks_claimed + stc_workshare_equivalent_continued_weeks_claimed) |> 
  select(state, report_date, nsa_continued_claims) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_continued_claims) |> 
  setnames(old = state.abb, new = state.name) |> 
  select(-PR, -VI) |> 
  rename(`District of Columbia` = DC)

# xlsx workbook
wb <- wb_workbook()

wb$
  add_worksheet(sheet = "Initial claims")$
  add_data(x = initial_claims)$
  add_worksheet(sheet = "Continued claims")$
  add_data(x = continued_claims)$
  save(file = "output/state_ui_claims.xlsx")
