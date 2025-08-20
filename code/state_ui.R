# import libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx2)

# Raw ETA ####
## Download ####
# UI IC & CC (NSA) comes for ETA 539, which can be found here: https://oui.doleta.gov/unemploy/DataDownloads.asp
# download eta 539 files with system command: https://oui.doleta.gov/unemploy/csv/ar539.csv
# "wget -N" omites download if data has not been updated "-P" sets the file destination"
system(paste0("wget -N https://oui.doleta.gov/unemploy/csv/ar539.csv  -P input/"))

## Wrangle ####
# format columns based on DOL Data Map (https://oui.doleta.gov/dmstree/handbooks/402/402_4/4024c6/4024c6.pdf#ETA203)
# and DOL UI Handbook (https://wdr.doleta.gov/directives/attach/ETAH/ETHand401_4th_s01.pdf)
eta.539_var_names <- read.csv("input/eta539_var_names.csv")

## Cleanse ####
eta.539_raw <- read.csv("input/ar539.csv") |>
  # replace variable names to be more readable
  setnames(old = eta.539_var_names$dol_code, new = eta.539_var_names$dol_title) |>
  # format date as class 'Date' 
  mutate(report_date = mdy(report_date),
         reflect_week_ending = mdy(reflect_week_ending))

# Analyze ####
## Initial claims (NSA) ####
initial_claims <- eta.539_raw  |> 
  # Initial Claims & Continued Claims, non seasonally adjusted (as seen here: https://oui.doleta.gov/unemploy/claims.asp) 
  # UI IC is calculated from c3 & c7 
  mutate(nsa_initial_claims = state_ui_initial_claims + stc_workshare_equivalent_initial_claims) |> 
  select(state, report_date, nsa_initial_claims) |> 
  # filter out unstable reporting
  filter(report_date >= '1987-01-01') |>
  # transform into wide format - each state is own column
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_initial_claims) |> 
  # remove Puerto Rico and US Virgin Islands
  select(-PR, -VI) |> 
  # replace state abbreviation with state name
  setnames(old = state.abb, new = state.name) |> 
  # rename DC (not included in state* utility functions)
  rename(`District of Columbia` = DC) |> 
  # sort data
  arrange(report_date)

## Continued claims (NSA) ####
continued_claims <- eta.539_raw |> 
  # Initial Claims & Continued Claims, non seasonally adjusted (as seen here: https://oui.doleta.gov/unemploy/claims.asp) 
  # UI CC is calculated from c8 & c12
  mutate(nsa_continued_claims = state_ui_adjusted_continued_weeks_claimed + stc_workshare_equivalent_continued_weeks_claimed) |> 
  select(state, reflect_week_ending, nsa_continued_claims) |> 
  # filter out unstable reporting
  filter(reflect_week_ending >= '1987-01-01') |>
  # transform into wide format - each state is own column
  pivot_wider(id_cols = reflect_week_ending, names_from = state, values_from = nsa_continued_claims) |> 
  # remove Puerto Rico & US Virgin Islands
  select(-PR, -VI) |> 
  # replace state abbreviation with state name
  setnames(old = state.abb, new = state.name) |> 
  # replace DC (not included in state.* utility data)
  rename(`District of Columbia` = DC) |> 
  # calculate US Total (good for checking against topline numbers)
  mutate(`US Total` = rowSums(.[2:52], na.rm = TRUE)) |> 
  # sort data 
  arrange(reflect_week_ending)

# Export ####

# create WB object
wb <- wb_workbook()

# write UI state IC to WB object
#note: $ - pipe operator in openxlsx2 
wb$
  # add new worksheet
  add_worksheet(sheet = "Initial claims")$
  # add data to worksheet
  add_data(x = initial_claims)$
  # set columm widths
  set_col_widths(cols = 2:ncol(initial_claims), widths = 15)$
  # format column headers
  add_cell_style(dims = wb_dims(rows = 1, cols = 2:ncol(initial_claims)), 
                 wrap_text = TRUE, horizontal = "center", vertical = "center")$
  # repeat for continued claims
  add_worksheet(sheet = "Continued claims")$
  add_data(continued_claims)$
  set_col_widths(cols = 2:ncol(continued_claims), widths = 15)$
  add_cell_style(dims = wb_dims(rows = 1, cols = 2:ncol(continued_claims)), 
                 wrap_text = TRUE, horizontal = "center", vertical = "center")$
  # save workbook to output folder
  save("output/state_ui.xlsx")

  
