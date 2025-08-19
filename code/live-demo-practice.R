library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx2)

system("wget -N https://oui.doleta.gov/unemploy/csv/ar539.csv -P input/")

eta.539_var_names <- read.csv("input/eta539_var_names.csv")
eta.539_old_names <- eta.539_var_names$dol_code
eta.539_new_names <- eta.539_var_names$dol_title

eta.539_raw <- read.csv("input/ar539.csv") |> 
  setnames(old = eta.539_old_names, new = eta.539_new_names) |> 
  select(all_of(eta.539_new_names)) |> 
  mutate(report_date = mdy(report_date),
         reflect_week_ending = mdy(reflect_week_ending)) |> 
  select(-week_number, -status, -change_date) |> 
  mutate(across(-c(state, report_date, reflect_week_ending), ~ as.numeric(.x)))

eta.539_stata_initial <- eta.539_raw |> 
  mutate(nsa_initial_claims = state_ui_initial_claims + stc_workshare_equivalent_initial_claims) |>
  select(state, report_date, nsa_initial_claims) |> 
  filter(report_date >= "1987-01-01") |> 
  arrange(state, report_date) |> 
  pivot_wider(id_cols = report_date, names_from = state, values_from = nsa_initial_claims) |> 
  select(-PR, -VI) |> 
  setnames(old = state.abb, new = state.name) |> 
  rename(`District of Columbia` = DC) %>% 
  select(report_date, sort(names(.))) |> 
  arrange(report_date)


wb <- wb_workbook()

wb$
  add_worksheet(sheet = "Initial claims")$
  add_data(x = eta.539_state_initial)$
  set_col_widths(cols = 2:ncol(data), widths = 15)$
  add_cell_style(dims = wb_dims(rows = 1, cols = 2:ncol(ui_state_ic)), 
                 wrap_text = TRUE, horizontal = "center", vertical = "center")$
  add_worksheet(sheet = "Continued claims")$
  add_data(x = eta.539_state_continued)$
  save("output/state_ui_counts.xlsx")

