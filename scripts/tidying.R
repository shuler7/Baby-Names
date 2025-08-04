# create a seperate dataframe for each country / state / sex
# which has the variables, name, year, count, prop

# countries are: usa, England / Wales / Scotland (EWS),
#                England / Wales (EW), and scotland.

# states: for USA, each of the 50 states and DC
#         no states for other countries

# sex: 'M' and 'F'

# prop: yearly prop among the subset (country / state / sex)

# year: 1910 - 2024 for USA
#       1996 - 2024 for EW and EWS
#       1974 - 2024 for Scotland

library(tidyverse)
library(readxl)
library(janitor)

###############################################################
# USA name data (1910 - 2024)
###############################################################

# Creates a .csv file for each state_sex pair.
state_codes <- c(state.abb, "DC")
sex <- c('M', 'F')

for (state in state_codes) {
  for (s in sex) {
    print(s)
    write_csv(
      x = read_csv(paste0("data_raw/names_by_state_USA/",
                          state,
                          ".txt"),
                   col_names = FALSE, 
                   col_select = c(4, 3, 5, 2)
      ) |>
        setNames(c("name", "year", "count", "sex")) |>
        filter(sex == s) |>
        select(-sex) |>
        group_by(year) |>
        mutate(proportion = count / sum(count)) |>
        ungroup(),
      file = paste0("data_tidy/usa_states/",
                    state,
                    "_",
                    s,
                    ".csv")
    )
  }
}

# Create national totals for each name from the state data

########
# USA_M
usa <- read_csv("data_tidy/usa_states/AK_M.csv")
for (state in state_codes) {
  usa <- bind_rows(
    usa,
    read_csv(paste0("data_tidy/usa_states/",state,"_M.csv"))
  ) |>
    group_by(name, year) |>
    summarise(count = sum(count), .groups = "drop")
}


# add the proportions column
usa <- usa |>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

write_csv(
  x = usa,
  file = "data_tidy/USA_M.csv"
)

#######
# USA_F
usa <- read_csv("data_tidy/usa_states/AK_F.csv")
for (state in state_codes) {
  usa <- bind_rows(
    usa,
    read_csv(paste0("data_tidy/usa_states/",state,"_F.csv"))
  ) |>
    group_by(name, year) |>
    summarise(count = sum(count), .groups = "drop")
}

# add the proportions column
usa <- usa |>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

write_csv(
  x = usa,
  file = "data_tidy/USA_F.csv"
)


###############################################################
# England / Wales name data (1996 - 2024)
###############################################################

# data is annoyingly formatted in an excel sheet


ew_f <- read_excel(
  "data_raw/names_england_wales_1996_2024.xlsx",
  sheet = "Table_1",
  range = "A5:BG23476"
  ) |>
  select(seq(1, 59, by = 2))|>
  set_names(c("name",seq(2024, 1996, by = -1)))|>
  mutate(across(everything(), ~ ifelse(. == "[x]", "0", .)))|>
  pivot_longer(-name, names_to = "year", values_to = "count") |>
  mutate(across(-name, ~ as.numeric(.))) |>
  filter(count != 0)|>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()|>
  filter(name != "0")

write_csv(
  x = ew_f,
  file = "data_tidy/EW_F.csv"
)


ew_m <- read_excel(
  "data_raw/names_england_wales_1996_2024.xlsx",
  sheet = "Table_2",
  range = "A5:BG18103"
  ) |>
  select(seq(1, 59, by = 2))|> 
  set_names(c("name",seq(2024, 1996, by = -1)))|>
  mutate(across(everything(), ~ ifelse(. == "[x]", "0", .)))|>
  pivot_longer(-name, names_to = "year", values_to = "count") |>
  mutate(across(-name, ~ as.numeric(.))) |>
  filter(count != 0)|>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()|>
  filter(name != "0")
  
write_csv(
  x = ew_m,
  file = "data_tidy/EW_M.csv"
)  


###############################################################
# Scotland name data (1974 - 2024)
###############################################################
scots_M <- read_csv("data_raw/names_scotland_1974_2024/full.csv") |>
  filter(Sex == "Boy") |>
  select(Name, Year, Number)|>
  set_names(c("name", "year", "count"))|>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

write_csv(
  x = scots_M,
  file = "data_tidy/Scotland_M.csv"
)

scots_F <- read_csv("data_raw/names_scotland_1974_2024/full.csv") |>
  filter(Sex == "Girl") |>
  select(Name, Year, Number)|>
  set_names(c("name", "year", "count"))|>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

write_csv(
  x = scots_F,
  file = "data_tidy/Scotland_F.csv"
)


###############################################################
# Merge Scotland, England, Wales name data (1996 - 2024)
###############################################################
scots_M <- read_csv("data_tidy/Scotland_M.csv")
ew_M <- read_csv("data_tidy/EW_M.csv") 

ews_M <- bind_rows(scots_M, ew_M) |>
  group_by(name, year) |>
  summarise(count = sum(count), .groups = "drop") |>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

write_csv(
  x = ews_M,
  file = "data_tidy/EWS_M.csv"
)

scots_F <- read_csv("data_tidy/Scotland_F.csv")
ew_F <- read_csv("data_tidy/EW_F.csv") 

ews_F <- bind_rows(scots_F, ew_F) |>
  group_by(name, year) |>
  summarise(count = sum(count), .groups = "drop") |>
  group_by(year) |>
  mutate(proportion = count / sum(count)) |>
  ungroup()

write_csv(
  x = ews_F,
  file = "data_tidy/EWS_F.csv"
)
  
  

