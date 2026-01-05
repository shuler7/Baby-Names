library(tidyverse)

source("scripts/get_data.R")

df <- get_data("USA", "F") |>
  group_by(year) |>
  arrange(desc(proportion)) |>
  slice(1:10) |>
  ungroup() |>
  count(name) |>
  arrange(desc(n))

df1 <- get_data("USA", "F") |>
  group_by(year) |>
  arrange(desc(proportion)) |>
  slice(1:10) |>
  ungroup() |>
  filter(year == 1963)

source("scripts/simple_plotting.R")

simple_plot(
  names = c('Helen', 'Betty','Linda', 'Lisa', 'Jennifer', 'Ashley', 'Isabella'),
  sexes = c('F','F','F','F', 'F', 'F', 'F', 'F'),
  region = c('USA'),
  start_year = 1910,
  end_year = 2024
)



