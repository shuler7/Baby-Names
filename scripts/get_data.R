# get data functions

get_data <- function(region, sex, with_ranks = FALSE) {
  if (region %in% c(state.abb, "DC")) {
    df <- read_csv(paste0("data_tidy/usa_states/",
                           region,
                           "_",
                           sex,
                           ".csv"))
  } else {
    df <- read_csv(paste0("data_tidy/",
                           region,
                           "_",
                           sex,
                           ".csv"))
  }
  
  # if with_ranks is TRUE, get rank data
  if (with_ranks == TRUE) {
    df <- bind_rows(
      df,
      df |>
        group_by(year) |>
        summarise(proportion = max(proportion), count = max(count)) |>
        mutate(name = "#1") |>
        select(name, year, count, proportion),
      df |>
        group_by(year) |>
        arrange(by = desc(proportion)) |>
        summarise(
          proportion = ifelse(length(proportion) >= 10, proportion[10], 0),
          count = ifelse(length(count) >= 10, count[10], 0)
        ) |>
        mutate(name = "#10") |>
        select(name, year, count, proportion),
      df |>
        group_by(year) |>
        arrange(by = desc(proportion)) |>
        summarise(
          proportion = ifelse(length(proportion) >= 100, proportion[100], 0),
          count = ifelse(length(count) >= 100, count[100], 0)
        ) |>
        mutate(name = "#100") |>
        select(name, year, count, proportion),
      df |>
        group_by(year) |>
        arrange(by = desc(proportion)) |>
        summarise(
          proportion = ifelse(length(proportion) >= 1000, proportion[1000], 0),
          count = ifelse(length(count) >= 1000, count[1000], 0)
        ) |>
        mutate(name = "#1000") |>
        select(name, year, count, proportion)
    )
  }
  
  return(df)
}
