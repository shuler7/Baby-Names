# plotting against ranks

# this script is designed to plot one name/sex/region against
# the ranks of that sex/region over time

library(tidyverse)

rank_plot(
  name = "Karen",
  sex = "F",
  region = "USA",
  start_year = 1910,
  end_year = 2024,
  ranks = c(1, 10, 100, 1000)
)

########################################################
rank_plot <- function(name,
                      sex,
                      region,
                      start_year,
                      end_year,
                      ranks = c(1, 10, 100, 1000)
                      ){
  source("scripts/get_data.R")
  names <- c(my_name, paste0("#", ranks))
  
  df <- get_data(region, sex, with_ranks = TRUE) |>
    filter(name %in% names) |>
    filter(year >= start_year & year <= end_year) |>
    mutate(
      name = factor(name, levels = names),
      color = ifelse(name == my_name, "red", "gray70"),
      linetype = ifelse(name == my_name, "solid", "dotted"),
      size = ifelse(name == my_name, 1, 0.8)
    )
  
  ggplot(df, aes(x = year, y = 100*proportion, group = name))+
    geom_line(aes(color = name, linetype = name, size = name))+
    scale_color_manual(name = NULL, values = setNames(df$color, df$name)) +
    scale_linetype_manual(name = NULL, values = setNames(df$linetype, df$name)) +
    scale_size_manual(name = NULL, values = setNames(df$size, df$name)) +
    theme_minimal()+
    labs(
      x = "Year",
      y = "Percentage of Babies",
      title = paste0(
        "Percentage of Babies Named ",
        my_name,
        " (",
        start_year,
        "-",
        end_year,
        ")\n",
        "with Ranks for Reference."
      )
    )
}
