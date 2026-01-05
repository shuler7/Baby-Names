library(tidyverse)

# simple plot

simple_plot(
  names = c('Leslie', 'Leslie'),
  sexes = c('F', 'M'),
  region = c('USA'),
  start_year = 1910,
  end_year = 1970
)

#################################################################
simple_plot <- function (names, sexes, regions, start_year, end_year){
  source("scripts/get_data.R")
  df <- data.frame(
    name = character(),
    year = integer(),
    count = numeric(),
    proportion = numeric(),
    region = character(),
    stringsAsFactors = FALSE
  )
  
  for (region in regions) {
    for (i in 1:length(names)) {
      df <- bind_rows(
        df,
        get_data(region, sexes[[i]]) |>
          filter(name == names[[i]])|>
          filter(year >= start_year & year <= end_year)|>
          mutate(region = region)|>
          mutate(sex = sexes[[i]])
      )
    }
  }
  
  df <- df |>
    complete(name, year = full_seq(start_year:end_year, 1), region, sex,
             fill = list(count = 0, proportion = 0)
             ) |>
    mutate(name_sex_region = paste(region, name, sex, sep = "/"))
  
  ggplot(df, aes(x = year, y =proportion, color = name_sex_region)) +
    geom_line(linewidth = 1)+
    theme_minimal()+
    scale_y_continuous(labels = scales::percent)+
    labs(
      y = "Percentage",
      x = "Year",
      color = "Region",
      title = "Baby Name Percentages over Time"
    )
} 


