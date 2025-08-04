# some popularity analysis

# Given number of years, num_years, and sex, build a dataframe with
# columns: name, n_year_avg_prop_r1, n_year_avg_prop_r2, diff

# The script is set up to identify names that are more common in 
# region 1 compared to region 2.

source("scripts/get_data.R")
num_years <- 10
r1 <- "TN"
r2 <- "USA" 
sex <- "M"

df <- bind_rows(
  get_data(r1, sex) |> mutate(region = "r1"),
  get_data(r2, sex) |> mutate(region = "r2")
) |> 
  filter(year >= 2024 - num_years) |>
  complete(name, year, region, fill = list(count = 0, proportion = 0)) |>
  group_by(name, region) |>
  summarise(avg_prop = mean(proportion), total_count = sum(count)) |>
  pivot_wider(
    names_from = region,
    values_from = c(avg_prop, total_count)
  ) |>
  mutate(diff_prop = avg_prop_r1 - avg_prop_r2,
         diff_total = total_count_r1 - total_count_r2,
         pct_diff = avg_prop_r1 / avg_prop_r2,
         prob_from_r1 = total_count_r1 / (total_count_r2 + total_count_r1))

df <- arrange(df, desc(prob_from_r1), desc(total_count_r1))

#############################################################


