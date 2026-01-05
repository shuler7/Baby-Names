names <- c('Leslie', 'Leslie')
sexes <- c('M', 'F')
regions <- c('USA')
start_year <- 1910
end_year <- 1980

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

ggplot(df, aes(x = year, y = 100*proportion, color = name_sex_region)) +
  geom_line(linewidth = 1)+
  theme_minimal()+
  labs(
    y = "Percentage",
    x = "Year",
    color = "Region"
  )

# Filter Leslie only, 1910–1980
leslie_1910_1980 <- df %>%
  mutate(name = tolower(name)) %>%
  filter(name == "leslie", year >= 1910, year <= 1980) %>%
  mutate(sex = recode(sex, "M" = "Male", "F" = "Female")) %>%
  group_by(year, sex) %>%
  summarize(proportion = sum(proportion), .groups = "drop") %>%
  complete(year = 1910:1980, sex, fill = list(proportion = 0))

# Plot with proportion on y-axis
ggplot(leslie_1910_1980, aes(x = year, y = 100*proportion, color = sex)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c("Male" = "#89CFF0", "Female" = "#FF69B4")) +
  labs(
    title = "The Gender Shift of the Name Leslie (1910–1980)",
    x = "Year",
    y = "Percentage of babies named Leslie",
    color = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )
