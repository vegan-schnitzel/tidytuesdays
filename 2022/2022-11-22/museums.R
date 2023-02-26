# tidytuesday challenge: 22 Nov 2022
# UK Museums

library(tidyverse)

# load data
museums <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# how many museums open each year?
open <- museums |> 
  select(Year_opened) |> 
  separate(col = Year_opened, into = c('yr1', 'yr2'), convert = TRUE) |> 
  # let's just assume for now that "yr1" is the opening year...
  group_by(yr1) |>
  # number of opening museums per year
  summarise(n = n()) |>
  # using cumulative sum as index for open museums
  # this disregards closing of museums (!)
  mutate(csum = cumsum(n)) |>
  filter(yr1 >= 1900) |> 
  arrange(desc(yr1))

# cumulative sum ---------------------------------------------------------------
ggplot(open, aes(yr1, csum)) +
  geom_line(size = 1) +
  labs(
    title = "Rate of museum openings is slowing down",
    subtitle = "(United Kingdom)",
    caption = "Data from museweb.dcs.bbk.ac.uk",
    x = "year",
    y = "cumulative sum of open museums"
  ) +
  theme_minimal(base_family = "raleway") +
  theme(
    plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
    panel.background = element_rect(fill = "#fafafa", colour = "#fafafa")
  )
#ggsave("museums.png", path = "2022/2022-11-22")

# sum of openings per half decade ----------------------------------------------
open_per_decade <- open |> 
  # floor opening year to nearest half decade
  mutate(halfDecade = yr1 - yr1 %% 5) |> 
  group_by(halfDecade) |> 
  summarise(n = sum(n))

ggplot(open_per_decade, aes(halfDecade, n)) +
  geom_col() +
  labs(
    title = "UK Museums",
    subtitle = "The number of museum openings in the UK in 5-year intervals from 1900 to present.",
    caption = "Data from museweb.dcs.bbk.ac.uk",
    x = "",
    y = "museum openings"
  ) +
  theme_minimal(base_family = "raleway") +
  theme(
    plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
    panel.background = element_rect(fill = "#fafafa", colour = "#fafafa")
  )
ggsave("museums.png", path = "2022/2022-11-22")