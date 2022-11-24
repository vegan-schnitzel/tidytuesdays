library(tidyverse)

# load data
museums <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# how many museums open per year?
open <- museums |> 
  select(Year_opened) |> 
  separate(col = Year_opened, into = c('yr1', 'yr2'), convert = TRUE) |> 
  # let's just assume for now that "yr1" is the opening year...
  group_by(yr1) |>
  # number of opening museums per year
  summarise(n = n()) |>
  # using cumulative sum as index for open museums
  # this disregards closing of museums
  mutate(csum = cumsum(n)) |>
  filter(yr1 >= 1900) |> 
  arrange(desc(yr1))

open

# plotting
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

ggsave("museums.png", path = "2022-11-22")
