library(tidyverse)
library(modelr)

# load data
worldcups <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

# How does the number of scored goals correlate with the number of matches played?

# create linear model
wc_mod <- lm(games ~ goals_scored, data = worldcups)

# define outliers via residuals
wc <- worldcups |>
  add_residuals(wc_mod) |> 
  mutate(outlier = abs(resid) >= 20)

# plotting...
ggplot(wc, aes(games, goals_scored)) +
  geom_point(aes(colour = outlier), show.legend = FALSE) +
  # display linear regression on inliers only
  geom_smooth(method = "lm", se = FALSE, colour = "#F8766D", size = 1/2,
              data = subset(wc, outlier == FALSE)) +
  # add year label to outlier
  ggrepel::geom_label_repel(aes(label = year), 
                            data = subset(wc, outlier == TRUE)) +
  # beautification
  labs(
    title = "FIFA World Cup History",
    subtitle = "The 16 teams of 1954 World Cup in Switzerland were on fire",
    caption = "Data from kaggle.com",
    x = "# games",
    y = "# scored goals"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
    panel.background = element_rect(fill = "#fafafa", colour = "#fafafa")
  )
ggsave("worldcup.png", path = "2022-11-29")