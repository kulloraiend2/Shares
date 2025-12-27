library(tidyverse)
library(readr)


forestfires <- read.csv("forestfires.csv") %>%
  mutate(
    month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")),
    day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))
  )



ffMonth <- forestfires %>%
  # mutate(
  #   month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
  # ) %>%
  group_by(month) %>%
  summarise(n=n())

ggplot(data = ffMonth) +
  aes(x = month, y = n) + 
  geom_bar(stat='identity')

ffDay <- forestfires %>%
  # mutate(
  #   day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))
  # ) %>%
  group_by(day) %>%
  summarise(n=n())

ggplot(data = ffDay) +
  aes(x = day, y = n) + 
  geom_bar(stat='identity')


# boxplots
ggplot(data = forestfires) +
  aes(x = month, y = FFMC) +
  geom_boxplot()

boxplots <- function(x, y) {
  ggplot(data = forestfires) +
    aes_string(x = x, y = y) +
    geom_boxplot()
}

vars <- names(forestfires)[5:12]

map2("month", vars, boxplots)
map2("day", vars, boxplots)

# scatter plots
create_scatter = function(x, y, ds) {
  ggplot(data = ds) + 
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.3) +
    theme(panel.background = element_rect(fill = "white"))
}
map2(vars, "area", create_scatter, ds=forestfires)

map2(vars, "area", create_scatter, ds=filter(forestfires, area<300))
map2(vars, "area", create_scatter, ds=filter(forestfires, area!=0))
  