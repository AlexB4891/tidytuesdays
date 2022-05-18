# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-05-03')
tuesdata <- tidytuesdayR::tt_load(2022, week = 18)
  
capacity <-  tuesdata$capacity

wind <-  tuesdata$wind

solar <-  tuesdata$solar

average_cost <-  tuesdata$average_cost

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')


data_plot <- capacity %>% 
  group_by(year) %>% 
  summarise(
    media = mean(total_gw),
    sd = sd(total_gw),
    n = n(),
    ls = media + 1.96*sd/sqrt(n),
    li = media - 1.96*sd/sqrt(n)
  )


grafico <- data_plot %>% 
  ggplot(aes(x = year,media)) +
  geom_errorbar(aes(ymax = ls,ymin = li)) +
  geom_line() +
  geom_point() +
  labs(caption = "Data from:  Berkeley Lab | Elaborated by: Alex",
       title = "Esto es un titulo",
       subtitle = "Esto un subtitulo")


ggsave(filename = "2022/week_18/evolucion_medias.png",dpi = 300)


