library(tidyverse)
library(latex2exp)


tuesdata <- tidytuesdayR::tt_load('2022-06-21')


plot_mapa <- tuesdata$census %>% 
  mutate(ratio = (black_free/white)*1000) %>% 
  group_by(region,year) %>% 
  summarise(across(ratio,mean,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(log_ratio = log(ratio),
         ratio = round(ratio,2),
         region = factor(region,
                         levels = c("Midwest",
                                    "Northeast",
                                    "South",
                                    "West",
                                    "USA Total")),
         color = if_else(region == "USA Total","red","blue")) 


plot_mapa <- ggplot(data = plot_mapa,
                    aes(x = year,y = region))+
  geom_tile(aes(fill = color,
                alpha = log_ratio,
                color = color))+
  geom_text(aes(label = ratio)) +
  scale_fill_manual(values = c(red = "#C70039", blue = "#FFC300")) +
  scale_color_manual(values = c(red = "#C70039", blue = "#FFC300")) +
  theme_minimal() +
  labs(
    title = "Free black men per 1.000 white men in each US region",
    subtitle = TeX("$\\Ratio = \\frac{Number \\; of \\; Free \\; black \\; men}{Number \\; of \\; white\\;men}\\times 1000"),
    caption = "Elaboration: Alex Bajaña @AlexBajaa5 | Data from: US Census's Archives, Slave Voyages, and Black Past.",
    y = "US Region",
    x = ""
  )+
  theme(legend.position = "none",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(face = "bold",size = 14,hjust = 0.5),
        plot.background = element_rect(fill = "white")) 


ggsave(filename = "2022/week_25/tt_21062022.png",plot = plot_mapa)

