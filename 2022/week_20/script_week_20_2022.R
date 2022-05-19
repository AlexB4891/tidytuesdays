
# Libraries ---------------------------------------------------------------


library(tidyverse)
library(ggforce)
library(tidytuesdayR)
library(ggrepel)
library(cowplot)


# Reading and loading data -----------------------------------------------------

tuesdata <- tt_load('2022-05-17')

world <- map_data("world")


# Preparing data: ---------------------------------------------------------

# Calculating the centroids of each country polygon

centroids <- world %>% 
  group_by(region) %>% 
  summarise(across(.cols = c(long,lat),.fns = median))

# Some corrections on region and filtering:

euro_summ <- tuesdata$eurovision %>% 
  filter(winner,
         year>=2000) %>% 
  count(artist_country,section) %>% 
  rename(region = artist_country) %>% 
  mutate(region = case_when(
    region == "United Kingdom" ~ "UK",
    region == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    region == "Serbia & Montenegro" ~ "Serbia",
    TRUE ~ region,
  ))

# Join data to the centroids:

euro_data <- euro_summ %>%
  inner_join(centroids) 

# Countries just in our dataset:
world <- world %>% 
  filter(region %in% unique(euro_data$region))
  
# Creating a factor for the section:

winners_data <- euro_data %>% 
  rename_with(.cols = c(long,lat),.fn = ~str_c(.,"_cent")) %>% 
  mutate(section = factor(section,levels = c(
    "first-semi-final",
    "second-semi-final",
    "semi-final",
    "grand-final"
  )),
  section = fct_relabel(section,
                        .fun = ~str_replace_all(.,pattern = "-",replacement = " ") %>% str_to_sentence))

# Preparing data for the sides sankeys:

winners_data <- winners_data %>% 
  filter(!is.na(section)) %>% 
  mutate(indicador = if_else(section %in% c( "Semi final","Grand final"),"a","b")) %>% 
  group_by(indicador) %>% 
  arrange(section) %>% 
  mutate(acum = cumsum(n),
         position = 125*(acum/sum(n)),
         position = position - 50) %>% 
  ungroup()

# Total number of winners in all sections by country:

sumas <- winners_data %>% 
  group_by(region) %>% 
  summarise(n = sum(n),across(c(long_cent,lat_cent),unique))

# Creating the points for the `geom_diagonal_wide` function of `ggforce`:

puntos_inicio <- winners_data %>% 
  select(region,section, long_cent,lat_cent,position) 

puntos_fin <-puntos_inicio %>% 
  select(region,section, lat_cent = position) %>% 
  mutate(long_cent = if_else(section %in% c(  "Semi final","Grand final"),250,-75))

puntos <- bind_rows(
  puntos_inicio %>% 
    select(-position)%>% 
    mutate(lat_cent = lat_cent -0.5),
  puntos_inicio %>% 
    select(-position) %>% 
    mutate(lat_cent = lat_cent +0.5), 
  puntos_fin,
  puntos_fin %>% 
    mutate(lat_cent = lag(lat_cent))
) %>% 
  arrange(region,section) %>% 
  mutate(lat_cent = replace_na(lat_cent,-50),
         group = rep(1:116,each =4 ))  

# Label table:

textos <- puntos %>% 
  split(list(.$region,.$section)) %>% 
  map(slice,3:4) %>%
  map(group_by,region,section,group) %>% 
  map(summarise,across(c(long_cent,lat_cent),mean)) %>% 
  reduce(bind_rows) %>% 
  mutate(long_cent = if_else(section %in% c(  "Semi final","Grand final"),250,-75))

# Plotting:

plot <- ggplot()+
  geom_map(data = world,
           map = world,
           aes(long,lat,
               map_id = region),fill = "lightgray",color = "gray")+
  geom_point(data = sumas,
             aes(x = long_cent,y = lat_cent,size= n),
             color = "blue",alpha = 0.3) +
  geom_vline(mapping = aes(xintercept = 250),alpha = 0.2) +
  geom_vline(mapping = aes(xintercept = -75),alpha = 0.2) +
  geom_diagonal_wide(data = puntos,mapping = aes(x =long_cent,y = lat_cent,group = group,fill = section),alpha = 0.2) +
  geom_text_repel(data = textos,mapping = aes(x =long_cent,y = lat_cent,label = region),size = 3) + 
  scale_size(range = c(1,10))+
  scale_fill_viridis_d(begin = 0.5)+
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.4, 0.2), 
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5,size = 25),
        plot.subtitle =  element_text(hjust = 0.5,size = 16),
        plot.caption =  element_text(hjust = 0.5,size = 12),
        plot.background = element_rect(fill = "#DDE0ED"),
        plot.margin = margin(r = 2.5,l = 2.5,t = 1.5,b = 1.5,unit = "cm")) +
  labs(title = "Eurovision winners through the sections in the contests",
       size = "Total number of winners",
       fill = "Sections in Eurovision:",
       caption = "Thanks to  Tanya Shapiro and Bob Rudis for the data | Elaboration: Alex Bajaña @AlexBajaa5",
       subtitle = "In the map the size of the dot represents the amount of winners in all sections. Each stripe begin in each country and the final size at the sides is\nthe number of winner in each sections for each country. The grand champions are counted as the winner of the grand final section.\nThe data was filtered from 2000 onwards (In 2005 was introduced the semi-final section) ")

 # Saving the image:
 
 ggsave(plot = plot,filename = "2022/week_20/tt_map_w20_2022.png",dpi = 500,width = 18,height = 10)
