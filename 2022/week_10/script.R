# ------------------------------------------------------------------------- #
#                            EU Student mobility                            #
# ------------------------------------------------------------------------- #


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(circlize)
library(psData)

# Reading data ------------------------------------------------------------

erasmus <- tidytuesdayR::tt_load("2022-03-08")

erasmus_data <- erasmus$erasmus

View(erasmus_data)

erasmus_data %>% 
  group_by(academic_year) %>% 
  summarise(total = sum(participants))

summary <- erasmus_data %>% 
  filter(activity_mob == "Transnational youth meetings",
         academic_year == "2019-2020") %>% 
  group_by(sending_country_code,receiving_country_code) %>% 
  summarise(total = sum(participants,na.rm = TRUE) )


summary %>% 
  select(sending_country_code,receiving_country_code) %>% 
  map(unique) %>% 
  map(length)


wide_data <- summary %>% 
  pivot_wider(names_from = receiving_country_code,values_from = total) %>% 
  column_to_rownames(var = "sending_country_code")

countrycode <- read_csv("2022/week_10/country_iso.txt")

countrycode %>% 
  select()


chordDiagram(wide_data)  
  
  