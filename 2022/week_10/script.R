# ------------------------------------------------------------------------- #
#                            EU Student mobility                            #
# ------------------------------------------------------------------------- #


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(circlize)
library(psData)
library(janitor)

# Reading data ------------------------------------------------------------

# Debido a locale de mi computador he optado por descargar el archivo del
# periodo académico 2019-2020 directamente de:

# https://data.europa.eu/data/datasets/erasmus-mobility-statistics-2014-2019-v2?locale=en

# erasmus <- tidytuesdayR::tt_load("2022-03-08") 

erasmus <- read_csv("2022/week_10/2021.05.11 KA1 mobilities eligible finalised started in 2019.csv")

erasmus_data <- erasmus %>% 
  separate(col =  `Project Reference;Academic Year;Mobility Start Month;Mobility End Month;Mobility Duration;Activity (mob);Field of Education;Participant Nationality;Education Level;Participant Gender;Participant Profile;Special Needs;Fewer Opportunities;GroupLeader;Participant Age;Sending Country Code;Sending City;Sending Organization;Sending Organisation Erasmus Code;Receiving Country Code;Receiving City;Receiving Organization;Receiving Organisation Erasmus Code;Participants`,
           into = c('Project Reference','Academic Year','Mobility Start Month','Mobility End Month','Mobility Duration','Activity (mob)','Field of Education','Participant Nationality','Education Level','Participant Gender','Participant Profile','Special Needs','Fewer Opportunities','GroupLeader','Participant Age','Sending Country Code','Sending City','Sending Organization','Sending Organisation Erasmus Code','Receiving Country Code','Receiving City','Receiving Organization','Receiving Organisation Erasmus Code','Participants'),
           sep = ";"
           )

erasmus_data <- erasmus_data %>% 
  clean_names()  %>% 
  mutate(participants = as.numeric(participants))

View(erasmus_data)

erasmus_data %>% 
  group_by(academic_year) %>% 
  summarise(total = sum(participants,na.rm = T))

summary <- erasmus_data %>% 
  filter(activity_mob == "Structured Courses/Training Events",
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
  
  