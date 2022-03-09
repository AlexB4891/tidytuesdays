install.packages("tidytuesdayR")
install.packages("akc")

library(tidyverse)
library(akc)

tuesdata <- tidytuesdayR::tt_load('2022-01-18')

choco_data <- tuesdata$chocolate

glimpse(choco_data)

# Pregunta: ¿Que hace a un chocolate ser el número 1?

choco_data <- choco_data %>% 
  mutate(cocoa_percent = str_remove(string = cocoa_percent,pattern = "[:punct:]") 
         # %>% 
         #   as.numeric()
         ,
         number_ingredients = str_extract(ingredients,"[:digit:]"),
         number_ingredients = as.numeric(number_ingredients),
         recipe = str_remove_all(string = ingredients,pattern = "[:digit:]-"),
         recipe = str_trim(recipe))  
  
  
n_ingredients <- choco_data %>% 
  pull(number_ingredients) %>% 
  max(.,na.rm = T)


choco_data <- choco_data %>% 
  mutate(recipe = str_split(string = recipe,pattern = ",",n = 6),
         
         recipe = map(recipe,~.x %>%
                        tibble() %>% 
                        mutate(n = 1) %>% 
                        pivot_wider(names_from = 1,values_from = 2)))

choco_data <- choco_data %>% 
  mutate(recipe = reduce(recipe,bind_rows)) %>% 
  unnest(recipe)


# Son los chocolates cuyas recetas no se disponen

choco_data %>% 
  filter(`NA` == 1)  %>% 
  View

choco_data <- choco_data %>% 
  rename(sin_receta = `NA` ,
         S_otro = `S*` ) %>% 
  mutate(across(B:S_otro,~replace_na(.x,0)))


choco_data <- choco_data %>% 
  rowid_to_column(var = "rowid")

choco_data %>% 
  select(keyword = most_memorable_characteristics,rowid) %>% 
  keyword_clean(id = "rowid",sep = ",")
    

keyword_text <- choco_data %>% 
  select(keyword = most_memorable_characteristics,rowid) %>% 
  keyword_clean(id = "rowid",sep = ",") %>% 
  keyword_merge(keyword = "keyword")


grouped_text <- keyword_text %>%
  keyword_group()


keyword_text %>%
  keyword_group() %>% 
  keyword_cloud()

vector <- choco_data %>% 
  select(keyword = most_memorable_characteristics,rowid) %>%
  keyword_clean(id = "rowid",sep = ",") %>% 
  pull(keyword) %>% make_dict()





keyword_text  %>% make_dict(dict_vacabulary_vector = "keyword")


  