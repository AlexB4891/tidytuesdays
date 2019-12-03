# -------------------------------------------------------------------------#
#                    Philadelphia PArkin Violations                        #
# -------------------------------------------------------------------------#
# Elaborated by: Alex Bajaña

# Libraries:

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(ggthemes)
library(scales)

theme_

# Create the directory if it's ausent:

if(!dir.exists("week_49")){
dir.create("week_49")
}

# Obtain the data: may takes some minutes.

philly <- tidytuesdayR::tt_load("2019-12-03")

# First I will check the frequiencies of violations
# and issuing agency:

# Top 10 agencies and issuing agencies

vars <- c("violation_desc","issuing_agency")

sum_tab <- vars %>%
  set_names(.,.) %>% 
  map(~philly$tickets %>% 
        filter_at(.x,any_vars(!is.na(.))) %>% 
        pull(.x) %>% 
        tabyl %>% 
        rename_at(1,function(x).x) %>% 
        arrange(percent) %>% 
        mutate_at(.x,list(~factor(.) %>% 
                            fct_reorder(n) 
                          )
                  ) %>% 
        head(10)
      )

names(sum_tab) %>% 
str_split("_") %>% 
  map(~.x %>% 
  str_replace("desc$","description") %>% 
    str_c(collapse = " ") %>% 
  str_to_sentence
  )


frecuencies <- list(
    tables = sum_tab,
    vx = names(sum_tab),
    colors = c("#b4ffff","#b4ffda"),
    titles = c("Top 10 Parking Violations",
               "Top 10 Issuing agencies")) %>% 
  pmap(function(tables,vx,colors,titles){
    tables %>%  
      ggplot()+
      geom_col(aes_string(x = vx,
                          y = "n"),
               colour = colors) +
      ggplot2::coord_flip()+
      labs(title = titles)+
      theme_wsj() +
      theme(axis.title.x = element_blank())
    
  })

# First I will subset the dataset to the top ten Issuing agency:

top_10_a <- sum_tab[["issuing_agency"]]$issuing_agency 
top_10_v <- sum_tab[["violation_desc"]]$violation_desc 


# Tiles:

philly$tickets %>% 
  # filter(issuing_agency %in% top_10_a) %>% 
  group_by_at(vars) %>% 
  summarise(fine = sum(fine,na.rm=T)) %>% 
  filter(violation_desc %in% top_10_v) %>% 
  ggplot()+
  geom_tile(aes(x =issuing_agency,y = violation_desc,fill = fine )) +
  scale_fill_continuous(labels = dollar) +
  labs(x = "Parking violation",y = "Issuing agency",
       title = "¿Where the money goes?",
       fill = "Fine") + 
  theme_wsj()



    

    
