# -------------------------------------------------------------------------#
#                    Philadelphia Parkin Violations                        #
# -------------------------------------------------------------------------#
# Elaborated by: Alex Bajaña

# Libraries:

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggpubr)
library(scales)

# Create the directory if it's ausent:

if(!dir.exists("week_49")){
dir.create("week_49")
}

# Obtain the data: may takes some minutes.

philly <- tidytuesdayR::tt_load("2019-12-03")

# First I will check the frequiencies of violations
# and issuing agency:

# Top 10 violations and issuing agencies and 

# Using the the map function, combined to tidy evaluation
# the two summaries are done easily

vars <- c("violation_desc","issuing_agency")

sum_tab <- vars %>%
  set_names(.,.) %>% 
  map(~philly$tickets %>% 
        filter_at(.x,any_vars(!is.na(.))) %>% 
        pull(.x) %>% 
        tabyl %>% 
        rename_at(1,function(x).x) %>% 
        arrange(desc(percent)) %>% 
        mutate_at(.x,list(~factor(.) %>% 
                            fct_reorder(n) 
                          )
                  ) %>% 
        head(10)
      )

# purrr's function pmap allow us to pass arguments in a anonymous function 
# using the mapping logic:

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
      scale_y_continuous(labels = number) +
      ggplot2::coord_flip()+
      labs(title = titles)+
      theme_economist() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
  })

# First I will subset the dataset to the top ten Issuing agency and 
# violations in vectors:

top_10_a <- sum_tab[["issuing_agency"]]$issuing_agency 
top_10_v <- sum_tab[["violation_desc"]]$violation_desc 


# Filtering the dataset by the top 10 violations a tile plot is created:
# The idea is to show viusaly how the total fines are distributed in the agencies. 

tiles <- philly$tickets %>% 
  group_by_at(vars) %>% 
  summarise(fine = sum(fine,na.rm=T)) %>% 
  filter(violation_desc %in% top_10_v) %>% 
  ggplot(aes(x =issuing_agency,y = violation_desc,fill = fine ))+
  geom_tile() +
  geom_text(aes(label = dollar(fine)), colour = "white",size = 5.5)+
  scale_fill_continuous(labels = dollar) +
  labs(x = "Issuing agency",y = "Parking violation",
       title = "¿Where does the money goes?",
       fill = "Total fine earned") + 
  theme_economist()+
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.title = element_text(),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)
        )


# First I group the two bar plots:

plot_1 <- ggarrange(frecuencies[[1]],frecuencies[[2]])

# Add the tiles:

plot_2 <- ggarrange(plot_1,tiles,nrow =2)

# Annotate the figure

final_plot <- annotate_figure(plot_2,
                              bottom = text_grob(" Data source: Jess Streeter,  Philly Open Data \n Elaboration: Alex Bajaña @AlexBajaa5",
                                                 hjust = 1, x = 1, face = "italic", size = 10))

# Generate the png file:

png("week_49/joint_plot.png",res = 250,units = "in",width = 20,height = 8)

final_plot

dev.off()
