
# -------------------------------------------------------------------------#
#                   TIYTUESDAY WEEK 47: 2019 Bird of the Year              #
# -------------------------------------------------------------------------#

# Elaboration: Alex Bajaña

# Libraries:

library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)
library(wesanderson)

# Load data:

vote_data <- tt_load("2019-11-19")

# I use the non reshaped data:

vote_data <- vote_data$`BOTY-votes-2019`

# I created a function to determine the winner:

get_winner <- function(vote_data){
  
  # Setup:
  
  vote_data <- vote_data %>%
    mutate(vote_f = vote_1,
           times = 0)
  
  
  top_5 <- list()
  
  i = 0
  
  # Loop that recreate each round:
  repeat({
    
    i = i + 1
    
    # Counting votes by candidates:
    
    count <-  vote_data %>% 
      group_by(vote_f) %>% 
      tally %>% 
      arrange(desc(n)) 
    
    # Get the winner of the round:
    ganador <- count[1,] 
    
    
    # Get the loser of the round:
    perdedor <- count$vote_f[nrow(count)]
    
    # Calculate the mid value of each round:
    
    middle <- nrow(vote_data)/2
    
    # Condition:
    if(ganador$n < middle){
      
      # with mutate at i change the votes for the loser to NA
      # I create an index for each time the vote of each person is changed
      # until it turns in an NA.
      # Then if the loser create a hole in the voting preferences of a person
      # I replace it with the next prefered candidate:
      # Then I replace my final vote variable using the jararchy logic
      # of voting:
      
      vote_data <- vote_data %>% 
        mutate_at(vars(matches("vote_[0-9]")),funs(if_else(.==perdedor,NA_character_,.))) %>% 
        mutate(times = if_else(vote_f == perdedor,times + 1,times),
               vote_1 = if_else(is.na(vote_1) & !is.na(vote_2), vote_2, vote_1),
               vote_2 = if_else(is.na(vote_2) & !is.na(vote_3), vote_3, vote_2),
               vote_3 = if_else(is.na(vote_3) & !is.na(vote_4), vote_4, vote_3),
               vote_4 = if_else(is.na(vote_4) & !is.na(vote_5), vote_5, vote_4),
               vote_f = case_when(
                 vote_f == perdedor & times == 1 ~ vote_2,
                 vote_f == perdedor & times == 2 ~ vote_3,
                 vote_f == perdedor & times == 3 ~ vote_4,
                 vote_f == perdedor & times == 4 ~ vote_5,
                 vote_f == perdedor & times >  4 ~ vote_f,
                 TRUE                            ~ vote_f
               )) %>% 
        filter(!is.na(vote_f),vote_f != perdedor)
      
      # Condition met:
      
    }else{
      break
    }
    
    # To get the top 5 (consider that in the lasts round could be less than 5 birds)
    
    if(nrow(count)<5){
      n <- nrow(count)
    }else{
      n <- 5
    }
    
    # I keep each top 5 of each round:
    top_5[[i]] <- count[1:n,] %>% mutate(iteration = i,size = nrow(vote_data))
    
    # print(paste("Ganador con:", round((ganador$n/nrow(bird_data))*100,2))) 
    
    top_5
  })
  
  # Return:
  
  top_5
}


# Get the general results:

general <- get_winner(vote_data)

general <- general %>% 
  reduce(rbind) %>% 
  mutate(week = "Total")


# Get the weekly results:
separada <- vote_data %>% 
  mutate(week = if_else(between(date,ymd("20191028"),ymd("20191103")),"Week 1","Week 2")) %>% 
  split(.$week) %>% 
  map(get_winner)

separada <- separada %>% 
  imap(~.x %>% 
         reduce(rbind) %>% 
         mutate(week = .y) 
      )%>% 
  reduce(rbind)


# Ploting:

best_bird_plot <- rbind(general,separada) %>% 
  mutate(proportion = n/size) %>% 
  ggplot(aes(x = iteration,y=proportion,color=vote_f))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette = "Dark2")+
  scale_y_continuous(labels = percent)+
  labs(title = "Results of NZ bird of the year voting 2019",
       subtitle = "Comparison of total results and weekly results",
       x = "Round",
       y = "Porpotion of votes",
       caption = "Source: Nathan Moore, Dragonfly Data Science\nMade by: Alex Bajaña",
       color = "Bird breed")+
  theme_light()+
  theme(legend.position = "bottom")+
  facet_grid(week~.)

png("best_bird.png",units = "in",res = 250,height = 8,width = 12)

best_bird_plot

dev.off()

