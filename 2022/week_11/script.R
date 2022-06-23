library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(showtext)
library(ggpubr)
library(ggforce)

# Sys.setlocale("LC_ALL","English")

font_add_google(name = "Fredoka", family = "fred")

showtext_auto()

tuesdata <- tidytuesdayR::tt_load("2022-03-15")

cran_data <- tuesdata$cran

cran_list <- cran_data %>% 
  mutate(
    date = str_remove(date,"UTC|CDT") %>% str_trim,
    # date = if_else(date == "0",NA_character_,date),
    date_text = str_detect(date,"[A-z]")) %>% 
  split(.$date_text)

cran_list <- 
  cran_list %>% 
  map_at(.at = 1,
         .f = ~ .x %>% 
           mutate(date = str_sub(date,1,19))) %>% 
  map2(
                  .y = c("%Y-%m-%d %H:%M:%S",
                           "%a %b %d %H:%M:%S %Y"),
     ~.x %>% 
       mutate(date_f = as.Date(x = date,format = .y))
     
       ) %>% 
  reduce(bind_rows) 


errores <- cran_list %>% 
  filter(is.na(date_f)) %>% 
  pull(date) %>% 
  unique()

# Aun hay fechas que debemos corregir:


cran_list <- cran_list %>% 
  mutate(date_f = if_else(date %in%  errores,
                          true = ymd(str_sub(date,1,10)),
                          false = date_f))




cran_list <- cran_list %>% 
  mutate(date_f = floor_date(date_f,unit = "month"),
         indicador = case_when(rnw > 0 & rmd == 0  ~ "Documented with Rnw",
                               rnw == 0 & rmd > 0  ~ "Documented with Rmd",
                               rnw > 0 & rmd > 0   ~ "Documented with conbination",
                               rnw == 0 & rmd == 0 ~ "Not documented"),
         indicador = factor(indicador,levels = c("Documented with Rnw",
                                                 "Documented with Rmd",
                                                 "Documented with conbination",
                                                 "Not documented"))) 

latest <- max(cran_list$date_f,na.rm = T)

cran_list_l <- cran_list %>% 
  split(.$package) %>% 
  map(~{
    
    if(!is.infinite(min(.x$date_f,na.rm = T))){
    completa <- tibble(date_f = seq(min(.x$date_f,na.rm = T),latest,"1 month")) %>% 
      left_join(.x) %>% 
      fill(rnw,rmd,version,package,indicador)
    }else{
    completa <- .x  
    }
    return(completa)
  })

cran_list_l$purrr

cran_list <- cran_list_l %>% 
  reduce(bind_rows)


cran_list <- cran_list %>% 
  group_by(date_f,indicador) %>% 
  summarise(packages = n_distinct(package)) %>% 
  filter(date_f >= ymd("2004-01-01")) %>% 
  arrange(date_f) 

cran_list <- cran_list %>% 
  mutate(indicador = str_replace(indicador,"conbination","combination"))





plot_1 <- cran_list %>% 
  ggplot() +
  geom_area(aes(x = date_f,y = packages,fill = indicador),position = "stack") +
  geom_point(mapping = aes(x = ymd("2015-09-01"),y = 6000),size = 0.4,show.legend = FALSE) +
  scale_y_continuous(labels = number) + 
  labs(title = "(A) Evolution of the numer of packages")


plot_2 <- cran_list %>% 
  ggplot() +
  geom_col(aes(x = date_f,y = packages,fill = indicador),position = "fill") +
  scale_y_continuous(labels = percent) + 
  labs(title = "(B) Composition of the CRAN repository")

message <- str_c("In september 2015 purrr, my favorite package was uploaded",
                 "for the first time with no vignette, In it's version 0.2.3",
                 " a vignette was added for us to use",sep = "\n")



# text_grob <- ggpubr::text_grob(label = message,size = 10,family = "fred",lineheight = 0.4)


plot_1 <- plot_1 +
  inset_element(p = text_grob,
                left = -0.1,
                bottom = 0.55,
                right = 0.9,
                top = 0.95) 

patch <- plot_1 + plot_2 + 
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Presence of vignettes in the CRAN repository",
                  subtitle = "The data presented is the number of packages avaible in the CRAN in each month",
                  caption = "Tidytuesdays, data from  Robert Flight.| Elaborated by: Alex Bajaña| Twitter: @AlexBajaa5") &
  scale_fill_manual(values = c("#6E9AE5","#3970CD","#032E95","#950378")) &
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(family = "fred",size = 15),
        axis.text  = element_text(family = "fred",size = 20),
        plot.title = element_text(family = "fred",size = 25),
        legend.title = element_blank()) 

ggsave(plot = patch,filename = "2022/week_11/tt_cran_repository.png",bg = "white")



  

