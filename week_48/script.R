# -------------------------------------------------------------------------#
#                       Analysing student loan debts                       #
# -------------------------------------------------------------------------#
# Elaborated by: Alex Bajaña

# Libraries:

library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(scales)
library(gganimate)
library(lubridate)
library(ggthemr)

# remotes::install_github("https://github.com/cttobin/ggthemr",force = TRUE)




# Get the data:

sld <- tidytuesdayR::tt_load("2019-11-26")
sld <- sld$loans


# First transformations: --------------------------------------------------
# 1. Change the value of the variable quarter for 
#    it's equivalent in months
# 2. Create a date variable with format %y%m%d

sld_data <- sld %>% 
  mutate(quarter = case_when(quarter == 1 ~ "03",
                             quarter == 2 ~ "06",
                             quarter == 3 ~ "09",
                             TRUE         ~ "12"),
         date = str_c("20",year,"-",quarter,"-01")) 
  
  

# Clasification of agencies by size  --------------------------------------
# 1. I replace all NA values with 0
# 2. Then, replace symbols and sufixes in order to group all values by agency
# 3. After aggragete the total debt acquired, the mean of this vaiable during 
#    the study period will give us an idea of agency size
# 4. Using the 33, 66, quantiles I create a factor with cut function
# 5. I relabel my factor in order to get agency size

  a_size <- sld_data %>%
    mutate_if(is.double,funs(replace_na(.,replace = 0))) %>% 
    mutate(total_debt = starting + added,
           agency_name = str_replace_all(agency_name,"\\.|\\,|\\*|[Ii]nc|LP$|Professionals","") %>% 
             str_trim("both")) %>% 
    group_by(agency_name) %>% 
    summarise(total_debt = mean(total_debt,na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(agency_size = cut(total_debt,breaks = c(0,quantile(total_debt,probs = c(0.33,0.66,1)))),
           agency_size = fct_relabel(agency_size,function(x)c("Small","Medium","Big"))) %>% 
    split(.$agency_size) %>% 
    map(pull,agency_name)
  

# Final transformations ----------------------------------------------------
# 1. The types of payments are created following the idea that consolidation and
#    loan rehabilitations are forms of renegotation of the original debt, so
#    voluntary payments and wage garnishments are effective payments
# 2. Total debt is defined by the starting value at each qaurter plus the added values
# 3. Using the element a_size which is a list that contains the agencies in each 
#    agency size I clasificate the agencies in the complete table
# 4. Reorder the factor using the total_debt value
# 5. Scaling tha numeric values divinding them by one millon


  final_table <- sld_data %>% 
    mutate_if(is.double,funs(replace_na(.,replace = 0))) %>% 
    mutate(renogotation        = consolidation + rehabilitation ,
           effective_payments  = voluntary_payments + wage_garnishments,
           total_debt          = starting + added,
           date                = ymd(date),
           agency_size         = case_when(agency_name %in% a_size[["Small"]] ~ "Small",
                                           agency_name %in% a_size[["Medium"]] ~ "Medium",
                                           TRUE                                 ~ "Big")
           ) %>% 
    select(year,quarter,renogotation,
           effective_payments, agency_size,
           total_debt,agency_name,date) %>% 
    mutate(agency_size = fct_reorder(agency_size,total_debt)) %>% 
    mutate_if(is.numeric,funs(./10e6))


# Plotting ----------------------------------------------------------------
# For this case I will create a ggplot object using the geom_point function
# then I will animate that plot in order to watch the evolution of each 
# agency size along time 
  
  anim <-
    final_table  %>% 
    ggplot()+
    geom_point(aes(x= renogotation,
                   y = effective_payments,
                   size = total_debt,
                   color = agency_size)) +
    scale_x_continuous(labels = dollar)+
    scale_y_continuous(labels = dollar)+
    scale_size(labels = dollar,range = c(1,10))+
    labs(title = "Total student's loans debt by payment type and agendy size",
         subtitle = "Quarter: {frame_time}",
         x = "Renegotation with lender [MM USD]",
         y = "Effective payments [MM USD]",
         size = "Total debt [MM USD]",
         color="Agency size")+
      theme(axis.text.x = element_text(angle  =90))+
    facet_wrap(agency_size~.,scales = "free")  +
    transition_time(date)
  
# Set the theme with ggthemr package

  ggthemr("earth", type="outer", layout="scientific", spacing=2)
  
# Animate my plot:
  
    animate(anim, height = 500, width =850)
    
# Save my animation
  
  gganimate::anim_save(filename = "week_48/sld.gif", ani.width= 500, ani.height=850)
