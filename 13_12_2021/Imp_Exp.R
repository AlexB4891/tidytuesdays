###########################################################################
###                           LIBRARIES                                 ###
###########################################################################

library(data.table)
library(lubridate)
library(tidyverse)

###########################################################################
###                   ESTIMATE THE TOTAL FLOWS                          ###
###########################################################################

# list_of_files <- list.files(path = ".", recursive = TRUE,
#                             pattern = "\\.csv$", 
#                             full.names = TRUE)
# 
# 
# List_countries<-sapply(list_of_files, fread, simplify = FALSE)
# colnames<-c("Date", "Imp", "Exp")
# 
# List_countries<-lapply(List_countries, setNames, colnames) #Step 1: change the name in each list. The easiest is change the name in all columns in order to bind the rows
# 
# List_countries<-rbindlist(List_countries,
#                           use.names = TRUE, idcol = "List_countries") #Step 2: Bind the data into a only one tibble

load("13_12_2021/Data.RData")

# Imp_Exp<-
  tibble(List_countries)%>%
  mutate_at(c(3:4), as.numeric)%>% #Step 3: Change the Imp and Exp variables to numeric vectors
  mutate(Date= as.Date(Date,format = "%d.%m.%Y %H:%M")) %>% 
  filter(Date>ymd("20180131"))#Step 4: Create my sample. The use of "" is not allowed because R recognize that as a character
# ,
#          Date= as.Date(Date),
#          List_countries=substr(List_countries, 1, 9))%>%
  

