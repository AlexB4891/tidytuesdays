library(tidytuesdayR)
library(tidyverse)
library(forecast)

investiment_Data <- tidytuesdayR::tt_load(2021, week = 33)

sysfonts::font_add_google("Roboto", "Roboto")
showtext::showtext_auto()


investiment_Data$ipd %>%
  count(category,meta_cat) %>%
  View

health_investment <- investiment_Data$chain_investment %>%
  filter(meta_cat == "Health")

variation <- health_investment %>%
  pivot_wider(names_from = category,values_from = gross_inv_chain) %>%
  arrange(year) %>%
  mutate(across(.cols = 4:10,
                .fns = list(
                  var_1 = ~ (.x/lag(.x,n = 1) -1),
                  var_5 = ~ (.x/lag(.x,n = 5) -1),
                  var_10 = ~ (.x/lag(.x,n = 10) -1)
                )),
         across(.cols = matches("var_"),
                .fns = ~if_else(.x > 1,NA_real_,.x)))


plot_dataset <- variation %>%
  pivot_longer(cols = 4:31,names_to = "concept",values_to = "variation") %>%
  mutate(var_type = str_extract(concept,"_var_.+"),
         concept = str_replace(concept,"_var_.+",""),
         var_type = if_else(is.na(var_type),
                            true = "level",
                            false = str_replace(var_type,"^_",""))) %>%
  filter(str_detect(concept,"hospital")) %>%
  mutate(var_type = factor(var_type,levels = c("level","var_1","var_5","var_10")))


plot_dataset %>%
  filter(str_detect(var_type,"level") ) %>%
  pivot_wider(names_from = concept,values_from = variation) %>%
  mutate(across(.cols = 5:6,.fns = list(trend = ~ ma(.x,order = 4,centre = T)))) %>%
  pivot_longer(cols = 5:8,names_to = "concept",values_to = "variation") %>%
  ggplot() +
  geom_line(aes(x = year,y = variation,color = concept)) +
  facet_wrap(.~var_type,scales = "free")
