library(MASS)
library(janitor)
library(tidyverse)
library(broom)

ls(data_set)


# USA va a ser la categoría de referencia en el país:
# Los paises libres serán la categoria de referencia para el status:
# Los paises desarrollados serán la categoría de referencia para "Least developed country"

freedom_index_data <- tidytuesdayR::tt_load('2022-02-22')

data_set <- freedom_index_data$freedom

levels_country <- data_set %>% pull(country) %>% 
  unique 

levels_country <- levels_country[c(185,1:184,186:193)]

colores <- 
  c('Africa.0' = '#1F78B4',
    'Africa.1' = '#b45b1f',
    'Americas.0' = '#33A02C',
    'Americas.1' = '#992ca0',
    'Asia.0' = '#d24f4d',
    'Asia.1' = '#63ccce',
    'Europe.0' = '#a59b85',
    'Oceania.0' = '#ac56a0',
    'Oceania.1' = '#6d9a3d')


data_set <- data_set %>%
  clean_names() %>% 
  filter() %>% 
  mutate(index = cl + pr,
         index = factor(index),
         country = factor(x = country,levels = levels_country),
         status = factor(status, levels = c("F","NF","PF")),
         is_ldc = factor(is_ldc,levels = c(1,0)) ,
         region_name = factor(region_name))

data("wrld_simpl", package = "maptools")
world <- wrld_simpl %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs = "+proj=robin")


data_set <- data_set %>%  
  dplyr::mutate(
    country = as.character(country),
    country = dplyr::case_when(
      country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      country == "Cabo Verde" ~ "Cape Verde",
      country == "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire" ~ "Cote d'Ivoire",
      country == "Czechia" ~ "Czech Republic",
      country == "Eswatini" ~ "Swaziland",
      country == "Libya" ~ "Libyan Arab Jamahiriya",
      country == "Micronesia (Federated States of)" ~ "Micronesia, Federated States of",
      country == "Myanmar" ~ "Burma",
      country == "Democratic People's Republic of Korea" ~ "Korea, Democratic People's Republic of",
      country == "North Macedonia" ~ NA_character_,
      country == "Russian Federation" ~ "Russia",
      country == "Republic of Korea" ~ "Korea, Republic of",
      country == "South Sudan" ~ NA_character_,
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country == "United States of America" ~ "United States",
      country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      TRUE ~ country
    )
  ) %>% 
  dplyr::filter(!is.na(country)) %>% 
  dplyr::left_join(world, by = c("country" = "NAME"))


# la escala de Civil liberies `cl` y Political Rigths (pr) esta invertida:

liberty_chart_list <- data_set %>% 
  distinct(cl,pr,status,year) %>%
  arrange(year,cl,pr) %>% 
  pivot_wider(id_cols = c(year,cl),
              names_from = pr,
              values_from = status) %>% 
  split(.$year) %>% map(unnest)

flextable::flextable(liberty_chart_list$`2020`)

# Corregimos cambiamos el orden restando el valor de 8:

data_set <- data_set %>% 
  mutate(across(c(cl,pr),.fns = ~8-.x))

liberty_chart_list <- data_set %>% 
  distinct(cl,pr,status,year) %>%
  arrange(year,cl,pr) %>% 
  pivot_wider(id_cols = c(year,cl),
              names_from = pr,
              values_from = status) %>% 
  split(.$year) %>% map(unnest)

flextable::flextable(liberty_chart_list$`2020`)

# Vamos a modelar para el año 2019:

  
data_list <-   data_set %>% 
    split(.$year) %>% 
    map(~{
      
      data_model <- .x
      
      fit <- polr(formula = factor(index) ~ is_ldc  +  region_name,
                  data = data_model, method='logistic')
      
      probabilities <- predict(fit,data_model,type = "probs") %>% 
        as_tibble() %>%
        rename_with(.cols = everything(),
                    .fn = ~str_remove_all(.x,"[:punct:]") %>% 
                      str_c("category_",.)) %>% 
        bind_cols(data_model) %>% 
        dplyr::select(country,cl,pr,is_ldc,status,region_name,category_14,geometry) 
      
      # ??polr
      
      elements <- glance(fit)
      
      # residuals <- augment(fit)
      
      residuals <- residuals(fit)
      
      browser()
      
      return(list(probabilities,elements,residuals))
      
    })


# Pooled: -----------------------------------------------------------------

data_model <- data_set
# %>% 
#   filter(year == 2020)

fit <- polr(formula = factor(index) ~ is_ldc ,
            data = data_model, method='logistic', Hess=TRUE)

probabilities <- predict(fit,data_model,type = "probs") %>% 
  as_tibble() %>%
  rename_with(.cols = everything(),
              .fn = ~str_remove_all(.x,"[:punct:]") %>% 
                str_c("category_",.)) %>% 
  bind_cols(data_model) %>% 
  dplyr::select(year,country,cl,pr,is_ldc,status,region_name,category_14,geometry) 

# ??polr
plot(fit)

up <- update(fit, . ~ . + year:country)

deviance(fit) - deviance()

1-pchisq(q = deviance(up),df.residual(up))
1-pchisq(q = deviance(fit),df.residual(fit))


pr <- profile(fit)

plot(pr)

pairs(pr)

elements <- glance(fit)

# residuals <- augment(fit)

residuals <- residuals(fit)

chart <- probabilities %>% 
  mutate(indicador = glue::glue("{region_name}.{is_ldc}"),
         suma = cl + pr,
         # cl = cl/7,
         # pr = pr/7,
         index = (cl + pr)/14)

data_chart_2020 <- chart %>% 
  filter(year == 2020) 

data_chart_2010 <- chart %>%   
  filter(year == 2010) 
  

plot_map <- function(chart){ 
  
p <- ggplot() +
  geom_sf(data = chart %>% filter(region_name == "Africa"),
          aes(geometry = geometry, alpha = index, group = year,fill=indicador), 
          size = 0.05)+
  geom_sf(data = chart %>% filter(region_name == "Americas"),
          aes(geometry = geometry, alpha = index, group = year,fill=indicador), 
          size = 0.05)+
  geom_sf(data = chart %>% filter(region_name == "Asia"),
          aes(geometry = geometry, alpha = index, group = year,fill=indicador), 
          size = 0.05)+
  geom_sf(data = chart %>% filter(region_name == "Europe"),
          aes(geometry = geometry, alpha = index, group = year,fill=indicador), 
          size = 0.05)+
  geom_sf(data = chart %>% filter(region_name == "Oceania"),
          aes(geometry = geometry, alpha = index, group = year,fill=indicador), 
          size = 0.05) +
  # scale_alpha_continuous(range = c(0.2,1))+
  scale_fill_manual(values = colores)+
  # labs(title = "Probabilities of each country to achieve a perfect score",
  #      subtitle = str_c("Year:","2020"))+
  theme_minimal() + 
  theme(axis.text = element_blank(),
        legend.position = "none",
        title = element_blank())
return(p)

}


chart_2010 <- plot_map(data_chart_2010)

chart_2020 <- plot_map(data_chart_2020)

leyendas <- list(colores[1:2],
     colores[3:4],
     colores[5:6],
     colores[8:9],
     colores[7]) %>% 
  map(~{
    
    plot <- enframe(.x) %>% 
      ggplot() + 
      geom_col(aes(x = name,y = 1,fill = name)) + 
      scale_fill_manual(values = .x) +
      theme(
        rect = element_rect(fill = "transparent") ,
        legend.title = element_blank(),
            legend.text = element_text(size = 5),
            legend.key =   element_rect(size = 5),legend.margin = margin(0,0,0,0,unit = "cm"))
    
    legend <- cowplot::get_legend(plot)
      
    return(legend)
      })

legend_a <- ggdraw(leyendas[[1]])  
  
legend_b <- ggdraw(leyendas[[2]])

legend_c <- ggdraw(leyendas[[3]])  

legend_d <- ggdraw(leyendas[[4]])

legend_e <- ggdraw(leyendas[[5]])  

legenda_alpha <- tibble(value = seq(from = 1/14,to = 1,by = 1/14)) %>% 
  rowid_to_column() %>% 
  mutate(alpha = value) %>% 
  ggplot() + 
  geom_col(aes(x = "a",y = rowid,fill = value,alpha = alpha)) + 
  scale_fill_continuous(guide = "none") +
  scale_alpha_continuous(breaks = seq(from = 1/14,to = 1,by = 2/14),
                         label = percent) + 
  theme_minimal()+ 
  theme(
    legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = "left")

legenda_alpha <- cowplot::get_legend(legenda_alpha)

legenda_alpha <- ggdraw(legenda_alpha)

plot_grid(
  legenda_alpha_plot,
  plot_grid(
    ggdraw()+draw_text('Africa'),ggdraw()+draw_text('America'),
    legend_a,legend_b,
    ggdraw()+draw_text('Asia'),ggdraw()+draw_text('Oceania'),
            legend_c,legend_d,
    ggdraw()+draw_text('Europa'),ggdraw()+draw_text(''),
            legend_e,nrow = 3,
            ncol = 2,
            rel_widths = c(1,1),
            rel_heights = rep(c(1,2),3)),
  nrow = 2,rel_heights = c(1,2)
)


mapas <- (chart_2010 / chart_2020)

legenda_alpha_plot <- 
  legenda_alpha

legends_continente <- 
  ((wrap_elements(grid::textGrob('Asía'))/
  legend_a) | 
  (wrap_elements(grid::textGrob('America'))/
  legend_b)) / 
  ((wrap_elements(grid::textGrob('Africa'))/
  legend_c) |
  (wrap_elements(grid::textGrob('Oceania'))/
  legend_d)) /
  ((wrap_elements(grid::textGrob('Europa'))/
  legend_e) | plot_spacer()) 

legends <- legenda_alpha_plot/
  legends_continente

patchwork_alex <- mapas | legends +
  plot_layout(widths = c(2,1))
  

ggsave(plot = patchwork_alex,width = 90,height = 70,units = "cm",
       filename  = "2022/22022022/LIBERTY_PROVE.png",dpi = 100)


library(patchwork)

??patchwork

plots_comparados <- chart_2010 + chart_2020
 





animation::saveGIF(expr = iwalk(.x = data_list ,
                                ~{
                                  
                                  chart <- .x %>% 
                                    mutate(year = .y)
                                  
                                  
                                  p <- ggplot() +
                                    geom_sf(data = chart %>% filter(region_name == "Africa"),
                                            aes(geometry = geometry, alpha = category_14, group = year), 
                                            size = 0.05, fill ="red")+
                                    geom_sf(data = chart %>% filter(region_name == "Americas"),aes(geometry = geometry, alpha = category_14, group = year), 
                                            size = 0.05, fill ="yellow")+
                                    geom_sf(data = chart %>% filter(region_name == "Asia"),aes(geometry = geometry, alpha = category_14, group = year), 
                                            size = 0.05, fill ="orange")+
                                    geom_sf(data = chart %>% filter(region_name == "Europe"),aes(geometry = geometry, alpha = category_14, group = year), 
                                            size = 0.05, fill ="blue")+
                                    geom_sf(data = chart %>% filter(region_name == "Oceania"),aes(geometry = geometry, alpha = category_14, group = year), 
                                            size = 0.05, fill ="green") +
                                    scale_alpha_continuous(range = c(0,0.6))+
                                    labs(title = "Probabilities of each country to achieve a perfect score",
                                         subtitle = str_c("Year:",.y))+
                                    theme_minimal() + 
                                    theme(axis.text = element_blank(),
                                          legend.position = "top",
                                          legend.direction = "horizontal")
                                  
                                  # browser()
                                  plot(p)
                                  
                                  cli::cli_process_done()
                                }),
                   movie.name = "freedom_in_the_world.gif",
                   interval = 0.5,
                   ani.width = 1985,
                   ani.height = 1500,
                   ani.res = 300)


