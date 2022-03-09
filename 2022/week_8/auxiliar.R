library ( MASS )
library ( nnet )
comen.f = factor ( c ( " peces " ," invert " ," reptiles " ," pajaritos " ," otros " ) ,
                   levels = c ( " peces " ," invert " ," reptiles " ," pajaritos " ," otros " ))
tamano.f = factor ( c ( " <2 .3 " , " >2 .3 " ) , levels = c ( " >2 .3 " , " <2 .3 " ))
sexo.f = factor ( c ( " m " ," f " ) , levels = c ( " m " ," f " ))

charca.f = factor ( c ( " hancock " , " oklawaha " ," trafford " , " george " ) ,
                    levels = c ( " george " ," hancock " ," oklawaha " ," trafford " ))
temp = c (7 , 1 , 0 , 0 , 5 , 4 , 0 , 0 , 1 , 2 , 16 , 3 , 2 , 2 , 3 , 3 , 0 , 1 , 2 , 3 ,
          2 , 2 , 0 , 0 , 1 , 13 , 7 , 6 , 0 , 0 , 3 , 9 , 1 , 0 , 2 , 0 , 1 , 0 , 1 , 0 , 3 , 7 , 1 ,
          0 , 1 , 8 , 6 , 6 , 3 , 5 , 2 , 4 , 1 , 1 , 4 , 0 , 1 , 0 , 0 , 0 , 13 , 10 , 0 , 2 , 2 ,
          9 , 0 , 0 , 1 , 2 , 3 , 9 , 1 , 0 , 1 , 8 , 1 , 0 , 0 , 1)
tabla.array = expand.grid ( comen = comen.f , tamano = tamano.f , sexo = sexo.f ,
                            charca = charca.f )
tabla = tapply ( temp , tabla.array [ , 1:4] , sum )



chart <- data_list$`2020` %>% 
  mutate(year = 2020)
  
  
  p <- ggplot() +
  geom_sf(data = chart %>% filter(region_name == "Africa"),
          aes(geometry = geometry, alpha = cl, group = year), 
          size = 0.05, fill ="red")+
  geom_sf(data = chart %>% filter(region_name == "Americas"),aes(geometry = geometry, alpha = cl, group = year), 
          size = 0.05, fill ="yellow")+
  geom_sf(data = chart %>% filter(region_name == "Asia"),aes(geometry = geometry, alpha = cl, group = year), 
          size = 0.05, fill ="orange")+
  geom_sf(data = chart %>% filter(region_name == "Europe"),aes(geometry = geometry, alpha = cl, group = year), 
          size = 0.05, fill ="blue")+
  geom_sf(data = chart %>% filter(region_name == "Oceania"),aes(geometry = geometry, alpha = cl, group = year), 
          size = 0.05, fill ="green") +
  scale_alpha_continuous(range = c(0,0.6))+
  labs(title = "Probabilities of each country to achieve a perfect score",
       subtitle = str_c("Year:","2020"))+
  theme_minimal() + 
  theme(axis.text = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal")
