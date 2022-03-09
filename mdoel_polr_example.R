house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
house.plr
summary(house.plr, digits = 3)
## slightly worse fit from
summary(update(house.plr, method = "probit", Hess = TRUE), digits = 3)
## although it is not really appropriate, can fit
summary(update(house.plr, method = "loglog", Hess = TRUE), digits = 3)
summary(update(house.plr, method = "cloglog", Hess = TRUE), digits = 3)

predict(house.plr, housing, type = "p")
addterm(house.plr, ~.^2, test = "Chisq")
house.plr2 <- stepAIC(house.plr, ~.^2)
house.plr2$anova
anova(house.plr, house.plr2)

house.plr <- update(house.plr, Hess=TRUE)
pr <- profile(house.plr)

residuals(fit)
confint(pr)
plot(pr)

pairs(pr)


copen <- read.table("http://data.princeton.edu/wws509/datasets/copen.dat")

data_polr <- data_model %>% 
  group_by(country,is_ldc,year,cl) %>% 
  summarise(countries = n_distinct(country))



modelo_entendido <- polr(formula = factor(cl) ~ is_ldc + year ,
     weights = countries,
     data = data_polr )


modelo_2 <- update(modelo_entendido,.~.+year:country)

modelo_3 <- update(modelo_entendido,.~.+year:country +year:is_ldc )

pr <- profile(modelo_entendido)

plot(pr)

pairs(pr)

1-pchisq(q = deviance(modelo_entendido),df.residual(modelo_entendido))
1-pchisq(q = deviance(modelo_2),df.residual(modelo_2))
1-pchisq(q = deviance(modelo_3),df.residual(modelo_3))

deviance(modelo_entendido) - deviance(modelo_2)
deviance(modelo_entendido) - deviance(modelo_3)
deviance(modelo_2) - deviance(modelo_3)

2*(logLik(modelo_entendido) - logLik(modelo_2))
2*(logLik(modelo_entendido) - logLik(modelo_3))

AIC(modelo_entendido)
AIC(modelo_2)
AIC(modelo_3)




augment(modelo_entendido)

probabilities <- predict(modelo_entendido,data_polr,type = "probs") %>% 
  as_tibble() %>%
  rename_with(.cols = everything(),
              .fn = ~str_remove_all(.x,"[:punct:]") %>% 
                str_c("category_",.)) %>% 
  bind_cols(data_model)%>%
  dplyr::select(year,country,is_ldc,status,region_name,category_PF,geometry)

probabilities %>% rename(index = category_PF) %>% mutate(indicador = glue::glue("{region_name}.{is_ldc}")) %>% filter(year == 2020) %>%  plot_map
