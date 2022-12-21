#install.packages("worldmet")
library(worldmet)
library(lubridate)
library(tidyverse)
library(giosimport)
library(purrr)
library(stringr)
library(dplyr)
library(openair)
library(gridExtra)
library(devtools)
library(seasonal)
library(seasonalview)
library(gridExtra)
#install.packages("forecast")
library(forecast)
if (!require(fpp3))     {install.packages("fpp3");     require(fpp3)}
if (!require(ggplot2))  {install.packages("ggplot2");  require(ggplot2)}
if (!require(GGally))   {install.packages("GGally");  require(GGally)}
library(feasts)
library(lubridate)
library(fpp3)
library(GGally)
library(car)

getMeta(lat = 52.42, lon = 16.82 , plot = T, returnMap = T)

####kod stacji 123300-99999
data <- importNOAA(code = '123300-99999', year = 2012:2020)


###przesunięcie daty
data$date <- data$date + hours(1)

str(daneNOAA)

###selekcja danych
daneNOAA <- data[1:15]

daneNOAA <- daneNOAA %>% select(-code, -station, -latitude, -longitude, -cl_1 )


dane_f <- daneGIOS
dane_f <- left_join(dane_f, daneNOAA, by = "date")

dane_f <- dane_f %>% na.omit() %>% 
  select(date, obs, ws, air_temp, dew_point, atmos_pres, RH, visibility)

dane_m <- dane_f %>% timeAverage(avg.time = "month")


dane_m <- dane_m %>% 
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(index = date)

dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(obs, .direction = "down")


dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(ws, .direction = "down")


dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(air_temp, .direction = "down")

dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(dew_point, .direction = "down")


dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(visibility, .direction = "down")

dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(atmos_pres, .direction = "down")

dane_m <- dane_m %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(RH, .direction = "down")

dane_m %>% gg_season(obs)


dane_m <- dane_m %>%  rename(pm10 = obs)

dane <- dane_m

### modele

model
m1 <- dane %>% model(MEAN(pm10)) ; m1

# **Metoda Naiwne (prosta)** - 'Naiwne' ponieważ zakłądają, że czynniki określające
# zmienną prognozowaną są stałe (niezmienne). Prognozą jest wartość ostatniej
# obserwacji. Tak zwane prognozy losowego marszu.

m2 <- dane %>% model(NAIVE(pm10)) ; m2

# **Metoda naiwna (sozenowa)** - prognoza jest równan ostatniej obserwacji z
# każdego sezonu (pory roku, miesiaca, tygodnia itd..)

m3 <- dane %>% model(SNAIVE(pm10 ~ lag(6))) # lag - definiuje długość okresu sezonowego

# **Metaoda dryfu (naiwna)** -  prognozy mogą rosnąć lub maleć z czasem, przy
# czym zmianan ta jest średnią zmianą danych historycznych.

m4 <- dane %>% model(RW(pm10 ~ drift()))
# wyniki prognoz

gridExtra::grid.arrange(m1 %>% forecast() %>% autoplot(dane) + ggtitle("Średnia"),
                        m2 %>% forecast() %>% autoplot(dane) + ggtitle("Naiwna"),
                        m3 %>% forecast() %>% autoplot(dane) + ggtitle("Naiwna z trędem"),
                        m4 %>% forecast() %>% autoplot(dane) + ggtitle("Dryf"))

### RESZTY

## SNAIVE

fit_res_sn <- dane %>% 
  model(SNAIVE(pm10)) %>% 
  augment()

fit_res_sn %>% 
  autoplot(.resid)

fit_res_sn %>% 
  ggpubr::gghistogram('.resid')

fit_res_sn %>% 
  ACF(.resid) %>% 
  autoplot()

dane_m %>% 
  model(MEAN(pm10)) %>% 
  gg_tsresiduals()


shapiro.test(fit_res_sn$.resid) #reszty sa normlane p < o.o5


car::qqPlot(fit_res_sn$.resid)



fit <- dane_m %>% model(SNAIVE(pm10))

fit %>% tidy()

fit %>% 
  augment() %>% 
  features(.resid, ljung_box, lag = 6, dof = 1)


###OKRE?LENIE ZBIOR?W
treningowy <- dane_m %>% filter(year(date) <= 2019)

testowy <- dane_m %>% filter(year(date) == 2020)

###SELEKCJA INTERESUJ?CYCH DANYCH

dane_m %>% 
  GGally::ggpairs(columns = 2:ncol(.), title = "Macierz Korelacji")

###ZLOGARYTMOWANE PM10 KORELACJA

dane_m %>% mutate(pm10 = log10(pm10)) %>%
  GGally::ggpairs(columns = 2:ncol(.), title =  "Macierz korelacji (log10(PM10)")



###OPOZNIENIA

dane_m %>% gg_lag(geom = "point", lags = seq(2,12,2)) ### NAJLEPSZY LAG 12

###ZACHOWANIE PREDYKTOWRÓW NA PRZESTRZENI LAT

grid.arrange(
             dane_m %>% ACF(air_temp, lag_max = 12*10) %>% autoplot() + ggtitle("Temperatura"),
             dane_m %>% ACF(visibility, lag_max = 12*10) %>% autoplot() + ggtitle("Widoczność"),
             dane_m %>% ACF(dew_point, lag_max = 12*10) %>% autoplot() + ggtitle ("Punkt rosy"),
             ncol =2)

###KORELACJA Z TEMP,VISIBILITY,DEW_POINT,

sd <- sd(dane_m$pm10)

###REGRESJA LINIOWA PROSTA


list(mod1 = pm10 ~ air_temp,      
     mod2 = pm10 ~ visibility,  
     mod3 = pm10 ~ dew_point,     
     mod4 = log(pm10) ~ air_temp,
     mod5 = log(pm10) ~ visibility,
     mod6 = log(pm10) ~ dew_point,
     mod7 = log10(pm10) ~ air_temp,
     mod8 = log10(pm10) ~ visibility,
     mod9 = log10(pm10) ~ dew_point,
     mod10 = pm10^2 ~ air_temp,
     mod11 = pm10^2 ~ visibility,
     mod12 = pm10^2 ~ dew_point,
     mod13 = pm10/sd ~ air_temp,
     mod14 = pm10/sd ~ visibility,
     mod15 = pm10/sd ~ dew_point) -> modele_proste

map(modele_proste, as.formula)  -> modele_proste  

###WYB?R MODELI

proste <- map(.x = modele_proste, 
            .f = ~model(treningowy, TSLM(formula = .x)) %>% glance()) %>% 
  do.call(rbind, .) %>%  
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) 

proste %>% 
  mutate(.model = modele_proste) %>% 
  arrange(AIC) %>% 
  knitr::kable(digits = 2)

###REGRESJA WIELORAKA

list(mod1 = pm10 ~ air_temp + visibility,
     mod2 = pm10 ~ air_temp + dew_point,
     mod3 = pm10 ~ visibility + dew_point,
     mod4 = log(pm10) ~ air_temp + visibility,
     mod5 = log(pm10) ~ air_temp + dew_point,
     mod6 = log(pm10) ~ visibility + dew_point,
     mod7 = log10(pm10) ~ air_temp + visibility,
     mod8 = log10(pm10) ~ air_temp + dew_point,
     mod9 = log10(pm10) ~ visibility + dew_point,
     mod10 = pm10^2 ~ air_temp + visibility,
     mod11 = pm10^2 ~ air_temp + dew_point,
     mod12 = pm10^2 ~ visibility + dew_point,
     mod13 = pm10/sd ~ air_temp + visibility,
     mod14 = pm10/sd ~ air_temp + dew_point,
     mod15 = pm10/sd ~ visibility + dew_point,
     mod16 = pm10 ~ air_temp + visibility + dew_point,
     mod17 = log(pm10) ~ air_temp + visibility + dew_point,
     mod18 = log10(pm10) ~ air_temp + visibility + dew_point,
     mod19 = pm10^2 ~ air_temp + visibility + dew_point,
     mod20 = pm10/sd ~ air_temp + visibility + dew_point) -> wielorakie

map(wielorakie, as.formula)  -> wielorakie  

###WYB?R MODELI

wielorakie_m <- map(.x = wielorakie, 
              .f = ~model(treningowy, TSLM(formula = .x)) %>% glance()) %>% 
  do.call(rbind, .) %>%  
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) 

wielorakie_m %>% 
  mutate(.model = wielorakie) %>% 
  arrange(AIC) %>% 
  knitr::kable(digits = 2)

###WIELORAKA Z TRENDEM I SESONOWO?CI?

list(mod41 = pm10 ~ air_temp + visibility + trend() + season(),
     mod42 = pm10 ~ air_temp + dew_point + trend() + season(),
     mod43 = pm10 ~ visibility + dew_point + trend() + season(),
     mod44 = log(pm10) ~ air_temp + visibility + trend() + season(),
     mod45 = log(pm10) ~ air_temp + dew_point + trend() + season(),
     mod46 = log(pm10) ~ visibility + dew_point + trend() + season(),
     mod47 = log10(pm10) ~ air_temp + visibility + trend() + season(),
     mod48 = log10(pm10) ~ air_temp + dew_point + trend() + season(),
     mod49 = log10(pm10) ~ visibility + dew_point + trend() + season(),
     mod50 = pm10^2 ~ air_temp + visibility + trend() + season(),
     mod51 = pm10^2 ~ air_temp + dew_point + trend() + season(),
     mod52 = pm10^2 ~ visibility + dew_point + trend() + season(),
     mod53 = pm10/sd ~ air_temp + visibility + trend() + season(),
     mod54 = pm10/sd ~ air_temp + dew_point + trend() + season(),
     mod55 = pm10/sd ~ visibility + dew_point + trend() + season(),
     mod56 = pm10 ~ air_temp + visibility + dew_point + trend() + season(),
     mod57 = log(pm10) ~ air_temp + visibility + dew_point + trend() + season(),
     mod58 = log10(pm10) ~ air_temp + visibility + dew_point + trend() + season(),
     mod59 = pm10^2 ~ air_temp + visibility + dew_point + trend() + season(),
     mod60 = pm10/sd ~ air_temp + visibility + dew_point + trend(),
     mod99 = log10(pm10) ~ air_temp + trend() + season(),
     mod98 = log10(pm10) ~ air_temp + season()) -> wieloraka_sezon_trend
     
map(wieloraka_sezon_trend, as.formula)  -> wieloraka_sezon_trend 


###WYB?R

wieorakiest <- map(.x = wieloraka_sezon_trend, 
              .f = ~model(treningowy, TSLM(formula = .x)) %>% glance()) %>% 
  do.call(rbind, .) %>%  
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) 

tabela <- wieorakiest %>% 
  mutate(.model = wieloraka_sezon_trend) %>% 
  arrange(AIC) %>% 
  knitr::kable(digits = 2)


tabela

#### adjR2 wy?szy lepszy, CV ni?sze lepsze, AIC mniejsze lepsze

model1 = log10(pm10) ~ air_temp + visibility + trend() + season()
model2 = log10(pm10) ~ air_temp + dew_point + trend() + season()
model3 = log10(pm10) ~ visibility + dew_point + trend() + season()
model4 = log10(pm10) ~ air_temp + dew_point
model5 = log10(pm10) ~ dew_point
model6 = log10(pm10) ~ air_temp + trend() + season()



wybrany1 <- model(treningowy, TSLM(model1))
wybrany2 <- model(treningowy, TSLM(model2))
wybrany3 <- model(treningowy, TSLM(model3))
wybrany4 <- model(treningowy, TSLM(model4))
wybrany5 <- model(treningowy, TSLM(model5))
wybrany6 <- model(treningowy, TSLM(model6))



###RESZTY MODELI I AUTOKORELACJA RESZT

shapiro.test(wybrany1 %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁAD

ggpubr::ggqqplot(wybrany1 %>% residuals() %>%  pull(.resid), main = "Wykres Kwantyl-Kwantyl")

hist(wybrany1 %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu pierwszego", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(wybrany1) -> res1
durbinWatsonTest(wybrany1 %>% residuals() %>% pull(.resid))



###2

shapiro.test(wybrany2 %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(wybrany2 %>% residuals() %>%  pull(.resid), main = "Wykres Kwantyl-Kwantyl")

hist(wybrany2 %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu drugiego", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(wybrany2)

durbinWatsonTest(wybrany2 %>% residuals() %>% pull(.resid))


###3

shapiro.test(wybrany3 %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(wybrany3 %>% residuals() %>%  pull(.resid), main = "Wykres Kwantyl-Kwantyl")

hist(wybrany3 %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu trzeciego", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(wybrany3)

durbinWatsonTest(wybrany3 %>% residuals() %>% pull(.resid))

###4

shapiro.test(wybrany4 %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(wybrany4 %>% residuals() %>%  pull(.resid), main = "Wykres Kwantyl-Kwantyl")

hist(wybrany4 %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu czwartego", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(wybrany4)

durbinWatsonTest(wybrany4 %>% residuals() %>% pull(.resid))

###5

shapiro.test(wybrany5 %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(wybrany5 %>% residuals() %>%  pull(.resid), main = "Wykres Kwantyl-Kwantyl")

hist(wybrany5 %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu[ piątego", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(wybrany5)

durbinWatsonTest(wybrany5 %>% residuals() %>% pull(.resid))

###6


shapiro.test(wybrany6 %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(wybrany6 %>% residuals() %>%  pull(.resid), main = "Wykres Kwantyl-Kwantyl")


hist(wybrany6 %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu szóstego", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(wybrany6)

durbinWatsonTest(wybrany6 %>% residuals() %>% pull(.resid))

###ETS

ets_model <- treningowy %>% model(ETS(pm10))

ets_model %>% report()

###KOMPONENTY ETS

dane %>% 
  model(ETS(pm10 ~ error("M") + trend("N") + season("M"))) %>% 
  report()

ets_model %>% 
  components() %>%
  autoplot() +
  ggtitle("ETS(M,N,M) components")

shapiro.test(ets_model %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(ets_model %>% residuals() %>%  pull(.resid))

hist(ets_model %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu ETS", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(ets_model)

durbinWatsonTest(ets_model %>% residuals() %>% pull(.resid))

###ARIMA


model_arima  <- treningowy %>%
  model(arima = ARIMA(pm10, stepwise = F, approximation = F))

model_arima %>% report()

model_arima %>% gg_tsresiduals()

shapiro.test(model_arima %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(model_arima %>% residuals() %>%  pull(.resid))

hist(model_arima %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu ARIMA", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(model_arima)

durbinWatsonTest(model_arima %>% residuals() %>% pull(.resid))
###REGRESJA DYNAMICZNA

fit_dynamiczna <- treningowy %>%
  model(arima = ARIMA (pm10 ~ air_temp + dew_point + trend() + season() ))

fit_dynamiczna %>% report()

fit_dynamiczna %>% gg_tsresiduals()

augment(fit_dynamiczna) %>%
  features(.resid, ljung_box, dof = 5, lag = 12)

shapiro.test(fit_dynamiczna %>% residuals() %>% pull(.resid))  ###NORMALNOŚĆ ROZKŁADU

ggpubr::ggqqplot(fit_dynamiczna %>% residuals() %>%  pull(.resid))

hist(fit_dynamiczna %>% residuals() %>% pull(.resid), main = "Histogram reszt modelu regresji dynamicznej", 
     xlab = "reszty", ylab = "częstotliwość występowania")

gg_tsresiduals(fit_dynamiczna)

durbinWatsonTest(fit_dynamiczna %>% residuals() %>% pull(.resid))    

###DYNAMICZNE
list(model1,
     model2,
     model3,
     model4,
     model5,
     model6) -> wybrane_modele

map(wybrane_modele, as.formula)  -> wybrane_modele

wybrane_modele_tab <- map(.x = wybrane_modele, 
                          .f = ~model(treningowy, TSLM(formula = .x)) %>% glance()) %>% 
  do.call(rbind, .) %>%  
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) 

wybrane_modele_tab

wynikowa <- wybrane_modele_tab %>% 
  mutate(.model = wybrane_modele) %>% 
  arrange(AIC) %>% 
  knitr::kable(digits = 2)

wynikowa

###PREDYKCJA DLA NAJLEPSZYCH MODELI + ARIMA + ETS + DYNAMICZNA ARIMA LAG = 6 lub 12
pred_ets <- ets_model %>%  forecast(h = "12 months")

pred_arima <- model_arima %>% forecast(h = "12 months")

###PLOT ARIMA
pred_arima %>%
  autoplot(dane, level = 95) + xlab("Rok") +
  ylab("Częstotliwość wystąpienia") +
  scale_color_brewer(type = "qual", palette = "Dark2") -> pf_a ; pf_a

###PLOT ETS
pred_ets %>%
  autoplot(dane, level = 95) + xlab("Rok") +
  ylab("Częstotliwość wystąpienia") +
  scale_color_brewer(type = "qual", palette = "Dark2") -> pf_e ; pf_e

###WYKRESY DO WYSWIETLENIA
grid.arrange(pf_a + ggtitle("ARIMA"),
             pf_e + ggtitle("ETS"))

###MODELE DYNAMICZNE

###1

fit_model1 <- treningowy %>%
  model(model = TSLM(log10(pm10) ~ air_temp + visibility + trend() + season()))

pd_model1 <- forecast(fit_model1, testowy)

treningowy %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_model1), size = 1) +
  autolayer(pd_model1, alpha = 0.5, level = 95) +
  guides(colour = guide_legend(title = "Model 1")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> w1

####2
fit_model2 <- treningowy %>%
  model(model = TSLM(log10(pm10) ~ air_temp + dew_point + trend() + season()))

pd_model2 <- forecast(fit_model2, testowy)

treningowy %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_model2), size = 1) +
  autolayer(pd_model2, alpha = 0.5, level = 95) +
  guides(colour = guide_legend(title = "Model 2")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> w2


###3
fit_model3 <- treningowy %>%
  model(model = TSLM(log10(pm10) ~ visibility + dew_point + trend() + season()))

pd_model3 <- forecast(fit_model3, testowy)

treningowy %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_model3), size = 1) +
  autolayer(pd_model3, alpha = 0.5, level = 95) +
  guides(colour = guide_legend(title = "Model 3")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> w3


###4
fit_model4 <- treningowy %>%
  model(model = TSLM(log10(pm10) ~ air_temp + dew_point))

pd_model4 <- forecast(fit_model4, testowy)

treningowy %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_model4), size = 1) +
  autolayer(pd_model4, alpha = 0.5, level = 95) +
  guides(colour = guide_legend(title = "Model 4")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> w4

###5
fit_model5 <- treningowy %>%
  model(model = TSLM(log10(pm10) ~ dew_point))

pd_model5 <- forecast(fit_model5, testowy)

treningowy %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_model5), size = 1) +
  autolayer(pd_model5, alpha = 0.5, level = 95) +
  guides(colour = guide_legend(title = "Model 5")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> w5


###6
fit_model6 <- treningowy %>%
  model(model = TSLM(log10(pm10) ~ air_temp + trend() + season()))

pd_model6 <- forecast(fit_model6, testowy)

treningowy %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_model6), size = 1) +
  autolayer(pd_model6, alpha = 0.5, level = 95) +
  guides(colour = guide_legend(title = "Model 6")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> w6

grid.arrange(w1 + ggtitle("Model 1"),
             w2 + ggtitle("Model 2"),
             w3 + ggtitle("Model 3"),
             w4 + ggtitle("Model 4"),
             w5 + ggtitle("Model 5"),
             w6 + ggtitle("Model 6"))


### NAJLEPSZY MODEL

dane_m %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = "Model"), data = fitted(fit_model1), size = 1) +
  autolayer(pd_model1, alpha = 0.7, level = 95) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> p1


dane_m %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = "Model"), data = fitted(fit_model2), size = 1) +
  autolayer(pd_model2, alpha = 0.7, level = 95) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> p2

dane_m %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = "model"), data = fitted(fit_model3), size = 1) +
  autolayer(pd_model3, alpha = 0.7, level = 95) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> p3

dane_m %>% 
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = "model"), data = fitted(fit_model4), size = 1) +
  autolayer(pd_model4, alpha = 0.7, level = 95) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> p4


dane_m %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = "model"), data = fitted(fit_model5), size = 1) +
  autolayer(pd_model5, alpha = 0.7, level = 95) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> p5

dane_m %>%
  autoplot(pm10) + xlab("Rok") + ylab("Stężenie PM10") +
  geom_line(aes(y = .fitted, colour = "model"), data = fitted(fit_model6), size = 1) +
  autolayer(pd_model6, alpha = 0.7, level = 95) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  theme_bw() -> p6
###DO WYSWIETLENIA

grid.arrange(p1 + ggtitle("Predykcja modelu 1"),
             p2 + ggtitle("Predykcja modelu 2"),
             p3 + ggtitle("Predykcja modelu 3"),
             p4 + ggtitle("Predykcja modelu 4"),
             p5 + ggtitle("Predykcja modelu 5"),
             p6 + ggtitle("Predykcja modelu 6"))



###WNIOSKI
