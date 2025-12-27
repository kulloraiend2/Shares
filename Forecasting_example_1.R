# 
# Time Series and Forecasting Using R
# https://www.geeksforgeeks.org/r-language/time-series-and-forecasting-using-r/
# 
# Kullo 04.09.2025
# 
# Veendu, et pakett on olemas
if (!require("forecast")) install.packages("forecast")

# Lae pakett 
library(forecast)
library(tidyverse)

# # load the AirPassengers dataset
# dataset <- AirPassengers
# 
# print(head(dataset))
# print(class(dataset))
# str(dataset)
# attributes(dataset)
# 
# # plot the AirPassengers data to observe trends and patterns over time
# plot(AirPassengers)
# 
# # decompose the data using the "multiplicative" model, as the seasonal variation seems to change with the trend.
# data <- ts(AirPassengers, frequency=12)
# d <- decompose(data, "multiplicative")
# plot(d)
# str(d)
# 
# # use the ARIMA model to forecast passenger data for the next 10 years (120 months)
# model<-auto.arima(AirPassengers)
# summary(model)
# 
# f <- forecast(model, level=c(95), h=10*12)
# plot(f)
# str(f)
# 
# #  forecast passenger counts for the next 10 years (120 months) using the ARIMA model.
# forecast_result <- forecast(model, level = c(95), h = 10 * 12)
# plot(forecast_result)
# str(forecast_result)

# Prognoosi viimase kvartali keskmist palka

# Tartu maakonna keskmine palk alates 2021 I kv 
# c <- c(1394, 1498, 1441, 1561, 1516, 1680, 1614, 1750, 1730, 1889, 1790, 1941, 1888, 2039, 1962, 2092, 2023, 2175) 
# Aegrida viimase kvartali esialgu avaldatud vigase tulemusega
# c[length(c)] <- 2614

# Kokku keskmine palk alates 2021 I kv
c <- c(1406, 1476, 1463, 1548, 1536, 1666, 1641, 1735, 1741, 1872, 1812, 1904, 1894, 2007, 1959, 2062, 2011, 2126, 2075)  
# Aegrida viimase kvartali esialgu avaldatud vigase tulemusega
# c[length(c)] <- 2284

# RAA0012; SKP aheldatud väärtus alates 2021 I kv 
# c <- c(
#       7415.4,
#       7574.3,
#       7646.2,
#       7582.1,
#       7476.4,
#       7446.5,
#       7309.3,
#       7254.4,
#       7245.5,
#       7225.1,
#       7249.2,
#       7257.4,
#       7243.6,
#       7227.4,
#       7219.4,
#       7234.1,
#       7280.0
# )

# Aegrida, kvartalid alates 2021 I kv
# Jäta viimane kvartal teadaolevast aegreast välja. Viimast kvartalit hindame.
data <- ts(head(c, n = length(c) - 1), frequency = 4, start = c(2021, 1))

# VKK10 Kaupade ekspordi väärtus kuus
# c <- c(1034614420,
#        1117507310,
#        1143196056,
#        1194411651,
#        1237890914,
#        1269216151,
#        1192553400,
#        1183556150,
#        1230790240,
#        1360676115,
#        1387835316,
#        1070122028,
#        1158313697,
#        1162821038,
#        1238777800,
#        1240460687,
#        1334521028,
#        1174628284,
#        1173658990,
#        1147752858,
#        1184164178,
#        1273833012,
#        1215359329,
#        1077755475,
#        1145966997,
#        1088109983,
#        1233430696,
#        1014701155,
#        998516685,
#        1167462056,
#        1161641492,
#        1084500289,
#        1308611051,
#        1403809103,
#        1327361526,
#        1339675830,
#        1186890950,
#        1254166142,
#        1464065482,
#        1538210023,
#        1472380051,
#        1465049992,
#        1503154170,
#        1576100870,
#        1710277293,
#        1662071763,
#        1743805004,
#        1676793943,
#        1702113677,
#        1533224816,
#        1985054694, # 51
#        1624847855, # 52
#        1943280698,
#        1886987006,
#        1745718673,
#        1945492767,
#        1989496253,
#        1880686451,
#        1794128323,
#        1703267854,
#        1555057233,
#        1584753395,
#        1740430956,
#        1489258937,
#        1649628655,
#        1600669472,
#        1265639805,
#        1486407997,
#        1485965924,
#        1474911949,
#        1521220896,
#        1307448096,
#        1337526553,
#        1321897751,
#        1455628223,
#        1523086461,
#        1544587716,
#        1362507808,
#        1315484573,
#        1457542346,
#        1519871889,
#        1653367749,
#        1571371259,
#        1337144529,
#        1500761572,
#        1472558968,
#        1712482292,
#        1584070616,
#        1584074933,
#        1442922376,
#        1427583722
#        
# )
# c <- head(c, 51)

# Turism TU121 Majutatud 
# c <- c(
#   205540,
#   209742,
#   239423,
#   250576,
#   303911,
#   372687,
#   502931,
#   448125,
#   276301,
#   273780,
#   234604,
#   273875,
#   208405,
#   218936,
#   233384,
#   262149,
#   322059,
#   407092,
#   523645,
#   487883,
#   295464,
#   288742,
#   261857,
#   280339,
#   223216,
#   232356,
#   89719,
#   12884,
#   37372,
#   163167,
#   346692,
#   330587,
#   153641,
#   158009,
#   118120,
#   106368,
#   96229,
#   99757,
#   51185,
#   58928,
#   90007,
#   216226,
#   380716,
#   341551,
#   214455,
#   227439,
#   167264,
#   194589,
#   157829,
#   169756,
#   192443,
#   236007,
#   248592,
#   336963,
#   479044,
#   419520,
#   259745,
#   269837,
#   225707,
#   257455,
#   196442,
#   221666,
#   226277,
#   250191,
#   264763,
#   337119,
#   481498,
#   428925,
#   273789,
#   251345,
#   222494,
#   271221,
#   193953,
#   224948,
#   237380,
#   233843,
#   293434,
#   384269,
#   507590,
#   455498,
#   277577,
#   275028,
#   246548,
#   280322,
#   204971,
#   230410,
#   217018,
#   251330,
#   304185,
#   390642,
#   501898
#   
# )
# c <- head(c, 31)

# Aegrida, kuud alates 2018M01
# Jäta viimane kuu teadaolevast aegreast välja. Viimast kuud hindame.
# data <- ts(head(c, n = length(c) - 1), frequency = 12, start = c(2018, 1))

# Aegrida on määratud
str(data)
plot(data)

# decompose the data using the "multiplicative" model
d <- decompose(data, "multiplicative")
plot(d)
str(d)

# use the ARIMA model to forecast salary data for the next 1 quarter
model <- auto.arima(data)
str(model)
summary(model)

forecast_result <- forecast(model, level = c(95), h = 1)
plot(forecast_result)
str(forecast_result)
summary(forecast_result)

# tõenäosusega 95% ennustatud keskmise palga vahemik täisarvuna
bounds <- round(c(as.double(forecast_result[["lower"]]), as.double(forecast_result[["upper"]])), digits = 0)
bounds

# Aegrea viimane, kontrollitav väärtus
c_last <- c[length(c)]
c_last

# Kas aegrea viimane väärtus on ennustatud vahemikus 
between(c_last, bounds[1], bounds[2])

# Aegrea eelviimane, kontrollitavale eelnev väärtus
c_prev <- c[length(c) - 1]

# Keskmise palga viimase kvartali suhteline muutus eelmise kvartali suhtes protsentides
round(((c_last - c_prev) / c_prev) * 100, digits = 1)

# Keskmise palga viimase kvartali ennustatav keskmine palk
c_last_forecast_mean <- round(c(as.double(forecast_result[["mean"]])), digits = 0)
c_last_forecast_mean
  
# Keskmise palga viimase kvartali ennustatava keskmise palga suhteline muutus eelmise kvartali suhtes protsentides
round(((c_last_forecast_mean - c_prev) / c_prev) * 100, digits = 1)

# Keskmise palga ja hinnangu keskväärtuse erinevus
round(c_last - c_last_forecast_mean, digits = 0)

# Keskmise palga ja hinnangu keskväärtuse erinevus hinnangu keskväärtuse suhtes protsentides
round(((c_last - c_last_forecast_mean) / c_last_forecast_mean) * 100, digits = 1)

# Usalduspiiri absoluutväärtus
confidence_limit <- abs(c_last_forecast_mean - bounds[1])
confidence_limit

# usalduspiiri suhe ennustatavasse keskväärtusesse protsentides
round((confidence_limit / c_last_forecast_mean) * 100, digits = 1)

