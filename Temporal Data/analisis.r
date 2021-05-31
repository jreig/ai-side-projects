# ==============================================================================
# Autor: Joan Reig Doménech
# Entorno: Windows 10 - RStudio V1.2.5033 - R v3.6.2
# 
# Descripción:
#   Analisis de las caracteristicas de la serie temporal
# ==============================================================================

library(anomalize)
library(dplyr)
library(forecast)
library(htmlwidgets)
library(timetk)
library(tseries)
library(tsoutliers)
library(TSstudio)
library(xts)

# ------------------------------------------------------------------------------
# DATOS
# ------------------------------------------------------------------------------
demandaRaw <- read.csv("Demanda_2015.txt", sep = "\t", header = F)
demandaRaw$index <- paste(demandaRaw$V1, demandaRaw$V2, sep='T')
demandaRaw$consumo <- demandaRaw$V3
demandaRaw$V1 <- NULL
demandaRaw$V2 <- NULL
demandaRaw$V3 <- NULL

# Convertir datos en una serie temporal
demandaRawM<-demandaRaw
demandaRawM$index<-as.POSIXct(demandaRawM$index, tz = "GMT", format="%d/%m/%yT%H:%M")
demandaXtsM<-as.xts(demandaRawM$consumo, order.by = demandaRawM$index)

# Convertir datos en una serie temporal horaria para poder mostrar mejores gráficas
# Para ello se hace la media de todos los valores tomados en una hora
demandaRawH<-demandaRaw
demandaRawH$index<-as.POSIXct(demandaRawH$index, tz = "GMT", format="%d/%m/%yT%H")
demandaRawH<-demandaRawH %>% group_by(index) %>% summarise(consumo=mean(consumo))
demandaXtsH<-as.xts(demandaRawH$consumo, order.by = demandaRawH$index)

# Convertir datos en una serie temporal diaria para poder mostrar mejores gráficas
# Para ello se hace la media de todos los valores tomados en un dia
demandaRawD<-demandaRaw
demandaRawD$index<-as.POSIXct(demandaRawD$index, tz = "GMT", format="%d/%m/%y")
demandaRawD<-demandaRawD %>% group_by(index) %>% summarise(consumo=mean(consumo))
demandaXtsD<-as.xts(demandaRawD$consumo, order.by = demandaRawD$index)

# ------------------------------------------------------------------------------
# ANÁLISIS Y CARACTERÍSTICAS
# ------------------------------------------------------------------------------
ts_plot(demandaXtsM, title = "Consumo")
saveWidget(ts_plot(demandaXtsM, title = "Consumo"), "consumo-datos.html")

ts_quantile(demandaXtsH, title = "Consumo horario")
saveWidget(ts_quantile(demandaXtsH, title = "Consumo horario"), "consumo-horario.html")

ts_quantile(demandaXtsD, title = "Consumo diario", period = "monthly")
saveWidget(ts_quantile(demandaXtsD, title = "Consumo diario", period = "monthly"), "consumo-diario.html")

ts_cor(ts(demandaXtsM), type = 'acf', seasonal = F, lag.max = 6*24*7)
saveWidget(ts_cor(ts(demandaXtsM), type = 'acf', seasonal = F, lag.max = 6*24*7), "correlacion-temporal.html")

# -----------------------------------------------------------------------------
# DETECCIÓN DE OUTLIERS (GESD)
# -----------------------------------------------------------------------------
# Descomposicion mediante TSL
demandaTk <- tk_tbl(demandaXtsM, rename_index = "indexD")
demandaTkDesc <- time_decompose(demandaTk, "value", frequency = "1 week")

# Estacionalidad
ts_plot(ts(demandaTkDesc$season[1:1008]))
saveWidget(ts_plot(ts(demandaTkDesc$season[1:1008])), "estacionalidad.html")

# Tendencia
ts_plot(ts(demandaTkDesc$trend))
saveWidget(ts_plot(ts(demandaTkDesc$trend)), "tendencia.html")

# Irregular
mybinsize<-IQR(demandaTkDesc$remainder, na.rm = TRUE)/4        
mysd<-sd(demandaTkDesc$remainder, na.rm = TRUE)
mymin<-min(demandaTkDesc$remainder, na.rm = TRUE) - mysd
mymax<-max(demandaTkDesc$remainder, na.rm = TRUE) + mysd
mybins <- seq(mymin, mymax, mybinsize)
hist<-hist(demandaTkDesc$remainder, breaks=mybins, na.rm = TRUE)

# Detectar outliers en componente irregular
outliers <- anomalize(demandaTkDesc, "remainder", method = "gesd", verbose = T)

plot(demandaTkDesc$remainder, type = "l")
lines(outliers$anomalized_tbl$remainder_l1)
lines(outliers$anomalized_tbl$remainder_l2)
points(x = outliers$anomaly_details$outlier_idx, y = outliers$anomaly_details$outlier_vals, col = "#ff0000")

plot_anomalies(outliers$anomalized_tbl)
