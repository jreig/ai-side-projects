# ==============================================================================
# Autor: Joan Reig Doménech
# Entorno: Windows 10 - RStudio V1.2.5033 - R v3.6.2
# 
# Descripción:
#   Predicción de valores mediante Prophet y visualización de resultados
# ==============================================================================

library(dygraphs)
library(forecast)
library(htmlwidgets)
library(Metrics)
library(MLmetrics)
library(prophet)
library(xts)

# -----------------------------------------------------------------------------
# FUNCIONES AUXILIARES
# -----------------------------------------------------------------------------
is_weekend <- function(ds) {
  date <- as.Date(ds)
  # La funcion devuelve el dia localizado (español en mi caso)
  day <- format(date, '%A')
  return(day == 'sábado' | day == 'domingo')
}

is_winter <- function(ds) {
  date <- as.Date(ds)
  month <- as.numeric(format(date, "%m"))
  return (month >= 12 | month <= 2)
}

is_spring <- function(ds) {
  date <- as.Date(ds)
  month <- as.numeric(format(date, "%m"))
  return (month >= 3 & month <= 5)
}

is_summer <- function(ds) {
  date <- as.Date(ds)
  month <- as.numeric(format(date, "%m"))
  return (month >= 6 & month <= 8)
}

is_autum <- function(ds) {
  date <- as.Date(ds)
  month <- as.numeric(format(date, "%m"))
  return (month >= 9 & month <= 11)
}

plot_results <- function (test, forecast, title, file) {
  cols = as.vector(c("yhat_lower", "yhat", "yhat_upper"))
  r <- as.xts(test$y, order.by = test$ds)
  p <- as.xts(forecast[, cols], order.by = forecast$ds)
  dyp <- dygraph(cbind(r, p), main = title, ylab = "Demanda") %>%
    dySeries(name = "r", label = "Real") %>%
    dySeries(name = c("yhat_lower", "yhat", "yhat_upper"), label = "Predicción") %>%
    dyOptions(labelsUTC = T, colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
    dyLegend(labelsSeparateLines = T)
  saveWidget(dyp, file)
  dyp
}

# -----------------------------------------------------------------------------
# DATOS
# -----------------------------------------------------------------------------
# Prophet requiere los datos en columnas nombradas 'ds' e 'y'
demanda <- read.csv("Demanda_2015.txt", sep = "\t", header = F)
demanda$ds <- paste(demanda$V1, demanda$V2, sep='T')

demanda$ds <- as.POSIXct(demanda$ds, tz = "GMT", format="%d/%m/%yT%H:%M")
demanda$y <- demanda$V3
demanda$V1 <- NULL
demanda$V2 <- NULL
demanda$V3 <- NULL

# Añadir los filtros de estacionalidad para utilizarlos posteriormente
demanda$weekend <- is_weekend(demanda$ds)
demanda$weekday <- !is_weekend(demanda$ds)
demanda$winter <- is_winter(demanda$ds)
demanda$spring <- is_spring(demanda$ds)
demanda$summer <- is_summer(demanda$ds)
demanda$autum <- is_autum(demanda$ds)

# El objetivo del trabajo es predecir la demanda entre el 8 y el 14 de Junio,
# por lo que los datos para entrenar el modelos seran los disponibles hasta
# el dia 8 y los datos de test la semana a predecir
train <- demanda[1:22752, ]
test <- demanda[22753:23760, ]

# -----------------------------------------------------------------------------
# MODELO 0: tbats
# -----------------------------------------------------------------------------
# Generar modelo y entrenar
ts0 <- ts(as.xts(train$y, order.by = train$ds), frequency = 6*24)
m0 <- tbats(ts0)

# Predicción
forecast0 <- forecast(m0, h = 1008)
acc0 <- forecast::accuracy(forecast0, test$y)
mase0 <- mase(test$y, forecast0$mean, step_size = 6*24)

# Mostrar resultados
f0 <- test
f0$yhat_lower <- as.numeric(forecast0$lower[1])
f0$yhat <- as.numeric(forecast0$mean)
f0$yhat_upper <- as.numeric(forecast0$upper[1])
plot_results(test, f0, "Modelo 0 (tbats)", "modelo0.prediccion.html")

# -----------------------------------------------------------------------------
# MODELO 1: Parametros por defecto
# -----------------------------------------------------------------------------
# Generar modelo y entrenar
m1 <- prophet()
m1 <- fit.prophet(m1, train)

# Predicción
f1 <- predict(m1, test)
acc1 <- forecast::accuracy(f1$yhat, test$y)
mase1 <- mase(test$y, f1$yhat, step_size = 6*24)

# Mostrar resultado
plot_results(test, f1, "Modelo 1 (default)", "modelo1-prediccion.html")
prophet_plot_components(m1,f1)

# -----------------------------------------------------------------------------
# MODELO 2: Incluir dias festivos
# -----------------------------------------------------------------------------
# Generar modelo y entrenar
m2 <- prophet()
m2 <- add_country_holidays(m2, country_name = 'ES')
m2 <- fit.prophet(m2, train)

# Bucle de predicción
f2 <- predict(m2, test)
acc2 <- forecast::accuracy(f2$yhat, test$y)
mase2 <- mase(test$y, f2$yhat, step_size = 6*24)

# Mostrar resultado de mejor predicción
plot_results(test, f2, "Modelo 2 (holidays)", "modelo2-prediccion.html")
prophet_plot_components(m2,f2)

# -----------------------------------------------------------------------------
# MODELO 3: Incluir dias festivos y estacionalidad de fin de semana
# -----------------------------------------------------------------------------
# Generar modelo y entrenar
m3 <- prophet(daily.seasonality = F)
m3 <- add_seasonality(m3, name='daily_weekend', period=1, fourier.order=5, condition.name='weekend')
m3 <- add_seasonality(m3, name='daily_weekday', period=1, fourier.order=5, condition.name='weekday')
m3 <- add_country_holidays(m3, country_name = 'ES')
m3 <- fit.prophet(m3, train)

# Bucle de predicción
f3 <- predict(m3, test)
acc3 <- forecast::accuracy(f3$yhat, test$y)
mase3 <- mase(test$y, f3$yhat, step_size = 6*24)

# Mostrar resultado de mejor predicción
plot_results(test, f3, "Modelo 3 (holidays + daily)", "modelo3-prediccion.html")
prophet_plot_components(m3,f3)

# -----------------------------------------------------------------------------
# MODELO 4: Incluir dias festivos y estacionalidad diaria temporada
# -----------------------------------------------------------------------------
# Generar modelo y entrenar
m4 <- prophet(daily.seasonality = F)
m4 <- add_seasonality(m4, name='daily_weekend', period=1, fourier.order=5, condition.name='weekend')
m4 <- add_seasonality(m4, name='daily_weekday', period=1, fourier.order=5, condition.name='weekday')
m4 <- add_seasonality(m4, name='daily_winter', period=1, fourier.order=5, condition.name='winter')
m4 <- add_seasonality(m4, name='daily_spring', period=1, fourier.order=5, condition.name='spring')
m4 <- add_seasonality(m4, name='daily_summer', period=1, fourier.order=5, condition.name='summer')
m4 <- add_seasonality(m4, name='daily_autum', period=1, fourier.order=5, condition.name='autum')
m4 <- add_country_holidays(m4, country_name = 'ES')
m4 <- fit.prophet(m4, train)

# Bucle de predicción
f4 <- predict(m4, test)
acc4 <- forecast::accuracy(f4$yhat, test$y)
mase4 <- mase(test$y, f4$yhat, step_size = 6*24)

# Mostrar resultado de mejor predicción
plot_results(test, f4, "Modelo 4 (holidays + daily and seasons)", "modelo4-prediccion.html")
prophet_plot_components(m4,f4)

# -----------------------------------------------------------------------------
# MODELO 5: Incluir dias festivos y estacionalidad diaria estacional y semanal
# -----------------------------------------------------------------------------
# Generar modelo y entrenar
m5 <- prophet(daily.seasonality = F, weekly.seasonality = F)
m5 <- add_seasonality(m5, name='daily_weekend', period=1, fourier.order=5, condition.name='weekend')
m5 <- add_seasonality(m5, name='daily_weekday', period=1, fourier.order=5, condition.name='weekday')
m5 <- add_seasonality(m5, name='daily_winter', period=1, fourier.order=5, condition.name='winter')
m5 <- add_seasonality(m5, name='daily_spring', period=1, fourier.order=5, condition.name='spring')
m5 <- add_seasonality(m5, name='daily_summer', period=1, fourier.order=5, condition.name='summer')
m5 <- add_seasonality(m5, name='daily_autum', period=1, fourier.order=5, condition.name='autum')
m5 <- add_seasonality(m5, name='weekly_winter', period=7, fourier.order=5, condition.name='winter')
m5 <- add_seasonality(m5, name='weekly_spring', period=7, fourier.order=5, condition.name='spring')
m5 <- add_seasonality(m5, name='weekly_summer', period=7, fourier.order=5, condition.name='summer')
m5 <- add_seasonality(m5, name='weekly_autum', period=7, fourier.order=5, condition.name='autum')
m5 <- add_country_holidays(m5, country_name = 'ES')
m5 <- fit.prophet(m5, train)

# Bucle de predicción
f5 <- predict(m5, test)
acc5 <- forecast::accuracy(f5$yhat, test$y)
mase5 <- mase(test$y, f5$yhat, step_size = 6*24)

# Mostrar resultado de mejor predicción
plot_results(test, f5, "Modelo 5 (holidays + daily and seasons + weekly)", "modelo5-prediccion.html")
prophet_plot_components(m5,f5)

# -----------------------------------------------------------------------------
# RESULTADOS
# -----------------------------------------------------------------------------
acc0
acc1
acc2
acc3
acc4
acc5

mase0
mase1
mase2
mase3
mase4
mase5

# Exportar predicción del modelo 4 en xlsx
install.packages("writexl")
library(writexl)

results <- data.frame(strftime(f4$ds, tz = "GMT"), f4$yhat)
names(results) <- c('fecha', 'prediccion')
write_xlsx(x = results, path = "resultados.xlsx", col_names = TRUE)

