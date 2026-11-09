# ==============================================================================
# PROYECTO: Análisis de Incidencia Delictiva
# SCRIPT: comercio.R
# OBJETIVO: Analizar la serie de tiempo de comercio
# ==============================================================================
# Mejores modelos 
# SARIMA(1,1,2)(0,0,2) 
# Prophet cpr = 0.95, cps=0.3, sps=0.5
library(tidyverse)
library(plotly)
library(tseries)
library(forecast)
library(prophet)
library(readr)

# Funciones ----
graficar_serie <- function(serie, nombre){
  #Graficar serie de tiempo
  ggplot(serie, aes(x=FECHA, y=TOTAL))+
    geom_line(color = "black", linewidth = 1) +
    geom_point() +
    labs(title = paste0("Evolución Mensual (",nombre,")"),
         x="Fecha", y="Total de incidencias")
  
}
graficar_modelos <- function(dataset, nombre){
  #Graficar serie de tiempo vs ARIMA 1, ARIMA 2 Y ARIMA auto
  ggplot(dataset, aes(x = FECHA)) +
    geom_line(aes(y = TOTAL, color = "Datos Reales"), size = 1) +
    geom_line(aes(y = PREDICCIONES, color = "Modelo ARIMA"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PREDICCIONES_2, color = "Modelo ARIMA 2"), size = 1.5, linetype = "dashed") +
    geom_line(aes(y = PREDICCIONES_AUTO, color = "Modelo ARIMA AUTO"), size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Datos Reales" = "grey60", "Modelo ARIMA" = "steelblue", 
                                  "Modelo ARIMA 2" = "red", "Modelo ARIMA AUTO" = "green")) +
    labs(title = paste0("Comparación de Incidencia Delictiva: ", nombre),
         subtitle = "Valores observados vs. Ajustes de los modelo ARIMA",
         x = "Mes / Año",
         y = "Número total de delitos",
         color = "Referencia") +
    theme_minimal() +
    theme(legend.position = "bottom")
}
graficar_validacion <- function(train_data, test_data, predichos, pronostico, metricas, nombre){
  # Graficar serie, ajuste del modelo en train, predicción del test e intervalo de confianza
  plot_validación <- ggplot() +
    geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
    geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
    geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
              size = 1, linetype = "dashed") +
    geom_ribbon(aes(x = test_data$FECHA, 
                    ymin = as.numeric(pronostico$lower[,2]), 
                    ymax = as.numeric(pronostico$upper[,2])), 
                fill = "steelblue", alpha = 0.2) +
    labs(title = paste0("Validación del Modelo ARIMA: ", nombre),
         subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
         x = "Fecha", y = "Delitos", color = "Serie") +
    theme_minimal()
  
  print(plot_validación)
}

# Creación base inicial ----
# Leer archivo inicial
datos_narcotrafico <-readRDS(here('data', 'datos_narcotrafico.rds'))

#Creación datos de comercio
comercio <- datos_narcotrafico %>%
  filter(TIPO == "COMERCIO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

graficar_serie(na.omit(comercio), "COMERCIO")

metricas_comercio <- data.frame(Año = numeric(),
                                Modelo = character(),
                                RMSE   = numeric(),
                                MAE    = numeric(),
                                MAPE   = numeric(),
                                stringsAsFactors = FALSE)

# Grid ----
# Realizar Grid para los mejores parámetros de Prophet
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

# Dividir en Train/Test
n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

# Parámetros del GRID
range_grid <- c(0.8, 0.9, 0.95)
changepoints_grid <- c(0.3, 0.01, 0.05, 0.1, 0.5)
seasonality_grid  <- c(1.0, 5.0, 10.0, 20.0, 0.5)
#holidays_grid <- c(5,10,20)

grid <- expand.grid(
  changepoint.range = range_grid,
  changepoint.prior.scale = changepoints_grid,
  seasonality.prior.scale = seasonality_grid)#,
#  holidays.prior.scale = holidays_grid)
resultados_optimizacion <- data.frame()

# Ajustar el Grid
for(i in 1:nrow(grid)) {
  
  # Ajustar los parámetros a un modelo temporal
  m_temp <- prophet(
    train_data, 
    changepoint.range = grid$changepoint.range[i],
    changepoint.prior.scale = grid$changepoint.prior.scale[i],
    seasonality.prior.scale = grid$seasonality.prior.scale[i],
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )
  
  # Cross validation del modelo temporal
  df_cv <- cross_validation(
    m_temp, 
    initial = 365 * 8,  
    period = 365,       
    horizon = 365,      
    units = 'days'
  )
  
  # Guardar métricas del modelo temporal
  df_p <- performance_metrics(df_cv)
  
  # Guardar parámetros del modelo temporal
  resultado_temp <- data.frame(
    range = grid$changepoint.range[i],
    changepoint = grid$changepoint.prior.scale[i],
    seasonality = grid$seasonality.prior.scale[i],
    rmse_promedio = mean(df_p$rmse)
  )
  # Juntar métricas y parámetros
  resultados_optimizacion <- bind_rows(resultados_optimizacion, resultado_temp)
}

# Mostrar mejor resultado usando RMSE para ordenar
mejor_combinacion <- resultados_optimizacion %>% 
arrange(rmse_promedio) %>% 
head(1)

print("¡Los mejores hiperparámetros para Prophet son:")
print(mejor_combinacion)
# Expanding Window ----
# Aplicaremos un método de ventana deslizante para encontrar los mejores 
# parámetros para ARIMA y así pode comparar ARIMA con Prophet
## 2021 ----
### ARIMA ---- 
# Filtramos antes de 2021 y graficamos
comercio <- comercio %>%
  filter(year(FECHA)<=2021)

n_test <- 12

graficar_serie(na.omit(comercio), "COMERCIO")

# Descomponer serie en tendencia, estacionalidad y ruido
comercio_ts <- ts(na.omit(comercio$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(comercio_ts, type = "multiplicative")
plot(decomposed)

# Dividir serie en Train/Test
n_total <- nrow(comercio)
n_train <- n_total-n_test
train_data <- comercio[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- comercio[(n_train+1):n_total, ]

# Estabilizar la varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data #%>%
#  mutate(TOTAL=log(TOTAL+1))

# Estabilizar la media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver los outliers
graficar_serie(diff_arima, "comercio DIFF, LOG")
tsoutliers(train_ts)

# Crear variables dummy, para el tratamiento de los outliers
dummy_2017 <- rep(0, n_train)
dummy_2018 <- rep(0, n_train)
dummy_2019 <- rep(0, n_train)

dummy_2017[50] <- 1
dummy_2018[62] <- 1
dummy_2019[75:76] <- 1

xreg_matrix <- cbind(D2017 = dummy_2017, D2018 = dummy_2018, 
                     D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

# Ajustar modelos
modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 2), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 2), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- fitted(modelo)
train_data$PREDICCIONES_2 <- fitted(modelo_2)
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "comercio")

# Ver residuos de lo modelos
checkresiduals(modelo)
acf(residuals(modelo), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo), na.action = na.pass, lag.max = 60)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
acf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
acf(residuals(auto), na.action = na.pass, lag.max = 60)
pacf(residuals(auto), na.action = na.pass, lag.max = 60)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

# Comparamos el AIC
AIC(modelo, modelo_2, auto)

# El mejor modelo ARIMA es modelo_2

### Test ----
# Variables dummy para el test
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2017 = dummy_test_2, D2018 = dummy_test_2, D2019 = dummy_test_2)

# Pronóstico de la serie con el modelo
pronostico <- forecast(modelo_2, h = n_test, xreg = xreg_matrix)

# Obtener las métricas
reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2021, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar la predicción
graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "comercio")

## Prophet ----
# Crear df para el modelo prophet
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

# Dividir en Train/Test
n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

# Ajustar modelo
modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, 
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.3,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 0.5,
  holidays.prior.scale = 10,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- fit.prophet(modelo, train_data)

# Hacer predicción
future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

# Obtener métricas del modelo
metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

# Graficar puntos de cambios
plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

# Guardar las métricas
metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2021, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicción
ggplot() +
  geom_line(data = train_data,
            aes(x = ds, y = y, color = "Train"),
            size = 1) +
  geom_line(data = test_data,
            aes(x = ds, y = y, color = "Real"),
            size = 1) +
  geom_line(data = forecast_test,
            aes(x = ds, y = yhat, color = "Predicción"),
            linetype = "dashed",
            size = 1) +
  geom_ribbon(data = forecast_test,
              aes(x = ds,
                  ymin = yhat_lower,
                  ymax = yhat_upper),
              alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## 2022 ----
# Recuperar los años filtrados
comercio <- datos_narcotrafico %>%
  filter(TIPO == "COMERCIO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
### ARIMA ----
# Filtramos antes de 2022 y graficamos
comercio <- comercio %>%
  filter(year(FECHA)<=2022)

n_test <- 12

graficar_serie(na.omit(comercio), "COMERCIO")

# Descomponer serie en tendencia, estacionalidad y ruido
comercio_ts <- ts(na.omit(comercio$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(comercio_ts, type = "multiplicative")
plot(decomposed)

# Dividir en Train/Test
n_total <- nrow(comercio)
n_train <- n_total-n_test
train_data <- comercio[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- comercio[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data #%>%
#  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "comercio DIFF, LOG")
tsoutliers(train_ts)

# Ajustar variables dummy para tratar outliers
dummy_2017 <- rep(0, n_train)
dummy_2018 <- rep(0, n_train)
dummy_2019 <- rep(0, n_train)

dummy_2017[50] <- 1
dummy_2018[62] <- 1
dummy_2019[75:76] <- 1

xreg_matrix <- cbind(D2017 = dummy_2017, D2018 = dummy_2018, 
                     D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

# Ajustar modelos
modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 2), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 2), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- fitted(modelo)
train_data$PREDICCIONES_2 <- fitted(modelo_2)
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "comercio")

# Ver residuos de los modelos
checkresiduals(modelo)
acf(residuals(modelo), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo), na.action = na.pass, lag.max = 60)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
acf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
acf(residuals(auto), na.action = na.pass, lag.max = 60)
pacf(residuals(auto), na.action = na.pass, lag.max = 60)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

# Comparar AIC de los modelos
AIC(modelo, modelo_2, auto)

# El mejor modelo ARIMA es modelo_2

### Test ----
# Variables dummy para el test
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2017 = dummy_test_2, D2018 = dummy_test_2, D2019 = dummy_test_2)

# Ajustar modelo
pronostico <- forecast(modelo_2, h = n_test, xreg = xreg_matrix)

# Obtener métricas
reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2022, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar prediccion
graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "comercio")

### Prophet ----
# Crear df para prophet
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

# Dividir en Train/Test
n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

# Ajustar modelo
modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, 
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.3,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 0.5,
  holidays.prior.scale = 10,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- fit.prophet(modelo, train_data)

# Hacer la predicción
future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

# Obtener las métricas
metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2022, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar la predicción
ggplot() +
  geom_line(data = train_data,
            aes(x = ds, y = y, color = "Train"),
            size = 1) +
  geom_line(data = test_data,
            aes(x = ds, y = y, color = "Real"),
            size = 1) +
  geom_line(data = forecast_test,
            aes(x = ds, y = yhat, color = "Predicción"),
            linetype = "dashed",
            size = 1) +
  geom_ribbon(data = forecast_test,
              aes(x = ds,
                  ymin = yhat_lower,
                  ymax = yhat_upper),
              alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## 2023 ----
# Recuperar los años filtrados
comercio <- datos_narcotrafico %>%
  filter(TIPO == "COMERCIO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
### ARIMA ----
# Filtramos antes del 2023 y graficamos
comercio <- comercio %>%
  filter(year(FECHA)<=2023)

n_test <- 12

graficar_serie(na.omit(comercio), "COMERCIO")

# Descomponer serie
comercio_ts <- ts(na.omit(comercio$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(comercio_ts, type = "multiplicative")
plot(decomposed)

# Dividir datos
n_total <- nrow(comercio)
n_train <- n_total-n_test
train_data <- comercio[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- comercio[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data #%>%
#  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "comercio DIFF, LOG")
tsoutliers(train_ts)

# Crear dummys
dummy_2017 <- rep(0, n_train)
dummy_2018 <- rep(0, n_train)
dummy_2019 <- rep(0, n_train)

dummy_2017[50] <- 1
dummy_2018[62] <- 1
dummy_2019[75:76] <- 1

xreg_matrix <- cbind(D2017 = dummy_2017, D2018 = dummy_2018, 
                     D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

# AJustar modelos
modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 2), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 2), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)

summary(modelo)
summary(modelo_2)
summary(auto)

# Graficar modelos
train_data$PREDICCIONES <- fitted(modelo)
train_data$PREDICCIONES_2 <- fitted(modelo_2)
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "comercio")

# Checar residuos de los modelos
checkresiduals(modelo)
acf(residuals(modelo), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo), na.action = na.pass, lag.max = 60)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
acf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
acf(residuals(auto), na.action = na.pass, lag.max = 60)
pacf(residuals(auto), na.action = na.pass, lag.max = 60)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

# Comparar modelos
AIC(modelo, modelo_2, auto)

# El mejor modelo ARIMA es modelo_2

### Test ----
# Variables dummy para test
dummy_test <- rep(0, n_test)
dummy_test[2] <- 1
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2017 = dummy_test_2, D2018 = dummy_test_2, D2019 = dummy_test)

# Hacer pronóstico
pronostico <- forecast(modelo_2, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

# Obtener métricas 
metricas <- accuracy(predichos, reales)
print(metricas)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2023, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicciones
graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "comercio")

### Prophet ----
# Hacer df para prophet
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

# Dividir datos
n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

# Ajustar modelo
modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, 
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.3,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 0.5,
  holidays.prior.scale = 10,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- fit.prophet(modelo, train_data)

# Hacer predicciones
future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

# Obtener métricas
metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2023, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicciones
ggplot() +
  geom_line(data = train_data,
            aes(x = ds, y = y, color = "Train"),
            size = 1) +
  geom_line(data = test_data,
            aes(x = ds, y = y, color = "Real"),
            size = 1) +
  geom_line(data = forecast_test,
            aes(x = ds, y = yhat, color = "Predicción"),
            linetype = "dashed",
            size = 1) +
  geom_ribbon(data = forecast_test,
              aes(x = ds,
                  ymin = yhat_lower,
                  ymax = yhat_upper),
              alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## 2024 ----
# Recuperar los años filtrados
comercio <- datos_narcotrafico %>%
  filter(TIPO == "COMERCIO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
### ARIMA ----
# Quitar los años después de 2024 y graficar
comercio <- comercio %>%
  filter(year(FECHA)<=2024)

n_test <- 12

graficar_serie(na.omit(comercio), "COMERCIO")

# Descomponer serie
comercio_ts <- ts(na.omit(comercio$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(comercio_ts, type = "multiplicative")
plot(decomposed)

# Train/Test
n_total <- nrow(comercio)
n_train <- n_total-n_test
train_data <- comercio[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- comercio[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data #%>%
#  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "comercio DIFF, LOG")
tsoutliers(train_ts)

# Crear variables dummy
dummy_2017 <- rep(0, n_train)
dummy_2018 <- rep(0, n_train)
dummy_2019 <- rep(0, n_train)
dummy_2023 <- rep(0, n_train)

dummy_2017[50] <- 1
dummy_2018[62] <- 1
dummy_2019[75:76] <- 1
dummy_2023[122] <- 1

xreg_matrix <- cbind(D2017 = dummy_2017, D2018 = dummy_2018, 
                     D2019 = dummy_2019, D2023 = dummy_2023)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

# Ajustar modelos
modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 2), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 2), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)

summary(modelo)
summary(modelo_2)
summary(auto)

# Graficar modelos
train_data$PREDICCIONES <- fitted(modelo)
train_data$PREDICCIONES_2 <- fitted(modelo_2)
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "comercio")

# Residuos de los modelos
checkresiduals(modelo)
acf(residuals(modelo), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo), na.action = na.pass, lag.max = 60)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
acf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
acf(residuals(auto), na.action = na.pass, lag.max = 60)
pacf(residuals(auto), na.action = na.pass, lag.max = 60)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

# Comparar modelos
AIC(modelo, modelo_2, auto)

# El mejor modelo ARIMA es modelo_2

### Test ----
# Variables dummy test
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2017 = dummy_test_2, D2018 = dummy_test_2, 
                     D2019 = dummy_test_2, D2023 = dummy_test_2)

# Hacer pronóstico
pronostico <- forecast(modelo_2, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

# Obtener métricas
metricas <- accuracy(predichos, reales)
print(metricas)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2024, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicción
graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "comercio")

### Prophet ----
# Crear df para prophet
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

# Dividir datos
n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

# Ajustar modelo
modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, 
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.3,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 0.5,
  holidays.prior.scale = 10,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- fit.prophet(modelo, train_data)

# Hacer predicciones
future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

# Obtener métricas
metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

# GRaficar puntos de cambio del modelo
plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2024, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicciones
ggplot() +
  geom_line(data = train_data,
            aes(x = ds, y = y, color = "Train"),
            size = 1) +
  geom_line(data = test_data,
            aes(x = ds, y = y, color = "Real"),
            size = 1) +
  geom_line(data = forecast_test,
            aes(x = ds, y = yhat, color = "Predicción"),
            linetype = "dashed",
            size = 1) +
  geom_ribbon(data = forecast_test,
              aes(x = ds,
                  ymin = yhat_lower,
                  ymax = yhat_upper),
              alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

## 2025 ----
# Recuperar los años filtrados
comercio <- datos_narcotrafico %>%
  filter(TIPO == "COMERCIO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
### ARIMA ----
# Graficar la serie
n_test <- 12

graficar_serie(na.omit(comercio), "COMERCIO")

# Descomponer serie
comercio_ts <- ts(na.omit(comercio$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(comercio_ts, type = "multiplicative")
plot(decomposed)

# Train/test
n_total <- nrow(comercio)
n_train <- n_total-n_test
train_data <- comercio[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- comercio[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data #%>%
#  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "comercio DIFF")
tsoutliers(train_ts)

# Crear dummies
dummy_2017 <- rep(0, n_train)
dummy_2018 <- rep(0, n_train)
dummy_2019 <- rep(0, n_train)
dummy_2023 <- rep(0, n_train)

dummy_2017[50] <- 1
dummy_2018[62] <- 1
dummy_2019[75:76] <- 1
dummy_2023[122] <- 1

xreg_matrix <- cbind(D2017 = dummy_2017, D2018 = dummy_2018, 
                     D2019 = dummy_2019, D2023 = dummy_2023)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

# Crear modelos
modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 2), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 2), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(1,1,2)(0,0,2)

# Graficar modelos
summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- fitted(modelo)
train_data$PREDICCIONES_2 <- fitted(modelo_2)
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "comercio")

# Comparar modelos
checkresiduals(modelo)
acf(residuals(modelo), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo), na.action = na.pass, lag.max = 60)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
acf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
pacf(residuals(modelo_2), na.action = na.pass, lag.max = 60)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
acf(residuals(auto), na.action = na.pass, lag.max = 60)
pacf(residuals(auto), na.action = na.pass, lag.max = 60)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# El mejor modelo ARIMA es modelo_2

### Test ----
# Variables dummy para el tes
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2017 = dummy_test_2, D2018 = dummy_test_2, 
                     D2019 = dummy_test_2, D2023 = dummy_test_2)

# Hacer pronóstico
pronostico <- forecast(modelo_2, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- as.numeric(pronostico$mean)

# Obtener métricas
metricas <- accuracy(predichos, reales)
print(metricas)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2025, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicción
graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "comercio")

### Prophet ----
# Df para prophet
datos_prophet <- comercio %>%
  select(ds = FECHA, y = TOTAL)

# Train/test
n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

# Ajustar modelo
modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, 
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.3,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 0.5,
  holidays.prior.scale = 10,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- fit.prophet(modelo, train_data)

# Hacer predicciones
future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

# Obtener metricas
metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

# Graficar puntos de cambio
plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_comercio <- rbind(
  metricas_comercio, 
  data.frame(Año = 2025, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

# Graficar predicciones
ggplot() +
  geom_line(data = train_data,
            aes(x = ds, y = y, color = "Train"),
            size = 1) +
  geom_line(data = test_data,
            aes(x = ds, y = y, color = "Real"),
            size = 1) +
  geom_line(data = forecast_test,
            aes(x = ds, y = yhat, color = "Predicción"),
            linetype = "dashed",
            size = 1) +
  geom_ribbon(data = forecast_test,
              aes(x = ds,
                  ymin = yhat_lower,
                  ymax = yhat_upper),
              alpha = 0.2) +
  labs(title = "Validación del Modelo Prophet: Comercio",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()