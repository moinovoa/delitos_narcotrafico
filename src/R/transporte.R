# ==============================================================================
# PROYECTO: Análisis de Incidencia Delictiva
# SCRIPT: transporte.R
# OBJETIVO: Analizar la serie de tiempo de transporte
# ==============================================================================
# Mejores modelos 
# SARIMA(1,1,0)(2,0,0)
# Prophet cpr = 0.95, cps=0.01, sps=20, hps=40
library(tidyverse)
library(plotly)
library(tseries)
library(forecast)
library(prophet)
library(here)

# Funciones ----
graficar_serie <- function(serie, nombre){
  # Graficar serie de tiempo
  ggplot(serie, aes(x=FECHA, y=TOTAL))+
    geom_line(color = "black", linewidth = 1) +
    geom_point() +
    labs(title = paste0("Evolución Mensual (",nombre,")"),
         x="Fecha", y="Total de incidencias")
  
}
graficar_modelos <- function(dataset, nombre){
  # Graficar serie de tiempo vs ARIMA 1, ARIMA 2, auto ARIMA
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
                    ymin = exp(as.numeric(pronostico$lower[,2]))-1, 
                    ymax = exp(as.numeric(pronostico$upper[,2]))-1), 
                fill = "steelblue", alpha = 0.2) +
    labs(title = paste0("Validación del Modelo ARIMA: ", nombre),
         subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
         x = "Fecha", y = "Delitos", color = "Serie") +
    theme_minimal()
  
  print(plot_validación)
}

# Creación base inicial ----
# Leer archivo inicial
datos_narcotrafico <- readRDS(here('data', 'datos_narcotrafico.rds'))

# Creación base transporte
transporte <- datos_narcotrafico %>%
  filter(TIPO == "TRANSPORTE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))

graficar_serie(transporte, "TRANSPORTE")

metricas_transporte <- data.frame(Año = numeric(),
                             Modelo = character(),
                             RMSE   = numeric(),
                             MAE    = numeric(),
                             MAPE   = numeric(),
                             stringsAsFactors = FALSE)

# Grid ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

range_grid <- c(0.8, 0.9, 0.95)
changepoints_grid <- c(0.3, 0.01, 0.05, 0.1, 0.5)
seasonality_grid  <- c(1.0, 5.0, 10.0, 20.0, 0.5)
#holidays_grid <- c(5,10,20)

grid <- expand.grid(
  changepoint.range = range_grid,
  changepoint.prior.scale = changepoints_grid,
  seasonality.prior.scale = seasonality_grid)#,
#holidays.prior.scale = holidays_grid)
resultados_optimizacion <- data.frame()

for(i in 1:nrow(grid)) {
  
  m_temp <- prophet(
    train_data, 
    changepoint.range = grid$changepoint.range[i],
    changepoint.prior.scale = grid$changepoint.prior.scale[i],
    seasonality.prior.scale = grid$seasonality.prior.scale[i],
    #holidays.prior.scale = grid$holidays.prior.scale[i],
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE
  )
  
  df_cv <- cross_validation(
    m_temp, 
    initial = 365 * 8,  
    period = 365,       
    horizon = 365,      
    units = 'days'
  )
  
  df_p <- performance_metrics(df_cv)
  
  resultado_temp <- data.frame(
    range = grid$changepoint.range[i],
    changepoint = grid$changepoint.prior.scale[i],
    seasonality = grid$seasonality.prior.scale[i],
    #holidays = grid$holidays.prior.scale[i],
    rmse_promedio = mean(df_p$rmse)
  )
  
  resultados_optimizacion <- bind_rows(resultados_optimizacion, resultado_temp)
}

mejor_combinacion <- resultados_optimizacion %>% 
  arrange(rmse_promedio) %>% 
  head(1)

print("¡Los mejores hiperparámetros para Prophet son:")
print(mejor_combinacion)
# 2021 ----
## ARIMA ----
transporte <- transporte %>%
  filter(year(FECHA)<=2021)

n_test <- 12

graficar_serie(transporte, "TRANSPORTE")

# Descomponer serie
transporte_ts <- ts(transporte$TOTAL, start = c(2013, 1), frequency = 12)

decomposed <- decompose(transporte_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(transporte)
n_train <- n_total-n_test
train_data <- transporte[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- transporte[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data %>%
  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "transporte DIFF, LOG")
tsoutliers(train_ts)

dummy_2019 <- rep(0, n_train)

dummy_2019[77:78] <- 1

xreg_matrix <- cbind(D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 0), xreg = xreg_matrix,
                seasonal = list(order = c(2, 0, 0), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 0), xreg = xreg_matrix,
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "transporte")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo

## Test ----
dummy_test <- rep(0, n_test)
xreg_matrix <- cbind(D2019 = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2021, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "transporte")

## Prophet ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  changepoints = NULL,
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.01,
  growth = "linear",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL, 
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 1,
  holidays.prior.scale = 20,
  interval.width = 0.95,
  uncertainty.samples = 1000
)

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2021, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

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
  labs(title = "Validación del Modelo Prophet: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2022 ----
transporte <- datos_narcotrafico %>%
  filter(TIPO == "TRANSPORTE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
## ARIMA ----
transporte <- transporte %>%
  filter(year(FECHA)<=2022)

n_test <- 12

graficar_serie(transporte, "TRANSPORTE")

# Descomponer serie
transporte_ts <- ts(transporte$TOTAL, start = c(2013, 1), frequency = 12)

decomposed <- decompose(transporte_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(transporte)
n_train <- n_total-n_test
train_data <- transporte[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- transporte[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data %>%
  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "transporte DIFF, LOG")
tsoutliers(train_ts)

dummy_2019 <- rep(0, n_train)

dummy_2019[77:78] <- 1

xreg_matrix <- cbind(D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 0), xreg = xreg_matrix,
                seasonal = list(order = c(2, 0, 0), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 0), xreg = xreg_matrix,
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "transporte")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo

## Test ----
dummy_test <- rep(0, n_test)
xreg_matrix <- cbind(D2019 = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2022, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "transporte")

## Prophet ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  changepoints = NULL,
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.01,
  growth = "linear",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL, 
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 1,
  holidays.prior.scale = 20,
  interval.width = 0.95,
  uncertainty.samples = 1000
)

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2022, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

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
  labs(title = "Validación del Modelo Prophet: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2023 ----
transporte <- datos_narcotrafico %>%
  filter(TIPO == "TRANSPORTE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
## ARIMA ----
transporte <- transporte %>%
  filter(year(FECHA)<=2023)

n_test <- 12

graficar_serie(transporte, "TRANSPORTE")

# Descomponer serie
transporte_ts <- ts(transporte$TOTAL, start = c(2013, 1), frequency = 12)

decomposed <- decompose(transporte_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(transporte)
n_train <- n_total-n_test
train_data <- transporte[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- transporte[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data %>%
  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "transporte DIFF, LOG")
tsoutliers(train_ts)

dummy_2019 <- rep(0, n_train)

dummy_2019[77:78] <- 1

xreg_matrix <- cbind(D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 0), xreg = xreg_matrix,
                seasonal = list(order = c(2, 0, 0), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 0), xreg = xreg_matrix,
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "transporte")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo

## Test ----
dummy_test <- rep(0, n_test)
xreg_matrix <- cbind(D2019 = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2023, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "transporte")

## Prophet ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  changepoints = NULL,
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.01,
  growth = "linear",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL, 
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 1,
  holidays.prior.scale = 20,
  interval.width = 0.95,
  uncertainty.samples = 1000
)

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2023, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

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
  labs(title = "Validación del Modelo Prophet: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2024 ----
transporte <- datos_narcotrafico %>%
  filter(TIPO == "TRANSPORTE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
## ARIMA ----
transporte <- transporte %>%
  filter(year(FECHA)<=2024)

n_test <- 12

graficar_serie(transporte, "TRANSPORTE")

# Descomponer serie
transporte_ts <- ts(transporte$TOTAL, start = c(2013, 1), frequency = 12)

decomposed <- decompose(transporte_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(transporte)
n_train <- n_total-n_test
train_data <- transporte[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- transporte[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data %>%
  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "transporte DIFF, LOG")
tsoutliers(train_ts)

dummy_2019 <- rep(0, n_train)

dummy_2019[77:78] <- 1

xreg_matrix <- cbind(D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 0), xreg = xreg_matrix,
                seasonal = list(order = c(2, 0, 0), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 0), xreg = xreg_matrix,
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "transporte")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo

## Test ----
dummy_test <- rep(0, n_test)
xreg_matrix <- cbind(D2019 = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2024, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "transporte")

## Prophet ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  changepoints = NULL,
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.01,
  growth = "linear",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL, 
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 1,
  holidays.prior.scale = 20,
  interval.width = 0.95,
  uncertainty.samples = 1000
)

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2024, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

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
  labs(title = "Validación del Modelo Prophet: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2025 ----
transporte <- datos_narcotrafico %>%
  filter(TIPO == "TRANSPORTE") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
## ARIMA ----
transporte <- transporte %>%
  filter(year(FECHA)<=2025)

n_test <- 12

graficar_serie(transporte, "TRANSPORTE")

# Descomponer serie
transporte_ts <- ts(transporte$TOTAL, start = c(2013, 1), frequency = 12)

decomposed <- decompose(transporte_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(transporte)
n_train <- n_total-n_test
train_data <- transporte[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- transporte[(n_train+1):n_total, ]

# Estabilizar varianza
lambda <- BoxCox.lambda(train_ts)
print(lambda)

arima <- train_data %>%
  mutate(TOTAL=log(TOTAL+1))

# Estabilizar media
adf.test(arima$TOTAL)
kpss.test(arima$TOTAL)
diff_arima = diff(arima$TOTAL)
adf.test(diff_arima)
kpss.test(diff_arima)

diff_arima <- arima %>%
  mutate(TOTAL = c(NA,diff_arima))

# Ver outliers
graficar_serie(diff_arima, "transporte DIFF, LOG")
tsoutliers(train_ts)

dummy_2019 <- rep(0, n_train)

dummy_2019[77:78] <- 1

xreg_matrix <- cbind(D2019 = dummy_2019)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(1, 1, 0), xreg = xreg_matrix,
                seasonal = list(order = c(2, 0, 0), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(1, 1, 0), xreg = xreg_matrix,
                  seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "transporte")

checkresiduals(modelo)
Box.test(modelo$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(modelo_2)
Box.test(modelo_2$residuals, lag = 20, type = "Ljung-Box")

checkresiduals(auto)
Box.test(auto$residuals, lag = 20, type = "Ljung-Box")

AIC(modelo, modelo_2, auto)

# modelo

## Test ----
dummy_test <- rep(0, n_test)
xreg_matrix <- cbind(D2019 = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2025, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "transporte")

## Prophet ----
datos_prophet <- transporte %>%
  select(ds = FECHA, y = TOTAL)

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  changepoints = NULL,
  changepoint.range = 0.95,
  changepoint.prior.scale = 0.01,
  growth = "linear",
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL, 
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 1,
  holidays.prior.scale = 20,
  interval.width = 0.95,
  uncertainty.samples = 1000
)

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_transporte <- rbind(
  metricas_transporte, 
  data.frame(Año = 2025, Modelo = "Prophet", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

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
  labs(title = "Validación del Modelo Prophet: Transporte",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
