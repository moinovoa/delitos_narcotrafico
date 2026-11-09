# ==============================================================================
# PROYECTO: Análisis de Incidencia Delictiva
# SCRIPT: trafico.R
# OBJETIVO: Analizar la serie de tiempo de trafico
# ==============================================================================
# Mejores modelos 
# SARIMA(0,1,1)(0,0,1)
# Prophet cpr = 0.8, cps=0.01, sps=5, hps=10
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

# Creación base trafico
trafico <- datos_narcotrafico %>%
  filter(TIPO == "TRAFICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
trafico[11,2] = 1

graficar_serie(na.omit(trafico), "TRAFICO")

metricas_trafico <- data.frame(Año = numeric(),
                               Modelo = character(),
                               RMSE   = numeric(),
                               MAE    = numeric(),
                               MAPE   = numeric(),
                               stringsAsFactors = FALSE)

# Grid ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL) %>%
  mutate(salto_nivel = ifelse(ds >= as.Date("2019-06-01"), 1,0))

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
trafico <- trafico %>%
  filter(year(FECHA)<=2021)

n_test <- 12

graficar_serie(na.omit(trafico), "TRAFICO")

# Descomponer serie
trafico_ts <- ts(na.omit(trafico$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(trafico_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(trafico)
n_train <- n_total-n_test
train_data <- trafico[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- trafico[(n_train+1):n_total, ]

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
graficar_serie(diff_arima, "TRAFICO DIFF, LOG")
tsoutliers(train_ts)

dummy_2015 <- rep(0, n_train)
dummy_2017 <- rep(0, n_train)
dummy_CN <- rep(0, n_train)

dummy_2015[35] <- 1
dummy_2017[56] <- 1
dummy_CN[79:n_train] <- 1

xreg_matrix <- cbind(D2015 = dummy_2015, D2017 = dummy_2017, 
                     DCN = dummy_CN)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(0, 1, 1), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)(0,0,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "TRAFICO")

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

# modelo

## Test ----
dummy_test <- rep(1, n_test)
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2015 = dummy_test_2, D2017 = dummy_test_2, DCN = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_trafico <- rbind(
  metricas_trafico, 
  data.frame(Año = 2021, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "trafico")

## Prophet ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL) %>%
  mutate(salto_nivel = ifelse(ds >= as.Date("2019-06-01"), 1,0))

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

#m_temp <- prophet(train_data, changepoint.range = 0.8, yearly.seasonality = TRUE,
#                  changepoint.prior.scale =  0.3)
#changepoints <- m_temp$changepoints
#changepoints <- c(changepoints, as.Date("2019-11-01"), as.Date("2019-07-01")) 

modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, #changepoints
  changepoint.range = 0.8,
  changepoint.prior.scale = 0.01,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 5,
  holidays.prior.scale = NULL,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- add_regressor(modelo, 'salto_nivel', prior.scale = 0.05, standardize = 'auto')

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

future <- future %>%
  left_join(datos_prophet %>% select(ds, salto_nivel), by = "ds") %>%
  mutate(salto_nivel = if_else(is.na(salto_nivel), 1, salto_nivel))

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_trafico <- rbind(
  metricas_trafico, 
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
  labs(title = "Validación del Modelo Prophet: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2022 ----
trafico <- datos_narcotrafico %>%
  filter(TIPO == "TRAFICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
trafico[11,2] = 1
## ARIMA ----
trafico <- trafico %>%
  filter(year(FECHA)<=2022)

n_test <- 12

graficar_serie(na.omit(trafico), "TRAFICO")

# Descomponer serie
trafico_ts <- ts(na.omit(trafico$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(trafico_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(trafico)
n_train <- n_total-n_test
train_data <- trafico[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- trafico[(n_train+1):n_total, ]

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
graficar_serie(diff_arima, "TRAFICO DIFF, LOG")
tsoutliers(train_ts)

dummy_2015 <- rep(0, n_train)
dummy_2017 <- rep(0, n_train)
dummy_CN <- rep(0, n_train)

dummy_2015[35] <- 1
dummy_2017[56] <- 1
dummy_CN[79:n_train] <- 1

xreg_matrix <- cbind(D2015 = dummy_2015, D2017 = dummy_2017, 
                     DCN = dummy_CN)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(0, 1, 1), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  #seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)(0,0,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "TRAFICO")

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

# modelo

## Test ----
dummy_test <- rep(1, n_test)
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2015 = dummy_test_2, D2017 = dummy_test_2, DCN = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_trafico <- rbind(
  metricas_trafico, 
  data.frame(Año = 2022, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "trafico")

## Prophet ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL) %>%
  mutate(salto_nivel = ifelse(ds >= as.Date("2019-06-01"), 1,0))

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, #changepoints
  changepoint.range = 0.8,
  changepoint.prior.scale = 0.01,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 5,
  holidays.prior.scale = NULL,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- add_regressor(modelo, 'salto_nivel', prior.scale = 0.05, standardize = 'auto')

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

future <- future %>%
  left_join(datos_prophet %>% select(ds, salto_nivel), by = "ds") %>%
  mutate(salto_nivel = if_else(is.na(salto_nivel), 1, salto_nivel))

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_trafico <- rbind(
  metricas_trafico, 
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
  labs(title = "Validación del Modelo Prophet: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2023 ----
trafico <- datos_narcotrafico %>%
  filter(TIPO == "TRAFICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
trafico[11,2] = 1
## ARIMA ----
trafico <- trafico %>%
  filter(year(FECHA)<=2023)

n_test <- 12

graficar_serie(na.omit(trafico), "TRAFICO")

# Descomponer serie
trafico_ts <- ts(na.omit(trafico$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(trafico_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(trafico)
n_train <- n_total-n_test
train_data <- trafico[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- trafico[(n_train+1):n_total, ]

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
graficar_serie(diff_arima, "TRAFICO DIFF, LOG")
tsoutliers(train_ts)

dummy_2015 <- rep(0, n_train)
dummy_2017 <- rep(0, n_train)
dummy_CN <- rep(0, n_train)

dummy_2015[35] <- 1
dummy_2017[56] <- 1
dummy_CN[79:n_train] <- 1

xreg_matrix <- cbind(D2015 = dummy_2015, D2017 = dummy_2017, 
                     DCN = dummy_CN)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(0, 1, 1), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  #seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)(0,0,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "TRAFICO")

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

# modelo

## Test ----
dummy_test <- rep(1, n_test)
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2015 = dummy_test_2, D2017 = dummy_test_2, DCN = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_trafico <- rbind(
  metricas_trafico, 
  data.frame(Año = 2023, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "trafico")

## Prophet ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL) %>%
  mutate(salto_nivel = ifelse(ds >= as.Date("2019-06-01"), 1,0))

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, #changepoints
  changepoint.range = 0.8,
  changepoint.prior.scale = 0.01,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 5,
  holidays.prior.scale = NULL,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- add_regressor(modelo, 'salto_nivel', prior.scale = 0.05, standardize = 'auto')

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

future <- future %>%
  left_join(datos_prophet %>% select(ds, salto_nivel), by = "ds") %>%
  mutate(salto_nivel = if_else(is.na(salto_nivel), 1, salto_nivel))

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_trafico <- rbind(
  metricas_trafico, 
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
  labs(title = "Validación del Modelo Prophet: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2024 ----
trafico <- datos_narcotrafico %>%
  filter(TIPO == "TRAFICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
trafico[11,2] = 1
## ARIMA ----
trafico <- trafico %>%
  filter(year(FECHA)<=2024)

n_test <- 12

graficar_serie(na.omit(trafico), "TRAFICO")

# Descomponer serie
trafico_ts <- ts(na.omit(trafico$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(trafico_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(trafico)
n_train <- n_total-n_test
train_data <- trafico[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- trafico[(n_train+1):n_total, ]

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
graficar_serie(diff_arima, "TRAFICO DIFF, LOG")
tsoutliers(train_ts)

dummy_2015 <- rep(0, n_train)
dummy_2017 <- rep(0, n_train)
dummy_CN <- rep(0, n_train)

dummy_2015[35] <- 1
dummy_2017[56] <- 1
dummy_CN[79:n_train] <- 1

xreg_matrix <- cbind(D2015 = dummy_2015, D2017 = dummy_2017, 
                     DCN = dummy_CN)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(0, 1, 1), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  #seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)(0,0,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "TRAFICO")

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

# modelo

## Test ----
dummy_test <- rep(1, n_test)
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2015 = dummy_test_2, D2017 = dummy_test_2, DCN = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_trafico <- rbind(
  metricas_trafico, 
  data.frame(Año = 2024, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "trafico")

## Prophet ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL) %>%
  mutate(salto_nivel = ifelse(ds >= as.Date("2019-06-01"), 1,0))

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, #changepoints
  changepoint.range = 0.8,
  changepoint.prior.scale = 0.01,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 5,
  holidays.prior.scale = NULL,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- add_regressor(modelo, 'salto_nivel', prior.scale = 0.05, standardize = 'auto')

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

future <- future %>%
  left_join(datos_prophet %>% select(ds, salto_nivel), by = "ds") %>%
  mutate(salto_nivel = if_else(is.na(salto_nivel), 1, salto_nivel))

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_trafico <- rbind(
  metricas_trafico, 
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
  labs(title = "Validación del Modelo Prophet: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()

# 2025 ----
trafico <- datos_narcotrafico %>%
  filter(TIPO == "TRAFICO") %>%
  group_by(FECHA) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE))%>%
  ungroup()%>%
  filter(!year(FECHA) %in% c(2012,2026))
trafico[11,2] = 1
## ARIMA ----
trafico <- trafico %>%
  filter(year(FECHA)<=2025)

n_test <- 12

graficar_serie(na.omit(trafico), "TRAFICO")

# Descomponer serie
trafico_ts <- ts(na.omit(trafico$TOTAL), start = c(2013, 1), frequency = 12)

decomposed <- decompose(trafico_ts, type = "multiplicative")
plot(decomposed)

# Ajustar n's
n_total <- nrow(trafico)
n_train <- n_total-n_test
train_data <- trafico[1:n_train, ]
train_ts <- ts(train_data$TOTAL, start = c(2013, 1), frequency = 12)
test_data  <- trafico[(n_train+1):n_total, ]

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
graficar_serie(diff_arima, "TRAFICO DIFF, LOG")
tsoutliers(train_ts)

dummy_2015 <- rep(0, n_train)
dummy_2017 <- rep(0, n_train)
dummy_CN <- rep(0, n_train)

dummy_2015[35] <- 1
dummy_2017[56] <- 1
dummy_CN[79:n_train] <- 1

xreg_matrix <- cbind(D2015 = dummy_2015, D2017 = dummy_2017, 
                     DCN = dummy_CN)

# Autocorrelaciones
diff_arima = diff(arima$TOTAL)
par(mfrow = c(1, 2))
acf(diff_arima, main = "ACF de Delitos", lag.max = 60)
pacf(diff_arima, main = "PACF de Delitos", lag.max = 60)

modelo <- Arima(arima$TOTAL, 
                order = c(0, 1, 1), xreg = xreg_matrix,
                seasonal = list(order = c(0, 0, 1), period = 12),
                lambda = 1)
modelo_2 <- Arima(arima$TOTAL, 
                  order = c(0, 1, 1), xreg = xreg_matrix,
                  #seasonal = list(order = c(1, 0, 0), period = 12),
                  lambda = 1)

auto <- auto.arima(train_ts, seasonal = TRUE, xreg = xreg_matrix,
                   stepwise = FALSE, approximation = FALSE, lambda = "auto")
#(0,1,1)(0,0,1)

summary(modelo)
summary(modelo_2)
summary(auto)

train_data$PREDICCIONES <- exp(fitted(modelo))-1
train_data$PREDICCIONES_2 <- exp(fitted(modelo_2))-1
train_data$PREDICCIONES_AUTO <-  fitted(auto)

graficar_modelos(train_data, "TRAFICO")

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

# modelo

## Test ----
dummy_test <- rep(1, n_test)
dummy_test_2 <- rep(0, n_test)
xreg_matrix <- cbind(D2015 = dummy_test_2, D2017 = dummy_test_2, DCN = dummy_test)

pronostico <- forecast(modelo, h = n_test, xreg = xreg_matrix)

reales <- test_data$TOTAL
predichos <- exp(as.numeric(pronostico$mean))-1

metricas <- accuracy(predichos, reales)
print(metricas)

metricas_trafico <- rbind(
  metricas_trafico, 
  data.frame(Año = 2025, Modelo = "ARIMA", RMSE = metricas[1, "RMSE"], MAE = metricas[1, "MAE"], MAPE = metricas[1, "MAPE"])
)

graficar_validacion(train_data, test_data, predichos, pronostico, metricas, "trafico")

## Prophet ----
datos_prophet <- trafico %>%
  select(ds = FECHA, y = TOTAL) %>%
  mutate(salto_nivel = ifelse(ds >= as.Date("2019-06-01"), 1,0))

n_total <- nrow(datos_prophet)
n_train <- n_total-12

train_data <- datos_prophet[1:n_train, ]
test_data  <- datos_prophet[(n_train+1):n_total, ]

modelo <- prophet(
  seed = 12345,
  growth = "linear",
  changepoints = NULL, #changepoints
  changepoint.range = 0.8,
  changepoint.prior.scale = 0.01,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  holidays = NULL,
  seasonality.mode = "multiplicative",
  seasonality.prior.scale = 5,
  holidays.prior.scale = NULL,
  interval.width = 0.95,
  uncertainty.samples = 1000,
)

modelo <- add_regressor(modelo, 'salto_nivel', prior.scale = 0.05, standardize = 'auto')

modelo <- fit.prophet(modelo, train_data)

future <- make_future_dataframe(
  modelo,
  periods = nrow(test_data),
  freq = "month"
)

future <- future %>%
  left_join(datos_prophet %>% select(ds, salto_nivel), by = "ds") %>%
  mutate(salto_nivel = if_else(is.na(salto_nivel), 1, salto_nivel))

forecast_prophet <- predict(modelo, future)
predicciones <- tail(forecast_prophet$yhat, nrow(test_data))

metricas <- accuracy(predicciones, test_data$y)
metricas

forecast_test <- tail(forecast_prophet, nrow(test_data))

plot(modelo, forecast_prophet) + add_changepoints_to_plot(modelo)

metricas_trafico <- rbind(
  metricas_trafico, 
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
  labs(title = "Validación del Modelo Prophet: Tráfico",
       subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
       x = "Fecha", y = "Delitos", color = "Serie") +
  theme_minimal()
