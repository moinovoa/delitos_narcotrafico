# Librerías ----
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(tseries)
library(forecast)
library(prophet)

# Funciones ----
graficar_serie <- function(serie, nombre){
  ggplot(serie, aes(x=FECHA, y=TOTAL))+
    geom_line(color = "black", linewidth = 1) +
    geom_point() +
    labs(title = paste0("Evolución Mensual (",nombre,")"),
         x="Fecha", y="Total de incidencias")
  
}
graficar_modelos <- function(dataset, nombre){
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
  plot_validación <- ggplot() +
    geom_line(data = train_data, aes(x = FECHA, y = TOTAL, color = "Entrenamiento"), size = 1) +
    geom_line(data = test_data, aes(x = FECHA, y = TOTAL, color = "Real (Test)"), size = 1) +
    geom_line(aes(x = test_data$FECHA, y = predichos, color = "Predicción"), 
              size = 1, linetype = "dashed") +
    geom_ribbon(aes(x = test_data$FECHA, 
                    ymin = (as.numeric(pronostico$lower[,2]))^2, 
                    ymax = (as.numeric(pronostico$upper[,2]))^2), 
                fill = "steelblue", alpha = 0.2) +
    labs(title = paste0("Validación del Modelo ARIMA: ", nombre),
         subtitle = paste("MAPE en Test Set:", round(metricas[1, "MAPE"], 2), "%"),
         x = "Fecha", y = "Delitos", color = "Serie") +
    theme_minimal()
  
  print(plot_validación)
}
graficar_histograma <- function(estado, nombre){
  ggplot(estado, aes(x = TIPO, y = TOTAL))+
  geom_col(fill = "skyblue")+
  labs(title = paste("Delitos en: ", nombre))+
  theme_minimal()+
  theme(legend.position = "bottom")
}

# Creación base inicial ----
ruta <-"C:\\Users\\mois_\\OneDrive\\Servicio social\\Fuero_federal_2012-2026_feb2026.xlsx" #"C:\\Users\\Moises Novoa\\OneDrive\\nuevodrive\\OneDrive\\Servicio social\\Fuero_federal_2012-2026_feb2026.xlsx"#
excel_sheets(ruta)
datos <-read_excel(ruta)
nas <- datos %>%
  group_by(AÑO)%>%
  filter(is.na(MARZO))
datos_narcotrafico = datos %>%
  filter(CONCEPTO == "CONTRA LA SALUD")
conteo_ordenado <- datos_narcotrafico %>% 
  count(TIPO, sort = TRUE)
datos_narcotrafico <- datos_narcotrafico %>%
  pivot_longer(
    cols = ENERO:DICIEMBRE, 
    names_to = "MES",      
    values_to = "CONTEO"    
  ) 
datos_narcotrafico <- datos_narcotrafico %>% mutate(
  MES_NUM = match(MES, c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", 
                         "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", 
                         "NOVIEMBRE", "DICIEMBRE")),
  FECHA = as.Date(paste(AÑO, MES_NUM, "01", sep = "-"))
)
datos_narcotrafico <- datos_narcotrafico[, c("AÑO", "MES_NUM", "MES", "FECHA", 
                                             "INEGI", "ENTIDAD", "LEY", "CONCEPTO",
                                             "TIPO", "CONTEO")]

# Tipo de delito ----
composicion <- datos_narcotrafico %>%
  group_by(ENTIDAD) %>%
  summarize(TOTAL = sum(CONTEO, na.rm = TRUE)) %>%
  mutate(PORCENTAJE = (TOTAL / sum(TOTAL)) * 100)%>%
  arrange(desc(TOTAL))%>%
  slice_head(n = 10)

ggplot(composicion, aes(x = ENTIDAD, y = TOTAL))+
       geom_col(colour = "blue", fill = "sky blue")+
       labs(title = "Delitos x estado")+theme_minimal() +
         theme(legend.position = "bottom")

# Estadísticas ----
descriptivas <- datos_narcotrafico %>%
  group_by(TIPO) %>%
  summarize(
    MEDIA_MENSUAL = mean(CONTEO, na.rm = TRUE),
    MEDIANA_MENSUAL = median(CONTEO, na.rm = TRUE),
    DESV_EST = sd(CONTEO, na.rm = TRUE),
    MAXIMO = max(CONTEO, na.rm = TRUE),
    TOTAL_HIST = sum(CONTEO, na.rm = TRUE)
  )
descriptivas
