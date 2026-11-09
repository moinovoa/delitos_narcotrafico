# ==============================================================================
# PROYECTO: Análisis de Incidencia Delictiva
# SCRIPT: analisis_tipo_delito.R
# OBJETIVO: Leer CSVs crudos, limpiar valores nulos y hacer un EDA general.
# ==============================================================================
library(readxl)
library(tidyverse)
library(plotly)
library(tseries)
library(forecast)
library(prophet)
library(here)
# Creación base inicial ----
# Leer datos
datos <-read_excel(here('data', 'Fuero_federal_2012-2026_feb2026.xlsx'))
# Ver datos vacíos
nas <- datos %>%
  group_by(AÑO)%>%
  filter(is.na(MARZO))
# Despivotar columnas
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
saveRDS(datos_narcotrafico, here('data', 'datos_narcotrafico.rds'))

# Tipo de delito ----
# Gráfico de barras ed tipos de delito en carpetas de delitos contra la salud 
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
# Imprimir estadísticas descriptivas
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
