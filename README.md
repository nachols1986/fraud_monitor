# Fraud Payments Monitor

Este script en R se utiliza para monitorear y analizar los datos de pagos fraudulentos en un sistema de pagos. El objetivo principal es proporcionar métricas y análisis sobre los patrones de fraude en los pagos.

## Requisitos

El script utiliza varios paquetes de R, que se pueden instalar ejecutando el siguiente código:

```R
# Limpio la memoria
rm(list=ls())
gc()

# Package names
packages <- c("dplyr", "lubridate", "data.table", "googledrive", "readxl", "gtools", 
              "googlesheets4", "naniar", "stringr", "writexl", "fuzzyjoin")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
```

## Uso

Para utilizar este script, sigue los siguientes pasos:

1. Asegúrate de tener todos los paquetes necesarios instalados.
2. Ejecuta el script en un entorno de R.

## Funcionamiento

El script realiza las siguientes tareas principales:

1. Carga y procesa los datos de pagos.
2. Realiza análisis estadísticos y cálculos de métricas sobre los datos.
3. Genera visualizaciones y tablas resumen de los resultados.
4. Guarda los resultados en hojas de cálculo de Google Sheets para su posterior análisis y presentación.

## Resultados

Los resultados se dividen en tres niveles de granularidad temporal:

- **Diario:** Muestra métricas diarias sobre los pagos fraudulentos.
- **Semanal:** Resume las métricas semanales de los pagos fraudulentos.
- **Mensual:** Presenta un resumen mensual de los datos y métricas de fraudes.

Este es el flujo básico de trabajo del script, que te permite analizar y monitorear los datos de pagos fraudulentos de manera eficiente.
