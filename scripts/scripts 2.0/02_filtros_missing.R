# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 02: Filtros de Muestra, Tratamiento de Missings y Variables Econométricas
# ==============================================================================

library(tidyverse)
library(haven)

# 1. Asegurar dependencias de datos
# ------------------------------------------------------------------------------
if (!exists("casen_2024") ||
    !all(c("nhijos", "nhijos_menor", "es_jefe_pareja") %in% names(casen_2024))) {
  message("Ejecutando scripts/01_construccion_nhijos.R previamente...")
  source("scripts/01_construccion_nhijos.R")
}

# 2. Filtrado de Submuestra Objetivo Inicial
# ------------------------------------------------------------------------------
muestra_base <- casen_2024 |>
  mutate(
    activ_num = zap_labels(activ),
    edad_num  = as.numeric(zap_labels(edad)),
    sexo_num  = zap_labels(sexo)
  ) |>
  filter(
    edad_num >= 25,
    edad_num <= 45,
    activ_num == 1,
    es_jefe_pareja == TRUE
  )

n_inicial <- nrow(muestra_base)
message("Muestra objetivo inicial (ocupados 25-45 años, jefes/parejas de hogar o de núcleo): ", n_inicial)

# 3. Diagnóstico formal de Valores Perdidos (Missing Values)
# ------------------------------------------------------------------------------
resumen_na <- muestra_base |>
  summarise(
    n = n(),
    na_yoprcor = sum(is.na(yoprcor)),
    pct_na_yoprcor = round(100 * na_yoprcor / n, 2),
    na_esc = sum(is.na(esc)),
    pct_na_esc = round(100 * na_esc / n, 2)
  )
print(resumen_na)

diagnostico_o15 <- muestra_base |>
  mutate(o15_num = zap_labels(o15)) |>
  group_by(o15_num) |>
  summarise(
    n = n(),
    na_yoprcor = sum(is.na(yoprcor)),
    pct_na = round(100 * na_yoprcor / n, 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_na))
print(diagnostico_o15)

# 4. Construcción y tipificación de variables econométricas
# ------------------------------------------------------------------------------
df_tig <- muestra_base |>
  filter(!is.na(yoprcor), yoprcor > 0, !is.na(esc)) |>
  mutate(
    # Variable Dependiente:
    ly = log(as.numeric(yoprcor)),
    
    # Ambas dummies disponibles para análisis dual:
    mujer  = if_else(sexo_num == 2, 1L, 0L), # 1 = Mujer, 0 = Hombre (Base: Hombre)
    hombre = if_else(sexo_num == 1, 1L, 0L), # 1 = Hombre, 0 = Mujer (Base: Mujer)
    
    nhijos = as.integer(nhijos),
    nhijos_menor = as.integer(nhijos_menor),
    
    # Controles de Capital Humano y Demográficos:
    edad  = as.numeric(edad_num),
    edad2 = edad^2,
    esc   = as.numeric(zap_labels(esc)),
    
    # Controles Laborales:
    horas = as.numeric(zap_labels(o10)),
    
    # Controles Geográficos:
    region = as.factor(zap_labels(region)),
    rural  = if_else(zap_labels(area) == 2, 1L, 0L)
  )

# Imputación de horas si presenta missing menor
if (any(is.na(df_tig$horas))) {
  mediana_h <- median(df_tig$horas, na.rm = TRUE)
  df_tig <- df_tig |> mutate(horas = if_else(is.na(horas), mediana_h, horas))
}

# 5. Balance Muestral Final
# ------------------------------------------------------------------------------
n_final <- nrow(df_tig)
message("=================================================================")
message("Muestra final para estimación econométrica: N = ", n_final)
message("Hombres = ", sum(df_tig$hombre == 1), " | Mujeres = ", sum(df_tig$mujer == 1))
message("=================================================================")
