# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 09: Modelo 6 - Control por Segmentación Ocupacional (Limpieza y Sin Estudios)
# ==============================================================================

library(tidyverse)
library(haven)
library(sandwich)
library(lmtest)
library(car)

# 1. Cargar datos
# ------------------------------------------------------------------------------
if (file.exists("df_tig_procesada.rds")) {
  df_tig <- readRDS("df_tig_procesada.rds")
} else {
  source("scripts/03_clasificacion_carreras.R")
}

# 2. Construcción de variables específicas
# ------------------------------------------------------------------------------
# - trabaja_limpieza: CIUO-08 grupo 91 (Personal de limpieza de oficinas, 
#   hoteles, establecimientos y servicio doméstico en hogares).
# - sin_estudios_sup: personas cuya máxima educación no alcanza nivel superior
#   (base de comparación con profesionales y técnicos).
# - sin_estudios_basica: personas con a lo más educación básica (<= 8 años esc).

df_tig <- df_tig |>
  mutate(
    oficio4_num = zap_labels(oficio4_08),
    trabaja_limpieza = if_else(floor(oficio4_num / 100) == 91, 1L, 0L),
    sin_estudios_sup = if_else(area_estudio == "Sin educación superior", 1L, 0L),
    sin_estudios_basica = if_else(esc <= 8, 1L, 0L)
  )

# Verificar frecuencias
cat("Frecuencia de Trabajadores de Limpieza en la muestra:\n")
print(table(df_tig$trabaja_limpieza))
cat("\nFrecuencia de Trabajadores Sin Educación Superior:\n")
print(table(df_tig$sin_estudios_sup))

# 3. Estimación del Modelo 6
# ------------------------------------------------------------------------------
# Modelo 6: Incorpora mujer * nhijos + sin_estudios_sup + trabaja_limpieza
# junto a controles demográficos y laborales habituales.

m6 <- lm(ly ~ mujer * nhijos + sin_estudios_sup + trabaja_limpieza + 
           edad + edad2 + horas + rural + region,
         data = df_tig)

# Inferencia robusta HC1
ct_m6 <- coeftest(m6, vcov. = vcovHC(m6, type = "HC1"))

cat("\n==============================================================================\n")
cat("RESULTADOS MODELO 6: SEGMENTACIÓN EN LIMPIEZA Y SIN ESTUDIOS SUPERIORES\n")
cat("==============================================================================\n")
print(ct_m6[c("(Intercept)", "mujer", "nhijos", "mujer:nhijos", 
              "sin_estudios_sup", "trabaja_limpieza", "horas", "edad"), ])

r2_m6 <- summary(m6)$adj.r.squared
cat(sprintf("\nR^2 Ajustado Modelo 6: %.4f\n", r2_m6))

# Test lineal del efecto neto en mujeres (beta_2 + beta_3)
lin_test_m6 <- linearHypothesis(m6, "nhijos + mujer:nhijos = 0", vcov. = vcovHC(m6, type = "HC1"))
cat("\nTest H0: nhijos + mujer:nhijos = 0 (Efecto neto en mujeres):\n")
print(lin_test_m6)

# Guardar base actualizada y modelo
saveRDS(df_tig, "df_tig_procesada.rds")
save(m6, file = "modelo_vulnerabilidad_m6.RData")
cat("\nScript 09 ejecutado y guardado exitosamente.\n")
