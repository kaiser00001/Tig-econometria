# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 06: Pruebas de Robustez y Diagnósticos Econométricos (Semana 6)
# NOTA: Base = MUJERES (dummy hombre: 1 = Hombre, 0 = Mujer)
# ==============================================================================

library(tidyverse)
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

if (!"hombre" %in% names(df_tig)) {
  df_tig$hombre <- if_else(df_tig$mujer == 0, 1L, 0L)
}

message("Muestra cargada: N = ", nrow(df_tig))

# 2. Robustez 1: Regresiones Separadas por Sexo
# ------------------------------------------------------------------------------
m_mujeres <- lm(ly ~ nhijos + edad + edad2 + esc + horas + rural + region + area_estudio,
                data = df_tig |> filter(hombre == 0))

m_hombres <- lm(ly ~ nhijos + edad + edad2 + esc + horas + rural + region + area_estudio,
                data = df_tig |> filter(hombre == 1))

ct_m <- coeftest(m_mujeres, vcov. = vcovHC(m_mujeres, type = "HC1"))
ct_h <- coeftest(m_hombres, vcov. = vcovHC(m_hombres, type = "HC1"))

b_m <- ct_m["nhijos", 1]; se_m <- ct_m["nhijos", 2]; p_m <- ct_m["nhijos", 4]
b_h <- ct_h["nhijos", 1]; se_h <- ct_h["nhijos", 2]; p_h <- ct_h["nhijos", 4]

message("\n==============================================================================")
message("ROBUSTEZ 1: ESTIMACIÓN POR SUBMUESTRAS SEPARADAS")
message("==============================================================================")
cat(sprintf("Mujeres - Efecto nhijos: %.4f (SE: %.4f, p-val: %.4f) -> Variación: %.2f%%\n",
            b_m, se_m, p_m, (exp(b_m) - 1) * 100))
cat(sprintf("Hombres - Efecto nhijos: %.4f (SE: %.4f, p-val: %.4f) -> Variación: %.2f%%\n",
            b_h, se_h, p_h, (exp(b_h) - 1) * 100))
cat(sprintf("Brecha no restringida (Hombres - Mujeres): +%.4f (+%.2f p.p.)\n",
            b_h - b_m, ((exp(b_h) - 1) - (exp(b_m) - 1)) * 100))

# 3. Robustez 2: Forma Funcional No Lineal (Dummies por Número de Hijos)
# ------------------------------------------------------------------------------
df_tig <- df_tig |>
  mutate(
    tramo_h = case_when(
      nhijos == 0 ~ "0_hijos",
      nhijos == 1 ~ "1_hijo",
      nhijos == 2 ~ "2_hijos",
      nhijos >= 3 ~ "3_mas_hijos"
    ),
    tramo_h = factor(tramo_h, levels = c("0_hijos", "1_hijo", "2_hijos", "3_mas_hijos"))
  )

m_nolineal <- lm(ly ~ hombre * tramo_h + edad + edad2 + esc + horas + rural + region + area_estudio,
                 data = df_tig)
ct_nl <- coeftest(m_nolineal, vcov. = vcovHC(m_nolineal, type = "HC1"))

message("\n==============================================================================")
message("ROBUSTEZ 2: EFECTOS NO LINEALES (DUMMIES POR CANTIDAD DE HIJOS, BASE: MUJER)")
message("==============================================================================")
vars_nl <- c("tramo_h1_hijo", "tramo_h2_hijos", "tramo_h3_mas_hijos",
             "hombre:tramo_h1_hijo", "hombre:tramo_h2_hijos", "hombre:tramo_h3_mas_hijos")
print(ct_nl[rownames(ct_nl) %in% vars_nl, ])

# 4. Diagnósticos Econométricos
# ------------------------------------------------------------------------------
bp_test <- bptest(m_nolineal)

m4_vif <- lm(ly ~ hombre + nhijos + edad + edad2 + esc + horas + rural + region + area_estudio,
             data = df_tig)
vif_resultado <- vif(m4_vif)

message("\n==============================================================================")
message("DIAGNÓSTICOS ECONOMÉTRICOS:")
message("------------------------------------------------------------------------------")
cat(sprintf("1. Test de Breusch-Pagan: BP = %.2f, df = %d, p-valor = %.5e\n",
            bp_test$statistic, bp_test$parameter, bp_test$p.value))
message("\n2. Factores de Inflación de la Varianza (VIF):")
print(vif_resultado)
message("==============================================================================")

save(m_hombres, m_mujeres, m_nolineal, file = "modelos_robustez.RData")
