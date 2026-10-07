# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 05: Estimación Econométrica de Modelos MCO Secuenciales (Semanas 4-5)
# Enfoque Principal: Base Hombres (dummy 'mujer') + Modelo Dual (dummy 'hombre')
# ==============================================================================

paquetes <- c("tidyverse", "sandwich", "lmtest", "car")
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# 1. Cargar datos procesados
# ------------------------------------------------------------------------------
if (file.exists("df_tig_procesada.rds")) {
  df_tig <- readRDS("df_tig_procesada.rds")
} else {
  source("scripts/03_clasificacion_carreras.R")
}

# Asegurar existencia de ambas dummies
if (!"hombre" %in% names(df_tig)) df_tig$hombre <- if_else(df_tig$mujer == 0, 1L, 0L)
if (!"mujer" %in% names(df_tig))  df_tig$mujer  <- if_else(df_tig$hombre == 0, 1L, 0L)

message("Muestra cargada: N = ", nrow(df_tig))

# 2. Estimación Secuencial de Modelos MCO (Enfoque Principal: Base = Hombres)
# ------------------------------------------------------------------------------
# ly = b0 + b1*mujer + b2*nhijos + b3*(mujer * nhijos) + u
m1 <- lm(ly ~ mujer * nhijos, data = df_tig)
m2 <- lm(ly ~ mujer * nhijos + edad + edad2 + esc, data = df_tig)
m3 <- lm(ly ~ mujer * nhijos + edad + edad2 + esc + horas + rural + region, data = df_tig)
m4 <- lm(ly ~ mujer * nhijos + edad + edad2 + esc + horas + rural + region + area_estudio, data = df_tig)
m5 <- lm(ly ~ mujer * nhijos_menor + edad + edad2 + esc + horas + rural + region + area_estudio, data = df_tig)

# Modelo Dual Espejo (Base = Mujeres) para verificación y lectura directa
m4_base_mujer <- lm(ly ~ hombre * nhijos + edad + edad2 + esc + horas + rural + region + area_estudio, data = df_tig)

# 3. Errores Estándar Robustos a Heterocedasticidad (HC1)
# ------------------------------------------------------------------------------
obtener_robustas <- function(modelo) {
  vcov_hc1 <- vcovHC(modelo, type = "HC1")
  coeftest(modelo, vcov. = vcov_hc1)
}

ct1 <- obtener_robustas(m1)
ct2 <- obtener_robustas(m2)
ct3 <- obtener_robustas(m3)
ct4 <- obtener_robustas(m4)
ct5 <- obtener_robustas(m5)
ct4_bm <- obtener_robustas(m4_base_mujer)

# 4. Tabla Comparativa Secuencial Principal (Base: Hombres)
# ------------------------------------------------------------------------------
coefs_interes <- c("(Intercept)", "mujer", "nhijos", "mujer:nhijos", 
                   "nhijos_menor", "mujer:nhijos_menor", "esc", "edad", "horas")

extraer_resumen <- function(ct, m, nombre) {
  coef_df <- as.data.frame(ct[, 1:4])
  colnames(coef_df) <- c("Estimate", "StdError", "t_val", "p_val")
  coef_df$Variable <- rownames(coef_df)
  
  coef_df |>
    filter(Variable %in% coefs_interes) |>
    mutate(
      Signif = case_when(
        p_val < 0.01 ~ "***",
        p_val < 0.05 ~ "**",
        p_val < 0.1  ~ "*",
        TRUE ~ ""
      ),
      Salida = sprintf("%.4f %s (%.4f)", Estimate, Signif, StdError),
      Modelo = nombre,
      N = nobs(m),
      R2_adj = round(summary(m)$adj.r.squared, 4)
    ) |>
    select(Modelo, Variable, Salida, N, R2_adj)
}

res_m1 <- extraer_resumen(ct1, m1, "M1: Base")
res_m2 <- extraer_resumen(ct2, m2, "M2: + Capital Humano")
res_m3 <- extraer_resumen(ct3, m3, "M3: + Laboral/Región")
res_m4 <- extraer_resumen(ct4, m4, "M4: Principal")
res_m5 <- extraer_resumen(ct5, m5, "M5: Robustez <18")

tabla_modelos <- bind_rows(res_m1, res_m2, res_m3, res_m4, res_m5) |>
  pivot_wider(names_from = Modelo, values_from = Salida)

message("\n==============================================================================")
message("TABLA PRINCIPAL MCO: BASE HOMBRES (ERRORES ROBUSTOS HC1)")
message("Formato: Coeficiente Error_Estándar (*** p<0.01, ** p<0.05, * p<0.1)")
message("==============================================================================")
print(as.data.frame(tabla_modelos), row.names = FALSE)

# 5. Comparación Dual: Base Hombre vs. Base Mujer (Modelo 4)
# ------------------------------------------------------------------------------
b2_h <- coef(m4)["nhijos"]
b3_h <- coef(m4)["mujer:nhijos"]
efecto_mujeres_m4 <- b2_h + b3_h
se_efecto_mujeres <- sqrt(vcovHC(m4, type = "HC1")["nhijos", "nhijos"] + 
                          vcovHC(m4, type = "HC1")["mujer:nhijos", "mujer:nhijos"] + 
                          2 * vcovHC(m4, type = "HC1")["nhijos", "mujer:nhijos"])

a2_m <- coef(m4_base_mujer)["nhijos"]
a3_m <- coef(m4_base_mujer)["hombre:nhijos"]
se_a2_m <- ct4_bm["nhijos", 2]

tabla_dual <- tibble(
  `Concepto Económico` = c(
    "Efecto Hijos en Hombres",
    "Efecto Hijos en Mujeres (Child Penalty)",
    "Diferencial por Sexo",
    "Brecha Base sin Hijos"
  ),
  `Enfoque 1 (Base Hombres, dummy 'mujer')` = c(
    sprintf("%.4f ** (beta_2 directo)", b2_h),
    sprintf("%.4f *** [SE: %.4f] (beta_2 + beta_3)", efecto_mujeres_m4, se_efecto_mujeres),
    sprintf("%.4f *** (beta_3)", b3_h),
    sprintf("%.4f *** (beta_1)", coef(m4)["mujer"])
  ),
  `Enfoque 2 (Base Mujeres, dummy 'hombre')` = c(
    sprintf("%.4f ** (alpha_2 + alpha_3)", a2_m + a3_m),
    sprintf("%.4f *** [SE: %.4f] (alpha_2 directo)", a2_m, se_a2_m),
    sprintf("+%.4f *** (alpha_3)", a3_m),
    sprintf("+%.4f *** (alpha_1)", coef(m4_base_mujer)["hombre"])
  )
)

message("\n==============================================================================")
message("EQUIVALENCIA DUAL DE LOS DOS MODELOS (M4 Principal):")
message("------------------------------------------------------------------------------")
print(as.data.frame(tabla_dual), row.names = FALSE)
message("==============================================================================")

# Guardar resultados
write_csv(tabla_modelos, "tabla_regresiones_mco.csv")
write_csv(tabla_dual, "tabla_dual_equivalencia.csv")
save(m1, m2, m3, m4, m5, m4_base_mujer, file = "modelos_estimados.RData")
message("Modelos guardados en 'modelos_estimados.RData' y tablas exportadas a CSV.")
