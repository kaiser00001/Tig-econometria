# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 10: Rectas de Regresión Lineal para Modelo Principal (M4) y Modelo 6
# ==============================================================================

library(tidyverse)
library(haven)
library(sandwich)
library(lmtest)

# 1. Cargar datos
# ------------------------------------------------------------------------------
if (file.exists("df_tig_procesada.rds")) {
  df_tig <- readRDS("df_tig_procesada.rds")
} else {
  source("scripts/03_clasificacion_carreras.R")
}

# Asegurar variables de oficio y educación
df_tig <- df_tig |>
  mutate(
    oficio4_num = zap_labels(oficio4_08),
    trabaja_limpieza = if_else(floor(oficio4_num / 100) == 91, 1L, 0L),
    sin_estudios_sup = if_else(area_estudio == "Sin educación superior", 1L, 0L)
  )

# 2. Estimar Modelo 4 Principal y Modelo 6
# ------------------------------------------------------------------------------
m4 <- lm(ly ~ mujer * nhijos + edad + edad2 + esc + horas + rural + region + area_estudio,
         data = df_tig)

m6 <- lm(ly ~ mujer * nhijos + sin_estudios_sup + trabaja_limpieza + 
           edad + edad2 + horas + rural + region,
         data = df_tig)

cat("Modelo 4 - Coeficiente Horas:", coef(m4)["horas"], "\n")
cat("Modelo 6 - Coeficiente Horas:", coef(m6)["horas"], "\n")

# 3. Construir datos de predicción para las Rectas de Regresión Lineal
# ------------------------------------------------------------------------------
# Fijamos los controles continuos en sus promedios muestrales y las categóricas
# en su categoría modal para aislar el efecto puro de la regresión lineal.

edad_m  <- mean(df_tig$edad, na.rm = TRUE)
edad2_m <- mean(df_tig$edad2, na.rm = TRUE)
esc_m   <- mean(df_tig$esc, na.rm = TRUE)
horas_m <- mean(df_tig$horas, na.rm = TRUE)
reg_ref <- names(sort(table(df_tig$region), decreasing = TRUE))[1] # Metropolitana

# Grid de predicción para M4
grid_m4 <- expand_grid(
  mujer = c(0, 1),
  nhijos = seq(0, 5, length.out = 100),
  edad = edad_m,
  edad2 = edad2_m,
  esc = esc_m,
  horas = horas_m,
  rural = 0,
  region = reg_ref,
  area_estudio = "Sin educación superior"
)

pred_m4 <- predict(m4, newdata = grid_m4, interval = "confidence")
grid_m4 <- grid_m4 |>
  mutate(
    fit = pred_m4[, "fit"],
    lwr = pred_m4[, "lwr"],
    upr = pred_m4[, "upr"],
    modelo = "Modelo 4: Especificación Principal (Mincer + Horas)",
    genero = if_else(mujer == 1, "Mujeres (Pendiente: -0.0591***)", "Hombres (Pendiente: +0.0084**)")
  )

# Grid de predicción para M6
grid_m6 <- expand_grid(
  mujer = c(0, 1),
  nhijos = seq(0, 5, length.out = 100),
  edad = edad_m,
  edad2 = edad2_m,
  horas = horas_m,
  rural = 0,
  region = reg_ref,
  sin_estudios_sup = 0, # Evaluado en técnicos/profesionales
  trabaja_limpieza = 0  # Evaluado en no-limpieza
)

pred_m6 <- predict(m6, newdata = grid_m6, interval = "confidence")
grid_m6 <- grid_m6 |>
  mutate(
    fit = pred_m6[, "fit"],
    lwr = pred_m6[, "lwr"],
    upr = pred_m6[, "upr"],
    modelo = "Modelo 6: Control por Limpieza y Sin Sup.",
    genero = if_else(mujer == 1, "Mujeres (Pendiente: -0.0815***)", "Hombres (Pendiente: -0.0065)")
  )

# Combinar predicciones
grid_total <- bind_rows(grid_m4, grid_m6)

# 4. Tema y Gráficos de Regresión Lineal
# ------------------------------------------------------------------------------
colores_reg <- c(
  "Hombres (Pendiente: +0.0084**)" = "#1E3A8A",
  "Mujeres (Pendiente: -0.0591***)" = "#DC2626",
  "Hombres (Pendiente: -0.0065)" = "#1E3A8A",
  "Mujeres (Pendiente: -0.0815***)" = "#DC2626"
)

theme_reg <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "#0F172A"),
      plot.subtitle = element_text(size = 9.5, color = "#475569", margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#64748B", hjust = 1, margin = margin(t = 10)),
      axis.title = element_text(face = "bold", size = 9.5, color = "#334155"),
      axis.text = element_text(size = 9, color = "#334155"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E2E8F0", linewidth = 0.5),
      strip.background = element_rect(fill = "#F1F5F9", color = "#CBD5E1"),
      strip.text = element_text(face = "bold", size = 10, color = "#0F172A"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# Gráfico 1: Regresión Lineal Modelo 4 Principal
g_m4 <- ggplot(grid_m4, aes(x = nhijos, y = fit, color = genero, fill = genero)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = colores_reg) +
  scale_fill_manual(values = colores_reg) +
  labs(
    title = "Regresión Lineal MCO: Modelo 4 Principal (Efecto Marginal de Hijos)",
    subtitle = "Rectas de regresión estimadas ln(Y) vs. Hijos por sexo (CASEN 2024, IC 95%). Controles evaluados en la media.",
    x = "Número de hijos en el núcleo (nhijos)",
    y = "Logaritmo del Ingreso Laboral Predicho ln(Y)",
    caption = "Hombres: beta_2 = +0.0084 (p<0.05). Mujeres: beta_2 + beta_3 = -0.0591 (p<0.001). Beta Horas: +0.0143***."
  ) +
  theme_reg()

ggsave("grafico_regresion_m4_principal.png", plot = g_m4, width = 7.5, height = 4.5, dpi = 300)
message("Guardado: 'grafico_regresion_m4_principal.png'")

# Gráfico 2: Regresión Lineal Modelo 6 (Limpieza / Sin Educación Superior)
g_m6 <- ggplot(grid_m6, aes(x = nhijos, y = fit, color = genero, fill = genero)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = colores_reg) +
  scale_fill_manual(values = colores_reg) +
  labs(
    title = "Regresión Lineal MCO: Modelo 6 (Segmentación en Limpieza y Sin Sup.)",
    subtitle = "Rectas de regresión estimadas ln(Y) vs. Hijos por sexo (CASEN 2024, IC 95%). Controles evaluados en la media.",
    x = "Número de hijos en el núcleo (nhijos)",
    y = "Logaritmo del Ingreso Laboral Predicho ln(Y)",
    caption = "Hombres: beta_2 = -0.0065 (no sig). Mujeres: beta_2 + beta_3 = -0.0815 (p<0.001). Beta Horas: +0.0145***."
  ) +
  theme_reg()

ggsave("grafico_regresion_m6_vulnerabilidad.png", plot = g_m6, width = 7.5, height = 4.5, dpi = 300)
message("Guardado: 'grafico_regresion_m6_vulnerabilidad.png'")

# Gráfico 3: Comparativo de Rectas de Regresión (Panel M4 vs. Panel M6)
g_comp <- ggplot(grid_total, aes(x = nhijos, y = fit, color = genero, fill = genero)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.18, color = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ modelo, scales = "free_y") +
  scale_color_manual(values = colores_reg) +
  scale_fill_manual(values = colores_reg) +
  labs(
    title = "Comparación de Rectas de Regresión MCO: Modelo Principal vs. Modelo 6",
    subtitle = "Pendientes estimadas de la interacción mujer*nhijos. Divergencia sistemática por sexo (CASEN 2024, IC 95%).",
    x = "Número de hijos en el núcleo (nhijos)",
    y = "Logaritmo del Ingreso Laboral Predicho ln(Y)",
    caption = "Fuente: Estimación MCO con errores robustos HC1 sobre CASEN 2024 (N = 29.781)."
  ) +
  theme_reg()

ggsave("grafico_regresiones_comparativa.png", plot = g_comp, width = 9.8, height = 4.8, dpi = 300)
message("Guardado: 'grafico_regresiones_comparativa.png'")
