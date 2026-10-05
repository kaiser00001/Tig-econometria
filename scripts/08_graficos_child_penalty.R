# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 08: Generación de Gráficos de Child Penalty (Poblacional y por Género)
# ==============================================================================

library(tidyverse)
library(scales)

# 1. Cargar datos procesados
# ------------------------------------------------------------------------------
if (file.exists("df_tig_procesada.rds")) {
  df_tig <- readRDS("df_tig_procesada.rds")
} else {
  source("scripts/03_clasificacion_carreras.R")
}

# Crear tramos de hijos para visualización discreta
df_graf <- df_tig |>
  mutate(
    hijos_cat = case_when(
      nhijos == 0 ~ "0 hijos",
      nhijos == 1 ~ "1 hijo",
      nhijos == 2 ~ "2 hijos",
      nhijos >= 3 ~ "3 o más"
    ),
    hijos_cat = factor(hijos_cat, levels = c("0 hijos", "1 hijo", "2 hijos", "3 o más")),
    genero_label = if_else(mujer == 1, "Mujeres (Dummy = 1)", "Hombres (Dummy = 0)")
  )

# Tema visual académico estilizado
theme_academic <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = 12.5, color = "#0F172A", hjust = 0),
      plot.subtitle = element_text(size = 9.5, color = "#475569", hjust = 0, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = "#64748B", hjust = 1, margin = margin(t = 10)),
      axis.title = element_text(face = "bold", size = 9.5, color = "#334155"),
      axis.text = element_text(size = 9, color = "#334155"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.5),
      strip.background = element_rect(fill = "#F1F5F9", color = "#CBD5E1"),
      strip.text = element_text(face = "bold", size = 10, color = "#0F172A"),
      legend.position = "none"
    )
}

formato_pesos <- function(x) {
  paste0("$", format(round(x), big.mark = ".", decimal.mark = ","))
}

# ------------------------------------------------------------------------------
# GRÁFICO 1: Child Penalty en el Promedio Poblacional (Sin Dummy de Mujer)
# ------------------------------------------------------------------------------
resumen_poblacional <- df_graf |>
  group_by(hijos_cat) |>
  summarise(
    n = n(),
    ingreso_medio = mean(yoprcor, na.rm = TRUE),
    se = sd(yoprcor, na.rm = TRUE) / sqrt(n),
    ic_inf = ingreso_medio - 1.96 * se,
    ic_sup = ingreso_medio + 1.96 * se,
    .groups = "drop"
  )

g1 <- ggplot(resumen_poblacional, aes(x = hijos_cat, y = ingreso_medio, group = 1)) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), fill = "#94A3B8", alpha = 0.25) +
  geom_line(color = "#1E293B", linewidth = 1.1) +
  geom_point(color = "#8B0000", size = 3.5) +
  geom_text(aes(label = formato_pesos(ingreso_medio)),
            vjust = -1.3, size = 3.3, fontface = "bold", color = "#0F172A") +
  scale_y_continuous(
    labels = formato_pesos,
    limits = c(650000, 1150000),
    breaks = seq(700000, 1100000, by = 100000)
  ) +
  labs(
    title = "Efecto del Número de Hijos sobre el Ingreso Laboral Promedio Poblacional",
    subtitle = "Muestra total de ocupados 25-45 años sin desagregar por sexo (CASEN 2024, N = 29.781, IC 95%).",
    x = "Número de hijos en el hogar",
    y = "Ingreso medio mensual (CLP)",
    caption = "Fuente: Elaboración propia en base a microdatos CASEN 2024."
  ) +
  theme_academic()

ggsave("grafico1_child_penalty_poblacional.png", plot = g1, width = 7.5, height = 4.2, dpi = 300)
message("Gráfico 1 generado exitosamente: 'grafico1_child_penalty_poblacional.png'")

# ------------------------------------------------------------------------------
# GRÁFICO 2: Child Penalty Desagregado por Sexo (Dummy = 0 vs Dummy = 1)
# ------------------------------------------------------------------------------
resumen_genero <- df_graf |>
  group_by(genero_label, mujer, hijos_cat) |>
  summarise(
    n = n(),
    ingreso_medio = mean(yoprcor, na.rm = TRUE),
    se = sd(yoprcor, na.rm = TRUE) / sqrt(n),
    ic_inf = ingreso_medio - 1.96 * se,
    ic_sup = ingreso_medio + 1.96 * se,
    .groups = "drop"
  )

colores_genero <- c("Hombres (Dummy = 0)" = "#1E3A8A", "Mujeres (Dummy = 1)" = "#991B1B")

g2 <- ggplot(resumen_genero, aes(x = hijos_cat, y = ingreso_medio, group = genero_label, color = genero_label)) +
  geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup, fill = genero_label), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.2) +
  geom_text(aes(label = formato_pesos(ingreso_medio)),
            vjust = -1.3, size = 3.2, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ genero_label) +
  scale_color_manual(values = colores_genero) +
  scale_fill_manual(values = colores_genero) +
  scale_y_continuous(
    labels = formato_pesos,
    limits = c(500000, 1180000),
    breaks = seq(500000, 1100000, by = 150000)
  ) +
  labs(
    title = "Asimetría de Género en la Penalización por Hijos: Hombres vs. Mujeres",
    subtitle = "Comparación del ingreso medio por hijos: Hombres (Dummy=0) vs. Mujeres (Dummy=1). CASEN 2024 (IC 95%).",
    x = "Número de hijos en el hogar",
    y = "Ingreso medio mensual (CLP)",
    caption = "Fuente: Elaboración propia en base a microdatos CASEN 2024."
  ) +
  theme_academic()

ggsave("grafico2_child_penalty_genero.png", plot = g2, width = 9.2, height = 4.5, dpi = 300)
message("Gráfico 2 generado exitosamente: 'grafico2_child_penalty_genero.png'")
