# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 04: Estadística Descriptiva y Análisis Preliminar de Brechas (Semana 3)
# ==============================================================================

library(tidyverse)

# 1. Cargar Base Consolidada
# ------------------------------------------------------------------------------
if (file.exists("df_tig_procesada.rds")) {
  df_tig <- readRDS("df_tig_procesada.rds")
} else {
  message("Base procesada no encontrada. Ejecutando scripts de preparación...")
  source("scripts/03_clasificacion_carreras.R")
}

message("Muestra analítica cargada con éxito: N = ", nrow(df_tig), " observaciones.")

# 2. TABLA 1: Estadísticas Descriptivas por Sexo y Test de Diferencia de Medias
# ------------------------------------------------------------------------------
# Variables cuantitativas analizadas:
vars_cuant <- c("yoprcor", "ly", "nhijos", "nhijos_menor", "edad", "esc", "horas")

etiquetas <- c(
  yoprcor      = "Ingreso Ocupación Principal ($ CLP)",
  ly           = "Logaritmo del Ingreso (ly)",
  nhijos       = "Número de hijos en el núcleo (nhijos)",
  nhijos_menor = "Número de hijos < 18 años",
  edad         = "Edad (años)",
  esc          = "Años de escolaridad (esc)",
  horas        = "Horas trabajadas semanales (horas)"
)

tabla1_lista <- lapply(vars_cuant, function(v) {
  hombres <- df_tig[[v]][df_tig$mujer == 0]
  mujeres <- df_tig[[v]][df_tig$mujer == 1]
  
  m_h <- mean(hombres, na.rm = TRUE)
  sd_h <- sd(hombres, na.rm = TRUE)
  m_m <- mean(mujeres, na.rm = TRUE)
  sd_m <- sd(mujeres, na.rm = TRUE)
  
  # Test t de Welch para muestras independientes
  tt <- t.test(mujeres, hombres)
  dif <- m_m - m_h
  pval <- tt$p.value
  
  tibble(
    Variable = etiquetas[v],
    `Hombres Media` = m_h,
    `Hombres DE`    = sd_h,
    `Mujeres Media` = m_m,
    `Mujeres DE`    = sd_m,
    `Diferencia (M - H)` = dif,
    `p-valor`       = pval
  )
})

tabla1_descriptiva <- bind_rows(tabla1_lista)

message("\n==============================================================================")
message("TABLA 1: ESTADÍSTICAS DESCRIPTIVAS Y DIFERENCIA DE MEDIAS POR SEXO")
message("==============================================================================")
print(as.data.frame(tabla1_descriptiva |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# 3. TABLA 2: Distribución de Número de Hijos según Sexo
# ------------------------------------------------------------------------------
tabla2_dist_hijos <- df_tig |>
  mutate(
    tramo_hijos = case_when(
      nhijos == 0 ~ "0 hijos",
      nhijos == 1 ~ "1 hijo",
      nhijos == 2 ~ "2 hijos",
      nhijos >= 3 ~ "3 o más hijos"
    ),
    tramo_hijos = factor(tramo_hijos, levels = c("0 hijos", "1 hijo", "2 hijos", "3 o más hijos")),
    genero = if_else(mujer == 1, "Mujeres", "Hombres")
  ) |>
  count(genero, tramo_hijos) |>
  group_by(genero) |>
  mutate(
    Total_Grupo = sum(n),
    Porcentaje = round(100 * n / Total_Grupo, 2)
  ) |>
  ungroup()

message("\n==============================================================================")
message("TABLA 2: DISTRIBUCIÓN DE HIJOS POR SEXO EN LA MUESTRA")
message("==============================================================================")
print(as.data.frame(tabla2_dist_hijos), row.names = FALSE)

# 4. TABLA 3: Evidencia Preliminar del Child Penalty (Ingreso y Brecha por Hijos)
# ------------------------------------------------------------------------------
# Inspección no paramétrica previa a la regresión:
# Evalúa si la brecha salarial relativa entre hombres y mujeres se acentúa
# a medida que aumenta la carga parental.
tabla3_brecha_previa <- df_tig |>
  mutate(
    tramo_hijos = case_when(
      nhijos == 0 ~ "0 hijos",
      nhijos == 1 ~ "1 hijo",
      nhijos == 2 ~ "2 hijos",
      nhijos >= 3 ~ "3 o más hijos"
    ),
    tramo_hijos = factor(tramo_hijos, levels = c("0 hijos", "1 hijo", "2 hijos", "3 o más hijos"))
  ) |>
  group_by(tramo_hijos, mujer) |>
  summarise(
    ingreso_medio = mean(yoprcor, na.rm = TRUE),
    ingreso_mediana = median(yoprcor, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = mujer,
    values_from = c(ingreso_medio, ingreso_mediana, n)
  ) |>
  rename(
    Media_Hombres = ingreso_medio_0,
    Media_Mujeres = ingreso_medio_1,
    Mediana_Hombres = ingreso_mediana_0,
    Mediana_Mujeres = ingreso_mediana_1,
    N_Hombres = n_0,
    N_Mujeres = n_1
  ) |>
  mutate(
    # Brecha relativa bruta: (Y_mujeres - Y_hombres) / Y_hombres
    Brecha_Media_Pct = round(100 * (Media_Mujeres - Media_Hombres) / Media_Hombres, 2),
    Brecha_Mediana_Pct = round(100 * (Mediana_Mujeres - Mediana_Hombres) / Mediana_Hombres, 2)
  )

message("\n==============================================================================")
message("TABLA 3: INGRESO MEDIO, MEDIANO Y BRECHA SALARIAL SEGÚN NÚMERO DE HIJOS")
message("==============================================================================")
print(as.data.frame(tabla3_brecha_previa), row.names = FALSE)

# Exportación de tablas para el informe escrito del TIG
write_csv(tabla1_descriptiva, "tabla1_descriptiva.csv")
write_csv(tabla2_dist_hijos, "tabla2_distribucion_hijos.csv")
write_csv(tabla3_brecha_previa, "tabla3_brechas_hijos.csv")
message("\nTablas exportadas en formato CSV (tabla1, tabla2, tabla3) para el informe.")
