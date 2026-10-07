# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 07: Análisis de Mecanismo - Interrupción Laboral por Crianza (Mediación)
#
# Hipótesis de mediación: el child penalty femenino opera en parte a través de
# los años de inactividad laboral acumulados durante la crianza temprana.
# Se construyen proxies de interrupción a partir de las edades de los hijos
# identificados en el núcleo y se evalúa cuánto del beta negativo se explica
# por este canal.
# ==============================================================================

library(tidyverse)
library(haven)
library(sandwich)
library(lmtest)
library(car)

# 1. Cargar datos base (pre-procesados)
# ------------------------------------------------------------------------------
if (file.exists("df_tig_procesada.rds")) {
  df_tig <- readRDS("df_tig_procesada.rds")
} else {
  source("scripts/03_clasificacion_carreras.R")
}

if (!"hombre" %in% names(df_tig)) df_tig$hombre <- if_else(df_tig$mujer == 0, 1L, 0L)

# Recargar CASEN completa para extraer edades de hijos
if (!exists("casen_2024")) load("casen_2024.RData")

# 2. Construcción de Proxies de Interrupción Laboral por Crianza
# ------------------------------------------------------------------------------
# Lógica: para cada núcleo (folio + nucleo), identificamos a los hijos (pco2 %in% c(4,5,6))
# y calculamos indicadores de carga de cuidado basados en sus edades.
#
# En Chile, el postnatal vigente otorga ~6 meses (24 semanas) de licencia.
# Empíricamente, la literatura muestra que la penalización se concentra en los
# primeros 6 años de vida del hijo (período preescolar de cuidado intensivo).
# Construimos:
#   - edad_hijo_menor: edad del hijo más pequeño en el hogar (recencia de la
#     interrupción más reciente).
#   - anios_cuidado_intensivo: suma de max(0, 6 - edad_hijo) para cada hijo,
#     proxy de años acumulados en etapa preescolar de alta demanda de cuidado.
#   - exp_potencial: experiencia potencial de Mincer (edad - esc - 6).
#   - exp_interrumpida: exp_potencial - anios_cuidado_intensivo (proxy de
#     experiencia efectiva neta de interrupciones por crianza).

# Los hijos se identifican por NÚCLEO (pco2 %in% c(4,5,6)), igual que nhijos
# en scripts/01: así cada persona recibe los proxies de SUS hijos y no los de
# todo el hogar (p.ej. una hija con hijos que vive con sus padres).
codigos_hijo_nuc <- c(4, 5, 6)

proxies_nucleo <- casen_2024 |>
  mutate(
    pco2_num   = zap_labels(pco2),
    nucleo_num = as.numeric(zap_labels(nucleo)),
    edad_num   = as.numeric(zap_labels(edad))
  ) |>
  filter(pco2_num %in% codigos_hijo_nuc) |>
  group_by(folio, nucleo_num) |>
  summarise(
    edad_hijo_menor = min(edad_num, na.rm = TRUE),
    # Años de cuidado intensivo acumulados (preescolar: 0 a 5 años)
    anios_cuidado_intensivo = sum(pmax(0, 6 - edad_num), na.rm = TRUE),
    # Número de hijos en etapa preescolar (0-5 años)
    nhijos_preescolar = sum(edad_num < 6, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Unir proxies a df_tig (por folio + núcleo)
# ------------------------------------------------------------------------------
df_tig <- df_tig |>
  select(-any_of(c("edad_hijo_menor", "anios_cuidado_intensivo",
                    "nhijos_preescolar", "exp_potencial", "exp_interrumpida"))) |>
  mutate(nucleo_num = as.numeric(zap_labels(nucleo))) |>
  left_join(proxies_nucleo, by = c("folio", "nucleo_num")) |>
  mutate(
    # Sin hijos en el núcleo, o persona que no es jefe/pareja de núcleo:
    # valores por defecto
    edad_hijo_menor         = if_else(jp_nucleo, replace_na(edad_hijo_menor, 99), 99),
    anios_cuidado_intensivo = if_else(jp_nucleo, replace_na(anios_cuidado_intensivo, 0), 0),
    nhijos_preescolar       = if_else(jp_nucleo, replace_na(nhijos_preescolar, 0L), 0L),

    # Experiencia potencial de Mincer: años desde que terminó de estudiar
    exp_potencial = pmax(0, edad - esc - 6),

    # Experiencia efectiva proxy: potencial menos años de cuidado intensivo
    exp_interrumpida = pmax(0, exp_potencial - anios_cuidado_intensivo),

    # Dummy: tiene al menos un hijo en edad preescolar (0-5 años)
    hijo_preescolar = if_else(nhijos_preescolar > 0, 1L, 0L)
  )

# 4. Análisis de Mediación Secuencial
# ------------------------------------------------------------------------------
# Modelo A: Modelo Principal (referencia, sin proxy de interrupción)
mA <- lm(ly ~ mujer * nhijos + edad + edad2 + esc + horas + rural + region + area_estudio,
         data = df_tig)

# Modelo B: Incorporando años de cuidado intensivo interactuados con mujer
# Si anios_cuidado_intensivo absorbe parte de mujer:nhijos, confirma el mecanismo
mB <- lm(ly ~ mujer * nhijos + mujer * anios_cuidado_intensivo + 
           edad + edad2 + esc + horas + rural + region + area_estudio,
         data = df_tig)

# Modelo C: Reemplazando experiencia potencial por experiencia interrumpida
# Si el retorno a la experiencia es significativo y nhijos se atenúa, la
# interrupción de carrera es el canal dominante
mC <- lm(ly ~ mujer * nhijos + exp_interrumpida + I(exp_interrumpida^2) + 
           esc + horas + rural + region + area_estudio,
         data = df_tig)

# Modelo D: Dummy de hijo preescolar (efecto de recencia de la interrupción)
mD <- lm(ly ~ mujer * nhijos + mujer * hijo_preescolar + 
           edad + edad2 + esc + horas + rural + region + area_estudio,
         data = df_tig)

# Inferencia robusta HC1
ctA <- coeftest(mA, vcov. = vcovHC(mA, type = "HC1"))
ctB <- coeftest(mB, vcov. = vcovHC(mB, type = "HC1"))
ctC <- coeftest(mC, vcov. = vcovHC(mC, type = "HC1"))
ctD <- coeftest(mD, vcov. = vcovHC(mD, type = "HC1"))

# 5. Tabla Comparativa de Mediación
# ------------------------------------------------------------------------------
vars_mediacion <- c("mujer", "nhijos", "mujer:nhijos", 
                    "anios_cuidado_intensivo", "mujer:anios_cuidado_intensivo",
                    "exp_interrumpida", "hijo_preescolar", "mujer:hijo_preescolar",
                    "esc", "horas")

extraer <- function(ct, m, nombre) {
  coef_df <- as.data.frame(ct[, 1:4])
  colnames(coef_df) <- c("Est", "SE", "t", "p")
  coef_df$Variable <- rownames(coef_df)
  coef_df |>
    filter(Variable %in% vars_mediacion) |>
    mutate(
      Sig = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ ""),
      Salida = sprintf("%.4f %s (%.4f)", Est, Sig, SE),
      Modelo = nombre,
      R2 = round(summary(m)$adj.r.squared, 4)
    ) |>
    select(Modelo, Variable, Salida, R2)
}

tabla_mediacion <- bind_rows(
  extraer(ctA, mA, "A: Principal (sin mediador)"),
  extraer(ctB, mB, "B: + Años Cuidado Intensivo"),
  extraer(ctC, mC, "C: Experiencia Interrumpida"),
  extraer(ctD, mD, "D: + Hijo Preescolar")
) |>
  pivot_wider(names_from = Modelo, values_from = Salida)

message("\n==============================================================================")
message("ANÁLISIS DE MEDIACIÓN: ¿EL CHILD PENALTY OPERA VÍA INTERRUPCIÓN LABORAL?")
message("==============================================================================")
print(as.data.frame(tabla_mediacion), row.names = FALSE)

# 6. Cálculo Formal de Atenuación del Child Penalty
# ------------------------------------------------------------------------------
b3_A <- coef(mA)["mujer:nhijos"]
b3_B <- coef(mB)["mujer:nhijos"]
b3_C <- coef(mC)["mujer:nhijos"]
b3_D <- coef(mD)["mujer:nhijos"]

atenuacion_B <- (1 - abs(b3_B) / abs(b3_A)) * 100
atenuacion_C <- (1 - abs(b3_C) / abs(b3_A)) * 100
atenuacion_D <- (1 - abs(b3_D) / abs(b3_A)) * 100

message("\n==============================================================================")
message("PORCENTAJE DE ATENUACIÓN DEL DIFERENCIAL mujer:nhijos (beta_3)")
message("------------------------------------------------------------------------------")
cat(sprintf("Modelo A (referencia): beta_3 = %.4f\n", b3_A))
cat(sprintf("Modelo B (+ años cuidado):  beta_3 = %.4f -> Atenuación: %.1f%%\n", b3_B, atenuacion_B))
cat(sprintf("Modelo C (exp. interrumpida): beta_3 = %.4f -> Atenuación: %.1f%%\n", b3_C, atenuacion_C))
cat(sprintf("Modelo D (+ hijo preescolar): beta_3 = %.4f -> Atenuación: %.1f%%\n", b3_D, atenuacion_D))
message("==============================================================================")
message("\nSi la atenuación es sustancial (>15-20%), la interrupción laboral por crianza")
message("es un canal de transmisión estadísticamente relevante del child penalty.")

# Guardar
save(mA, mB, mC, mD, file = "modelos_mediacion.RData")
write_csv(tabla_mediacion, "tabla_mediacion_interrupcion.csv")
saveRDS(df_tig, file = "df_tig_procesada.rds")
message("Modelos de mediación y base actualizada guardados exitosamente.")
