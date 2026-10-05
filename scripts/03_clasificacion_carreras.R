# ==============================================================================
# TIG Econometría (ICOM601) - Pregunta 2: Efecto de Hijos sobre Ingreso Laboral
# Script 03: Clasificación de Carreras Universitarias / Técnicas (Variable e7)
# ==============================================================================

library(tidyverse)
library(stringi)
library(haven)

# 1. Asegurar dependencias de datos
# ------------------------------------------------------------------------------
if (!exists("df_tig")) {
  message("Ejecutando scripts/02_filtros_missing.R previamente...")
  source("scripts/02_filtros_missing.R")
}

# 2. Función de normalización de texto libre
# ------------------------------------------------------------------------------
normalizar_texto <- function(x) {
  x <- str_to_lower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_squish(x)
  x
}

# 3. Diccionario de clasificación por expresiones regulares (Regex)
# ------------------------------------------------------------------------------
# Se emplean grupos no captantes (?:...) para evitar fugas del operador OR (|)
clasificar_carrera <- function(x_norm) {
  ING <- "ingenier\\w*"
  TEC <- "tecnic\\w*"
  PED <- "(?:pedagog\\w*|profesor\\w*|educador\\w*)"

  case_when(
    str_detect(x_norm, paste0(TEC, ".*enfermer")) ~ "Técnico en Enfermería",
    str_detect(x_norm, "^enfermer|enfermeria$|enfermeria y") & !str_detect(x_norm, TEC) ~ "Enfermería",

    str_detect(x_norm, paste0("ingeco|ingcom|^ing\\s*c$|ing\\.?\\s*com|", ING, ".*comercial")) ~ "Ingeniería Comercial",
    str_detect(x_norm, paste0(ING, ".*civil.*industrial")) ~ "Ingeniería Civil Industrial",
    str_detect(x_norm, paste0(ING, "\\s*industrial\\b")) & !str_detect(x_norm, "civil") ~ "Ingeniería Industrial",
    str_detect(x_norm, paste0(ING, ".*(en )?administracion( de empresas)?|", ING, ".*recursos humanos")) ~ "Ingeniería en Administración de Empresas",
    str_detect(x_norm, paste0(TEC, ".*administracion de empresas")) ~ "Técnico en Administración de Empresas",
    str_detect(x_norm, "^administracion de empresas$|administracion de empresas") & !str_detect(x_norm, paste0(ING, "|", TEC)) ~ "Administración de Empresas",
    str_detect(x_norm, "^administracion publica$|administracion publica") ~ "Administración Pública",

    str_detect(x_norm, "contador|contabilidad") ~ "Contabilidad / Auditoría",
    str_detect(x_norm, "^derecho$|derecho\\b") ~ "Derecho",
    str_detect(x_norm, paste0(TEC, ".*juridic")) ~ "Técnico Jurídico",
    str_detect(x_norm, "^psicologia$|psicologia\\b") ~ "Psicología",
    str_detect(x_norm, "psicopedagogia|psicopedagog") ~ "Psicopedagogía",
    str_detect(x_norm, "trabajo social|trabajador\\w*.*social|asistente social") ~ "Trabajo Social",
    str_detect(x_norm, "sociologia") ~ "Sociología",
    str_detect(x_norm, "periodismo") ~ "Periodismo",
    str_detect(x_norm, "publicidad") ~ "Publicidad",

    str_detect(x_norm, paste0(ING, ".*prevencion de riesgos")) ~ "Ingeniería en Prevención de Riesgos",
    str_detect(x_norm, paste0("^prevencion de riesgos$|", TEC, ".*prevencion de riesgos")) ~ "Técnico en Prevención de Riesgos",
    str_detect(x_norm, "kinesiologia") ~ "Kinesiología",
    str_detect(x_norm, "fonoaudiologia") ~ "Fonoaudiología",
    str_detect(x_norm, "terapia ocupacional") ~ "Terapia Ocupacional",
    str_detect(x_norm, "nutricion") ~ "Nutrición y Dietética",
    str_detect(x_norm, "tecnologia medica") ~ "Tecnología Médica",
    str_detect(x_norm, paste0(TEC, ".*odontologia")) ~ "Técnico en Odontología",
    str_detect(x_norm, "^odontologia$|odontologicas") ~ "Odontología",
    str_detect(x_norm, "^medicina$") ~ "Medicina",
    str_detect(x_norm, "medicina veterinaria|^veterinaria$") ~ "Medicina Veterinaria",
    str_detect(x_norm, "quimica y farmacia") ~ "Química y Farmacia",

    str_detect(x_norm, paste0(PED, ".*educacion fisica|", PED, ".*fisica$")) ~ "Pedagogía en Educación Física",
    str_detect(x_norm, paste0(PED, ".*educacion basica|", PED, ".*basica$")) ~ "Pedagogía Básica",
    str_detect(x_norm, paste0(PED, ".*parvul|", TEC, ".*parvul")) ~ "Educación/Técnico en Párvulos",
    str_detect(x_norm, paste0(PED, ".*diferencial")) ~ "Pedagogía Diferencial",
    str_detect(x_norm, paste0(PED, ".*historia")) ~ "Pedagogía en Historia",
    str_detect(x_norm, paste0(PED, ".*ingles")) ~ "Pedagogía en Inglés",
    str_detect(x_norm, paste0(PED, ".*matematicas")) ~ "Pedagogía en Matemáticas",
    str_detect(x_norm, paste0(PED, ".*lenguaje")) ~ "Pedagogía en Lenguaje",
    str_detect(x_norm, paste0(PED, ".*musica")) ~ "Pedagogía en Música",
    str_detect(x_norm, paste0("^", PED)) ~ "Otra Pedagogía",

    str_detect(x_norm, paste0(ING, ".*(en )?informatica|", ING, ".*computacion")) ~ "Ingeniería en Informática",
    str_detect(x_norm, paste0(TEC, ".*informatica")) ~ "Técnico en Informática",
    str_detect(x_norm, "^arquitectura$") ~ "Arquitectura",
    str_detect(x_norm, paste0(ING, ".*construccion|construccion civil")) ~ "Ingeniería en Construcción",
    str_detect(x_norm, paste0(TEC, ".*construccion")) ~ "Técnico en Construcción",
    str_detect(x_norm, paste0(ING, "\\s*civil\\b")) & !str_detect(x_norm, "industrial|comercial") ~ "Ingeniería Civil",
    str_detect(x_norm, paste0(ING, ".*(en )?electric")) ~ "Ingeniería Eléctrica",
    str_detect(x_norm, paste0(TEC, ".*electric")) ~ "Técnico en Electricidad",
    str_detect(x_norm, paste0(ING, ".*mecanic")) ~ "Ingeniería Mecánica",
    str_detect(x_norm, "mecanica automotriz") ~ "Técnico en Mecánica Automotriz",
    str_detect(x_norm, "^agronomia$") ~ "Agronomía",
    str_detect(x_norm, paste0(ING, ".*minas|^mineria$")) ~ "Ingeniería en Minas",
    str_detect(x_norm, "diseno grafico") ~ "Diseño Gráfico",
    str_detect(x_norm, "gastronomia") ~ "Gastronomía",

    TRUE ~ "Otra carrera universitaria/técnica"
  )
}

# 4. Agrupación en Áreas Amplias de Estudio
# ------------------------------------------------------------------------------
colapsar_area <- function(carrera) {
  case_when(
    carrera %in% c(
      "Ingeniería Comercial", "Ingeniería en Administración de Empresas",
      "Administración de Empresas", "Técnico en Administración de Empresas",
      "Contabilidad / Auditoría", "Administración Pública", "Publicidad"
    ) ~ "Administración y Negocios",

    carrera %in% c(
      "Ingeniería Civil Industrial", "Ingeniería Industrial", "Ingeniería en Informática",
      "Técnico en Informática", "Ingeniería en Construcción", "Técnico en Construcción",
      "Ingeniería Civil", "Ingeniería Eléctrica", "Técnico en Electricidad",
      "Ingeniería Mecánica", "Técnico en Mecánica Automotriz", "Arquitectura",
      "Ingeniería en Minas", "Diseño Gráfico", "Ingeniería en Prevención de Riesgos",
      "Técnico en Prevención de Riesgos"
    ) ~ "Ingeniería y Tecnología",

    carrera %in% c(
      "Técnico en Enfermería", "Enfermería", "Kinesiología", "Fonoaudiología",
      "Terapia Ocupacional", "Nutrición y Dietética", "Tecnología Médica",
      "Técnico en Odontología", "Odontología", "Medicina", "Medicina Veterinaria",
      "Química y Farmacia"
    ) ~ "Salud",

    carrera %in% c(
      "Educación/Técnico en Párvulos", "Pedagogía Básica", "Pedagogía en Educación Física",
      "Otra Pedagogía", "Pedagogía Diferencial", "Psicopedagogía", "Pedagogía en Inglés",
      "Pedagogía en Historia", "Pedagogía en Matemáticas", "Pedagogía en Lenguaje",
      "Pedagogía en Música"
    ) ~ "Educación",

    carrera %in% c(
      "Trabajo Social", "Psicología", "Sociología", "Periodismo", "Técnico Jurídico"
    ) ~ "Ciencias Sociales y Humanidades",

    carrera == "Derecho" ~ "Derecho",
    carrera == "Agronomía" ~ "Agropecuario",
    carrera == "Gastronomía" ~ "Servicios",
    carrera == "Otra carrera universitaria/técnica" ~ "Otra educación superior",

    TRUE ~ "Sin educación superior"
  )
}

# 5. Aplicación a la Muestra Analítica
# ------------------------------------------------------------------------------
# Corrección metodológica fundamental:
# Quienes NO tienen educación superior (e6a < 12) se codifican explícitamente como
# "Sin educación superior", evitando que el modelo econométrico descarte al ~50%
# de la muestra por presencia de NA.
df_tig <- df_tig |>
  mutate(
    e6a_num   = zap_labels(e6a),
    tiene_sup = e6a_num %in% c(12, 13, 14, 15),
    e7_texto  = if_else(tiene_sup & !is.na(e7), normalizar_texto(e7), ""),
    carrera_grupo = if_else(tiene_sup & e7_texto != "", clasificar_carrera(e7_texto), "Sin educación superior"),
    area_estudio  = colapsar_area(carrera_grupo),
    area_estudio  = factor(area_estudio, levels = c(
      "Sin educación superior",
      "Administración y Negocios",
      "Ingeniería y Tecnología",
      "Salud",
      "Educación",
      "Ciencias Sociales y Humanidades",
      "Derecho",
      "Agropecuario",
      "Servicios",
      "Otra educación superior"
    ))
  )

# Prueba de humo de normalización y clasificación
stopifnot(
  clasificar_carrera(normalizar_texto(c("ingeco", "ING.C", "ing.c", "ingenieria comercial", "Ing. Comercial"))) |>
    unique() |> length() == 1
)

# 6. Reporte de Distribución y Exportación de la Base Final Procesada
# ------------------------------------------------------------------------------
message("Distribución de la variable agregada de área de estudio:")
print(table(df_tig$area_estudio))

# Guardar base limpia consolidada para acelerar los análisis econométricos
saveRDS(df_tig, file = "df_tig_procesada.rds")
message("Base consolidada guardada exitosamente en 'df_tig_procesada.rds'")
