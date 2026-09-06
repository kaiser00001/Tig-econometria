# ------------------------------------------------------------------
# TIG Econometria - Pregunta 2
# Limpieza y agrupacion de la variable de carrera/programa de estudios
# (texto libre, variable e7) en categorias equivalentes, para usar
# como control adicional ("area de estudio") en el modelo de ingreso.
#
# Requiere casen_2024 cargado (con nhijos ya construido opcionalmente,
# ver scripts/01_construccion_nhijos.R).
# ------------------------------------------------------------------
library(tidyverse)
library(stringi)

# ------------------------------------------------------------------
# 1. Acotar a la muestra objetivo con estudios de educacion superior
#    e6a: 12 = Tecnico Nivel Superior, 13 = Profesional,
#         14 = Magister, 15 = Doctorado
# ------------------------------------------------------------------
edu_superior <- casen_2024 |>
  mutate(
    activ_num = haven::zap_labels(activ),
    e6a_num = haven::zap_labels(e6a)
  ) |>
  filter(edad >= 25, edad <= 45, activ_num == 1, e6a_num %in% c(12, 13, 14, 15)) |>
  filter(!is.na(e7), e7 != "")

nrow(edu_superior)

# ------------------------------------------------------------------
# 2. Normalizar el texto libre: minusculas, sin tildes, sin puntuacion,
#    espacios simples. Esto es lo que permite que "ingeco", "ING.C",
#    "ing.c" e "Ingeniería Comercial" terminen comparandose de forma
#    consistente.
# ------------------------------------------------------------------
normalizar <- function(x) {
  x <- str_to_lower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[[:punct:]]", " ")
  x <- str_squish(x)
  x
}

edu_superior <- edu_superior |>
  mutate(e7_norm = normalizar(e7))

# ------------------------------------------------------------------
# 3. Diccionario de equivalencias por regex, en orden de prioridad
#    (patrones mas especificos primero para evitar que una categoria
#    general "atrape" casos que deberian ir a una mas especifica).
#
#    Se usan "raices" de palabra (ingenier\\w*, tecnic\\w*, etc.) para
#    que una misma carrera escrita como "ingenieria X", "ingeniero(a)
#    X" o abreviada ("ingeco", "ing.c", "ING COM") caiga en la misma
#    categoria. Cobertura actual: ~35 categorias especificas + "Otra
#    carrera" para el resto (long tail de ~4.700 respuestas distintas,
#    65.5% de cobertura en la submuestra de educacion superior).
#
#    Nota tecnica: al combinar con paste0() una raiz que ya contiene
#    alternancias "|" (como PED, que agrupa pedagog\\w*/profesor\\w*/
#    educador\\w*), esa raiz debe ir entre parentesis no captantes
#    "(?:...)" para que el "|" no se "fugue" fuera de la raiz y rompa
#    la prioridad del case_when (bug detectado y corregido en esta
#    version: la primera version sin parentesis clasificaba CUALQUIER
#    "pedagogia..." como "Pedagogía en Educación Física").
# ------------------------------------------------------------------
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

    TRUE ~ "Otra carrera / no clasificada específicamente"
  )
}

edu_superior <- edu_superior |>
  mutate(carrera_grupo = clasificar_carrera(e7_norm))

# Prueba de humo: variantes de escritura de una misma carrera deben
# caer en la misma categoria
stopifnot(
  clasificar_carrera(
    normalizar(c("ingeco", "ING.C", "ing.c", "ingenieria comercial", "Ing. Comercial", "ingeniero(a) comercial"))
  ) |>
    unique() |>
    length() == 1
)

# ------------------------------------------------------------------
# 4. Cobertura del diccionario (35 categorias especificas)
# ------------------------------------------------------------------
tabla_cobertura <- edu_superior |>
  count(carrera_grupo, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

print(tabla_cobertura, n = 55)

# ------------------------------------------------------------------
# 5. Variable colapsada (area_amplia) para usar como control en el
#    modelo de la Pregunta 2, con pocas categorias y mas obs. por
#    celda. carrera_grupo se conserva intacto (~35 niveles) para
#    analisis mas finos si se necesita profundizar en una carrera
#    especifica.
# ------------------------------------------------------------------
colapsar_area <- function(carrera_grupo) {
  case_when(
    carrera_grupo %in% c(
      "Ingeniería Comercial", "Ingeniería en Administración de Empresas",
      "Administración de Empresas", "Técnico en Administración de Empresas",
      "Contabilidad / Auditoría", "Administración Pública", "Publicidad"
    ) ~ "Administración y Negocios",

    carrera_grupo %in% c(
      "Ingeniería Civil Industrial", "Ingeniería Industrial", "Ingeniería en Informática",
      "Técnico en Informática", "Ingeniería en Construcción", "Técnico en Construcción",
      "Ingeniería Civil", "Ingeniería Eléctrica", "Técnico en Electricidad",
      "Ingeniería Mecánica", "Técnico en Mecánica Automotriz", "Arquitectura",
      "Ingeniería en Minas", "Diseño Gráfico", "Ingeniería en Prevención de Riesgos",
      "Técnico en Prevención de Riesgos"
    ) ~ "Ingeniería, Construcción y Tecnología",

    carrera_grupo %in% c(
      "Técnico en Enfermería", "Enfermería", "Kinesiología", "Fonoaudiología",
      "Terapia Ocupacional", "Nutrición y Dietética", "Tecnología Médica",
      "Técnico en Odontología", "Odontología", "Medicina", "Medicina Veterinaria",
      "Química y Farmacia"
    ) ~ "Salud",

    carrera_grupo %in% c(
      "Educación/Técnico en Párvulos", "Pedagogía Básica", "Pedagogía en Educación Física",
      "Otra Pedagogía", "Pedagogía Diferencial", "Psicopedagogía", "Pedagogía en Inglés",
      "Pedagogía en Historia", "Pedagogía en Matemáticas", "Pedagogía en Lenguaje",
      "Pedagogía en Música"
    ) ~ "Educación",

    carrera_grupo %in% c(
      "Trabajo Social", "Psicología", "Sociología", "Periodismo", "Técnico Jurídico"
    ) ~ "Ciencias Sociales y Comunicación",

    carrera_grupo == "Derecho" ~ "Derecho",
    carrera_grupo == "Gastronomía" ~ "Servicios",
    carrera_grupo == "Agronomía" ~ "Agropecuario",

    is.na(carrera_grupo) ~ NA_character_,
    TRUE ~ "Otra / no clasificada"
  )
}

edu_superior <- edu_superior |>
  mutate(area_amplia = colapsar_area(carrera_grupo))

edu_superior |>
  count(area_amplia, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1))

# ------------------------------------------------------------------
# 6. Union de vuelta a casen_2024 (por folio/id_persona)
# ------------------------------------------------------------------
casen_2024 <- casen_2024 |>
  select(-any_of(c("carrera_grupo", "area_amplia"))) |>
  left_join(
    edu_superior |> select(folio, id_persona, carrera_grupo, area_amplia),
    by = c("folio", "id_persona")
  )
