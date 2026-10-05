# ------------------------------------------------------------------
# TIG Econometria - Pregunta 2
# Construccion de la variable nhijos desde el modulo de composicion
# del hogar de CASEN 2024 (Semana 1 del cronograma)
# ------------------------------------------------------------------
library(tidyverse)

load("casen_2024.RData")

# Verificacion basica de la carga
dim(casen_2024)
str(casen_2024[, c("yoprcor", "sexo", "edad", "esc", "region", "area")])
summary(casen_2024$yoprcor)

# pco1 llega como haven_labelled; as.numeric() directo falla por el
# metodo S3 de esa clase, por eso se desetiqueta con haven::zap_labels()
# antes de comparar codigos.
# Codigos de pco1 (relacion con la jefatura de hogar) que cuentan como
# "hijo/a": 4 = Hijo(a) de ambos, 5 = Hijo(a) solo de la jefatura,
# 6 = Hijo(a) solo del esposo(a)/pareja.
codigos_hijo <- c(4, 5, 6)

nhijos_hogar <- casen_2024 |>
  mutate(pco1_num = haven::zap_labels(pco1)) |>load("casen_2024.RData")
  mutate(es_hijo = pco1_num %in% codigos_hijo) |>
  group_by(folio) |>
  summarise(nhijos = sum(es_hijo, na.rm = TRUE), .groups = "drop")

casen_2024 <- casen_2024 |>
  select(-any_of("nhijos")) |>
  left_join(nhijos_hogar, by = "folio")

# Chequeo de la variable construida
summary(casen_2024$nhijos)
table(casen_2024$nhijos)

# Tamano de la submuestra objetivo: ocupados de 25-45 anios con
# ingreso valido (verificacion del minimo de 300 obs exigido)
casen_2024 |>
  mutate(activ_num = haven::zap_labels(activ)) |>
  filter(edad >= 25, edad <= 45, activ_num == 1, !is.na(yoprcor), yoprcor > 0) |>
  nrow()

# ------------------------------------------------------------------
# Registro de correcciones (para la declaracion de uso de IA)
# ------------------------------------------------------------------
# 1. haven_labelled: as.numeric(pco1) fallaba con
#    "Can't convert `x` <haven_labelled> to <double>" por el metodo S3
#    de esa clase. Se corrigio usando haven::zap_labels(pco1) antes de
#    comparar codigos (ver linea 24). El mismo patron se repite para
#    "activ" en el filtro de arriba y en scripts/02_filtros_missing.R.
#
# 2. scripts/03_clasificacion_carreras.R: el diccionario de carreras
#    tuvo un bug de agrupacion de regex. Al combinar con paste0() una
#    raiz que ya contenia alternancias internas
#    ("pedagog\\w*|profesor\\w*|educador\\w*") sin encerrarla en un
#    grupo no captante "(?:...)", el operador "|" se "fugaba" fuera de
#    la raiz y hacia que CUALQUIER texto que empezara con "pedagog"
#    cayera en la categoria "Pedagogía en Educación Física". Se
#    corrigio agrupando la raiz como "(?:pedagog\\w*|profesor\\w*|educador\\w*)".