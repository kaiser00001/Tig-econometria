# ------------------------------------------------------------------
# TIG Econometria - Pregunta 2
# Construccion de variables de hijos en CASEN 2024, por NUCLEO (pco2).
# Traduccion a R de la logica Stata (CASEN 2022):
#   A. Por jefatura/conyuge del hogar (pco1), distinguiendo hijastros
#   B. Por nucleo (pco2 + nucleo): aplica a todas las personas
#
# Variables que usan los demas scripts:
#   nhijos         hijos del nucleo, cualquier edad
#   nhijos_menor   hijos del nucleo menores de 18
#   jp_nucleo      jefe(a) o pareja de un nucleo (pco2 = 1, 2, 3)
#   es_jefe_pareja jefe/pareja del hogar (pco1) o jefe/pareja de nucleo
#   nucleo_num     identificador numerico del nucleo dentro del hogar
# ------------------------------------------------------------------
library(tidyverse)
library(survey)

load("casen_2024.RData")

# ---- 0. Inspeccion de codigos (equivale a codebook / label list) ----
etq <- function(x) attr(x, "labels")
etq(casen_2024$pco1)
etq(casen_2024$pco2)
casen_2024 |> count(haven::zap_labels(pco1))
casen_2024 |> summarise(across(c(folio, nucleo, pco1, pco2, edad, expr, varstrat, varunit),
                               ~ sum(is.na(.x))))

# ---- 1. Definicion de codigos (validados con etiquetas CASEN 2024) ----
JEFE       <- 1
CONYUGE    <- c(2, 3)     # pco1: esposo(a)/pareja distinto sexo e igual sexo
HIJO_AMB   <- 4           # pco1: hijo(a) de ambos
HIJO_JEFE  <- 5           # pco1: hijo(a) solo de la jefatura
HIJO_CONY  <- 6           # pco1: hijo(a) solo del esposo(a)/pareja

JNUC       <- 1           # pco2: jefatura de nucleo
PAREJA_NUC <- c(2, 3)     # pco2: esposo(a)/pareja (ambos sexos)
HIJO_NUC   <- c(4, 5, 6)  # pco2: hijo(a) de ambos / solo jefatura / solo pareja

# Comprobacion de que 4,5,6 de pco1 son hijos segun la etiqueta
stopifnot(all(str_detect(
  str_to_lower(names(etq(casen_2024$pco1))[etq(casen_2024$pco1) %in% c(HIJO_AMB, HIJO_JEFE, HIJO_CONY)]),
  "hij")))

EDADMAX <- 18   # nhijos_menor: hijos < 18

# ---- 2. Construccion ----
base <- casen_2024 |>
  select(-any_of(c("nhijos", "nhijos_menor", "n_hijos_A", "n_hijos_B",
                   "jp_nucleo", "es_jefe_pareja", "nucleo_num"))) |>
  mutate(
    pco1_n     = haven::zap_labels(pco1),
    pco2_n     = haven::zap_labels(pco2),
    nucleo_num = as.numeric(haven::zap_labels(nucleo)),
    menor      = !is.na(edad) & edad < EDADMAX     # NA -> FALSE (como Stata)
  )

# A. Jefe/conyuge del hogar (pco1): distingue hijastros (solo para comparar)
base <- base |>
  mutate(
    h_jefe = pco1_n %in% c(HIJO_AMB, HIJO_JEFE) & menor,
    h_cony = pco1_n %in% c(HIJO_AMB, HIJO_CONY) & menor
  ) |>
  group_by(folio) |>
  mutate(nh_jefe = sum(h_jefe), nh_cony = sum(h_cony)) |>
  ungroup() |>
  mutate(n_hijos_A = case_when(
    pco1_n == JEFE            ~ nh_jefe,
    pco1_n %in% CONYUGE       ~ nh_cony,
    TRUE                      ~ NA_real_
  ))

# B. Todos los nucleos (pco2): variables que usa el modelo
base <- base |>
  mutate(h_nuc_all = pco2_n %in% HIJO_NUC,
         h_nuc_men = h_nuc_all & menor) |>
  group_by(folio, nucleo_num) |>
  mutate(nh_nuc_all = sum(h_nuc_all), nh_nuc_men = sum(h_nuc_men)) |>
  ungroup() |>
  mutate(
    jp_nucleo    = pco2_n %in% c(JNUC, PAREJA_NUC),
    # Quien no encabeza ni es pareja de un nucleo: 0 hijos corresidentes
    # (valido solo si todo el que vive con sus hijos tiene nucleo propio)
    nhijos       = if_else(jp_nucleo, as.numeric(nh_nuc_all), 0),
    nhijos_menor = if_else(jp_nucleo, as.numeric(nh_nuc_men), 0),
    n_hijos_B    = nhijos_menor,
    # Poblacion elegible a ser madre/padre en el hogar: jefe/pareja del
    # hogar o jefe/pareja de un nucleo (incluye p.ej. hija o nuera con hijos)
    es_jefe_pareja = jp_nucleo | pco1_n %in% c(JEFE, CONYUGE)
  )

casen_2024 <- base |>
  select(-any_of(c("pco1_n", "pco2_n", "menor", "h_jefe", "h_cony",
                   "nh_jefe", "nh_cony", "h_nuc_all", "h_nuc_men",
                   "nh_nuc_all", "nh_nuc_men")))

# ---- 3. Chequeos ----
table(casen_2024$nhijos, useNA = "ifany")
table(casen_2024$nhijos_menor, useNA = "ifany")
casen_2024 |> count(sexo = haven::zap_labels(sexo), nhijos_menor) |> print(n = 30)
# A y B deben coincidir en la mayoria de jefes/conyuges sin hijastros
with(casen_2024[!is.na(casen_2024$n_hijos_A), ], table(A = n_hijos_A, B = n_hijos_B))
# Cuantas personas suma la definicion nueva de es_jefe_pareja
table(es_jefe_pareja = casen_2024$es_jefe_pareja, jp_nucleo = casen_2024$jp_nucleo)

# Submuestra objetivo (minimo 300 obs)
casen_2024 |>
  mutate(activ_num = haven::zap_labels(activ)) |>
  filter(edad >= 25, edad <= 45, activ_num == 1, !is.na(yoprcor), yoprcor > 0,
         es_jefe_pareja) |>
  nrow()

# ---- 4. Estimacion con diseño muestral (equivale a svyset + svy: mean) ----
options(survey.lonely.psu = "certainty")
dis <- svydesign(ids = ~varunit, strata = ~varstrat, weights = ~expr,
                 data = casen_2024, nest = TRUE)

# subpop: mujeres 15-49 (usar subset() sobre el diseño, no filter() antes)
svymean(~n_hijos_B, subset(dis, haven::zap_labels(sexo) == 2 & edad >= 15 & edad <= 49),
        na.rm = TRUE)

# ------------------------------------------------------------------
# Registro de correcciones (declaracion de uso de IA)
# ------------------------------------------------------------------
# 1. haven_labelled: as.numeric(pco1) falla por el metodo S3; se usa
#    haven::zap_labels() antes de comparar codigos (pco1, pco2, nucleo,
#    activ, sexo). Mismo patron en scripts/02 y 03.
# 2. scripts/03: bug de regex en PED corregido con "(?:...)".
# 3. nhijos pasa de "contar hijos por folio" a la logica por nucleo
#    (pco2), adaptada de Stata/CASEN 2022 a R/CASEN 2024. Los codigos de
#    pco2 se fijaron a mano tras fallar la deteccion por etiqueta (las
#    etiquetas empiezan con el numero, p.ej. "1. Jefatura de Nucleo").
# 4. scripts/07: los proxies de crianza pasan de folio a (folio, nucleo).
