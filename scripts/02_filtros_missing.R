# ------------------------------------------------------------------
# TIG Econometria - Pregunta 2
# Semana 2: filtros de edad/ocupacion y tratamiento de missing en
# yoprcor y esc, sobre la base con nhijos ya construido.
#
# Requiere haber corrido antes scripts/01_construccion_nhijos.R
# (o tener casen_2024 con la columna nhijos en el ambiente).
# ------------------------------------------------------------------
library(tidyverse)

# Submuestra objetivo: personas ocupadas de 25 a 45 anios
muestra <- casen_2024 |>
  mutate(activ_num = haven::zap_labels(activ)) |>
  filter(edad >= 25, edad <= 45, activ_num == 1)

nrow(muestra)

# Missing en yoprcor y esc dentro de la submuestra filtrada
muestra |>
  summarise(
    n = n(),
    na_yoprcor = sum(is.na(yoprcor)),
    pct_na_yoprcor = round(100 * na_yoprcor / n, 2),
    na_esc = sum(is.na(esc)),
    pct_na_esc = round(100 * na_esc / n, 2)
  )

# El missing en yoprcor no es aleatorio: se concentra casi por completo
# en o15 == 9 ("Familiar no remunerado"), donde el 100% de esas
# observaciones tiene yoprcor = NA. Es estructural (no tienen ingreso
# laboral por definicion de la categoria), no un problema de
# no-respuesta, por lo que se excluyen de forma explicita.
muestra |>
  mutate(o15_num = haven::zap_labels(o15)) |>
  group_by(o15_num) |>
  summarise(n = n(), na_yoprcor = sum(is.na(yoprcor)), pct_na = round(100 * na_yoprcor / n, 1)) |>
  arrange(desc(pct_na))

# No hay valores de yoprcor <= 0 (no se necesita filtro adicional por eso)
muestra |>
  filter(!is.na(yoprcor)) |>
  summarise(n_cero_o_neg = sum(yoprcor <= 0), min_val = min(yoprcor))

# Missing en esc no muestra un patron marcado por sexo
muestra |>
  group_by(sexo) |>
  summarise(n = n(), na_esc = sum(is.na(esc)))

# Muestra final: se excluyen los "familiar no remunerado" (sin ingreso
# por definicion, 145 obs) junto con el resto de los NA en yoprcor y
# esc (no-respuesta dispersa, ~2% y ~0.2% de la submuestra respectivamente)
muestra_final <- muestra |>
  filter(!is.na(yoprcor), !is.na(esc))

nrow(muestra_final)
nrow(muestra) - nrow(muestra_final)
