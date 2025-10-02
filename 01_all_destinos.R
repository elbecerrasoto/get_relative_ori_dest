library(tidyverse)
library(readxl)
library(janitor)
library(furrr)

# grep 'Por subsector de actividad' tabulados_TODFBCF/indice_archivos.txt  | grep 'Valores constantes base 2018 en millones de pesos' | grep 'Importado' |  perl -pe 's/^(.*\.xlsx)\|.*/\1/'


CORES <- future::availableCores()
if (.Platform$OS.type != "windows") {
  plan(multicore, workers = CORES)
} else {
  plan(multisession, workers = CORES)
}

DIR <- "tabulados_TODFBCF/"
NAMES <- "data/subsector_total.txt"
OUT <- "results/01_all_destinos.Rds"

INPUTS <- readLines(NAMES) %>%
  str_c(DIR, .)

read_origen_destino <- function(path) {
  SKIP_ABOVE <- 6
  SKIP_BELOW <- 5
  x <- read_xlsx(path, col_names = FALSE) |>
    slice_tail(n = -SKIP_ABOVE) |>
    slice_head(n = -SKIP_BELOW)

  sectors <- x[[1]] |>
    janitor::make_clean_names()

  x <- x[, -(1:2)] |>
    mutate(across(everything(), ~ as.double(.x)))

  x <- x |>
    set_names(sectors)

  x |> as.matrix()
}

ALL <- future_map(INPUTS, read_origen_destino)
write_rds(ALL, OUT)
