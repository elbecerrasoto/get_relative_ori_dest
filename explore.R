
library(tidyverse)
library(readxl)

PATH <- "tabulados_TODFBCF/"
XLSX_FILE <- "SubsectorConstantes.txt"

INPUTS <- readLines(XLSX_FILE) %>%
  str_c(PATH, .)
IN <- INPUTS[[1]]

SKIP_ABOVE <- 6
SKIP_BELOW <- 5
x <- read_xlsx(IN, col_names = FALSE) |>
  slice_tail(n = SKIP_ABOVE) |>
  slice_head(n = SKIP_BELOW)

