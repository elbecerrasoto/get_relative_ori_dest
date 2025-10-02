library(tidyverse)
library(readxl)
library(janitor)
library(furrr)

# ARGV <- commandArgs(trailingOnly = TRUE)
# TYPE <- ARGV[[1]]
# INPUT <- ARGV[[2]]
TYPE <- "nacional"
INPUT <- "data/subsector_nacional.txt"
OUTPUT <- "results/nacional.tsv"

DIR <- "tabulados_TODFBCF/"
MAP_FILE <- "data/ori_bir.tsv"
SHORT_NAMES <- "data/shocks_template.tsv"

CORES <- future::availableCores()
if (.Platform$OS.type != "windows") {
  plan(multicore, workers = CORES)
} else {
  plan(multisession, workers = CORES)
}

INPUTS <- readLines(INPUT) %>%
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

# Read them

ALL <- future_map(INPUTS, read_origen_destino)

# Sum them

map_ori_bir <- read_tsv(MAP_FILE, col_types = "ffff")

MAPVEC <- set_names(map_ori_bir$scian_bir, map_ori_bir$scian_ori)
which_MAPVEC <- function(iname) {
  which(MAPVEC == MAPVEC[[iname]])
}

mapped <- map(names(MAPVEC), which_MAPVEC) |>
  unique()

mapped <- set_names(mapped, unique(map_ori_bir$scian_bir))


collapse_matrix <- function(M, mapped) {
  reduce_cols <- function(M, idxs) {
    reduce(map(
      idxs,
      \(i) M[, i]
    ), `+`)
  }

  reduce_rows <- function(M, idxs) {
    reduce(map(
      idxs,
      \(i) M[i, ]
    ), `+`)
  }

  col_reduced <- map(mapped, \(idxs)
  reduce_cols(M, idxs)) %>%
    do.call(cbind, .)

  row_reduced <- map(mapped, \(idxs)
  reduce_rows(col_reduced, idxs)) %>%
    do.call(rbind, .)

  row_reduced
}

all_collapsed <- map(ALL, \(M) collapse_matrix(M, mapped))
ori_dest <- reduce(all_collapsed, `+`)

# Get the relative frecuencies

tmp <- read_tsv(SHORT_NAMES)
sectors <- unique(tmp$sector)

# length(ori_dest[ori_dest < 0]) / 80^2 * 100 # less than 1% is negative

ori_dest[ori_dest < 0] <- 0

Sums <- ori_dest |>
  colSums()

relative_buys <- function(col) {
  total <- sum(col)
  if (total > 0) {
    col / total
  } else {
    return(rep(0, length(col)))
  }
}

rel_ori_dest <- apply(ori_dest, 2, relative_buys)
colnames(rel_ori_dest) <- sectors

Trel_ori_dest <- rel_ori_dest |> as_tibble()
Trel_ori_dest <- Trel_ori_dest |>
  mutate(sector = sectors) |>
  relocate(sector)

Trel_ori_dest$type <- TYPE

Trel_ori_dest |>
  relocate(type, .after = sector) |>
  write_tsv(OUTPUT)
