library(tidyverse)

ALL <- read_rds("all_destinos.Rds")
map_ori_bir <- read_tsv("ori_bir.tsv", col_types = "ffff")

MAPVEC <- set_names(map_ori_bir$scian_bir, map_ori_bir$scian_ori)
which_MAPVEC <- function(iname) {
  which(MAPVEC == MAPVEC[[iname]])
}

mapped <- map(names(map_vec), which_MAPVEC) |>
  unique()

mapped <- set_names(mapped, unique(map_ori_bir$scian_bir))
M <- ALL[[1]]

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
