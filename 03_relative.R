library(tidyverse)

ori_dest <- read_rds("results/02_summed.Rds")
tmp <- read_tsv("data/shocks_template.tsv")
OUT <- "results/03_relative.Rds"

sectors <- unique(tmp$sector)

length(ori_dest[ori_dest < 0]) / 80^2 * 100 # less than 1% is negative

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

analyze_sector <- function(Trel_ori_dest, colN) {
  Qstruct <- Trel_ori_dest |>
    select(sector, all_of(colN)) |>
    filter(.data[[colN]] != 0) |>
    arrange(desc(.data[[colN]]))
  Qstruct
}

xOUT <- map(
  names(Trel_ori_dest)[-1],
  \(colN) analyze_sector(
    Trel_ori_dest,
    colN
  )
)


write_rds(xOUT, OUT)
