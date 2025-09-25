
library(tidyverse)

ALL <- read_rds("all_destinos.Rds")
S_ALL <- reduce(ALL, `+`)

str(S_ALL)
length(S_ALL[S_ALL < 0]) / 80^2 * 100 # less than 1% is negative

sectors <- colnames(S_ALL)

ori_dest <- S_ALL |>
  as_tibble()

ori_dest[ori_dest < 0] <- 0

Sums <- ori_dest |>
  colSums()

relative_buys <- function(col) {
  total <- sum(col)
  if(total > 0) {
    col / total
  } else {
    return(rep(0, length(col)))
  }
}


CLU_ori_dest <- ori_dest |>
  mutate(across(everything(), relative_buys)) |>
  as.matrix() |>
  t()

rowSums(CLU_ori_dest)

myinst <- function(pack) {
  renv::install(pack, prompt = FALSE)
}

library(Rtsne)
set.seed(424242)
tsne <- Rtsne(CLU_ori_dest,
              dims = 2, perplexity = 1, verbose = TRUE,
              partial_pca = TRUE, num_threads = 12
)

tsne_coords <- tsne$Y |>
  as_tibble()

ggplot(tsne_coords) +
  geom_point(aes(x = V1, y = V2))

library(philentropy)
# myinst("dbscan")

KL <- KL(CLU_ori_dest, unit = 'log') |> as.dist()
# HDB_wdl <- hdbscan(as.dist(wdl), minPts = MIN_PTS)