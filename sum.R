library(tidyverse)
MIN_CLU <- 2


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
  if (total > 0) {
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
library(dbscan)

# KL <- KL(CLU_ori_dest, unit = 'log') |> as.dist()
# KL <- dist(CLU_ori_dest)
KL <- JSD(CLU_ori_dest, unit = "log2") |> as.dist()

HDB_wdl <- hdbscan(KL, minPts = MIN_CLU)

tsne_coords$cluster <- factor(HDB_wdl$cluster)

scian <- names(ori_dest) |>
  str_remove("_.*")
tsne_coords$scian <- scian

library(ggrepel)
library(ggthemes)

p <- ggplot(tsne_coords, aes(x = V1, y = V2, color = cluster)) +
  geom_text_repel(
    aes(
      label = scian
    )
  ) +
  geom_point() +
  theme_fivethirtyeight(base_size = 18)


p_filtered <- ggplot(
  tsne_coords |> filter(cluster != 0),
  aes(x = V1, y = V2, color = cluster)
) +
  geom_text_repel(
    aes(
      label = scian
    )
  ) +
  geom_point() +
  theme_fivethirtyeight(base_size = 18)


ggsave("sectores_hermanados_inversion.png", p_filtered,
  units = "cm", width = 24, height = 16
)


tsne_coords$sector <- names(ori_dest)
tsne_coords <- tsne_coords |>
  arrange(cluster)

tsne_coords |>
  write_tsv("similar_structures.tsv")
ggsave("similar_structure.svg", p, units = "cm", width = 22, height = 16)
