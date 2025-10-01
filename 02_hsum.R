library(tidyverse)

ALL <- read_rds("all_destinos.Rds")
map_nat_bir <- read_tsv("ori_bir.tsv.tsv")

secs_nat <- map_nat_bir$`SECTOR MIP NACIONAL`
secs_bir <- unique(map_nat_bir$`SECTOR MIP BIREGIONAL`)

nat_dir <- 1:length(secs_nat)
bir_dir <- 1:length(secs_bir)

nat_dir <- set_names(nat_dir, secs_nat)
bir_dir <- set_names(bir_dir, secs_bir)

nat_dir[secs_nat]
bir_dir[secs_nat]


x <- ALL[[1]]
x78 <- x[1:78, 1:78]

nat_dir
