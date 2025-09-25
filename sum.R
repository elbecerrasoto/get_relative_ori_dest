
library(tidyverse)

ALL <- read_rds("all_destinos.Rds")
S_ALL <- reduce(ALL, `+`)

str(S_ALL)
length(S_ALL[S_ALL < 0]) / 80^2 * 100 # less than 1% is negative

sectors <- colnames(S_ALL)

ori_dest <- S_ALL |>
  as_tibble()

names(ori_dest)  

sectors[ori_dest$x325_industria_quimica > 0]
sectors
iQ_sum <- sum(ori_dest$x325_industria_quimica)

iQ_percents <- round(ori_dest$x325_industria_quimica / iQ_sum * 100, 2)

iQ_percents <- set_names(iQ_percents, sectors)
ori_dest$x325_industria_quimica

iQsorted <- tibble(sectors = sectors,
       iQ = iQ_percents) |>
  filter(iQ > 0) |>
  arrange(desc(iQ))

iQsorted |>
  write_tsv("industria_quimica_mexico.tsv")

iQsorted <- read_tsv("industria_quimica_mexico.tsv")

p <- ggplot(iQsorted, aes(x = reorder(sectors,-iQ)))+
 geom_col(aes(y = iQ))

p

p <- p +
  ggthemes::theme_fivethirtyeight() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(subtitle = "¿A qué sectores le compra\nla Industria Química?\nPorcentajes en México",
       x = "Sector", y = "Porcentaje")

ggsave("iQ.png", units = "cm", width = 18, height = 24)
