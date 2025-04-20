# H3 hexagons covering NM area bbox

library(aviodebris)

res <- 3
g <- plot_hexes_map(res)
ggsave(filename = here("figures", str_glue("cells_{res}.png")), plot = g)

res <- 2
g <- plot_hexes_map(res)
ggsave(filename = here("figures", str_glue("cells_{res}.png")), plot = g)
