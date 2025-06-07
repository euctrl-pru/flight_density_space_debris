# plot U-shaped impact probability density for debris of inclination `i`

library(tidyverse)
library(aviodebris)

# Earth equatorial radius
rE <- 6378e3
# altitude of satellite from (spherical) Earth surface, i.e. 500 km for LEO
alt <- 550e3

# inclination of Kosmos 482 Descent Craft (1972-023E, cat. nr. 6073)
# see https://sattrackcam.blogspot.com/2025/04/kosmos-842-descent-craft-reentry.html
inclination_deg = 51.95

www <- latitude_weights(
  delta_lat = 0.5,
  altitude = rE + alt,
  inclination_deg = inclination_deg
)
www |>
  ggplot() +
  geom_line(aes(x = lat, y = val), linewidth = 1.3) +
  scale_x_continuous("latitude", breaks = c(-90, -60, -30, 0, 30, 60, 90)) +
  labs(y = "density") +
  theme_minimal(base_size = 24) +
  theme(
    panel.grid.minor.x = element_blank(),
  )

ggsave("media/figures/weighting_inclination_i.png")
