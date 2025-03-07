# from https://www.urbandemographics.org/post/figures-map-layers-r/
# BUT see also https://marcosci.github.io/layer/

library("sf")
library("raster")
library("stars")
library("r5r")
library("geobr")
library("aopdata")
library("gtfs2gps")
library("ggplot2")
library("osmdata")
library("h3jsr")
library("viridisLite")
library("ggnewscale")
library("dplyr")
library("magrittr")

withr::local_options(list(java.parameters = '-Xmx2G'))

rotate_data <- function(data, x_add = 0, y_add = 0) {

  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }

  rotate_matrix <- function(x){
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }
  data %>%
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }

  rotate_matrix <- function(x) {
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }
  data %>%
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}



### get terrain data ----------------

elev <- system.file("extdata/poa/poa_elevation.tif", package = "r5r")

# read terrain raster and calculate hill Shade
dem <- stars::read_stars(elev)
dem <- st_as_sf(dem)

# crop
bbox <- st_bbox(dem)


### get public transport network data ----------------

gtfs <- gtfs2gps::read_gtfs( system.file("extdata/poa/poa_eptc.zip", package = "r5r") )
gtfs <- gtfs2gps::gtfs_shapes_as_sf(gtfs)

# crop
gtfs <- gtfs[bbox,]
gtfs <- st_crop(gtfs, bbox)
plot(gtfs['shape_id'])


### get OSM data ----------------

# roads from OSM
roads <- opq('porto alegre') %>%
  add_osm_feature(key = 'highway',
                  value = c("motorway", "primary","secondary")) %>% osmdata_sf()

roads <- roads$osm_lines

# crop
roads2 <- roads[bbox,]
roads2 <- st_crop(roads2, bbox)
plot(roads2['osm_id'])


### get H3 hexagonal grid ----------------

# get poa muni and hex ids
poa <- read_municipality(code_muni = 4314902 )
hex_ids <- h3jsr::polygon_to_cells(poa, res = 7, simple = TRUE)

# pass h3 ids to return the hexagonal grid
hex_grid <- h3jsr::cell_to_polygon(hex_ids, simple = FALSE)
plot(hex_grid)

# crop
hex_grid <- hex_grid[bbox,]
hex <- st_crop(hex_grid, bbox)
plot(hex)


### get land use data from AOP project ----------------
#' more info at https://www.ipea.gov.br/acessooportunidades/en/

landuse <- aopdata::read_access(city = 'poa', geometry = T, mode='public_transport')

# crop
landuse <- landuse[bbox,]
landuse <- st_crop(landuse, bbox)
plot(landuse['CMATT30'])

# hospitals
# generate one point per hospital in corresponding hex cells
df_temp <- subset(landuse, S004>0)
hospitals <- st_sample(x = df_temp, df_temp$S004, by_polygon = T)
hospitals <- st_sf(hospitals)
hospitals$geometry <- st_geometry(hospitals)
hospitals$hospitals <- NULL
hospitals <- st_sf(hospitals)
plot(hospitals)

# schools
# generate one point per schools in corresponding hex cells
df_temp <- subset(landuse, E001>0)
schools <- st_sample(x = df_temp, df_temp$E001, by_polygon = T)
schools <- st_sf(schools)
schools$geometry <- st_geometry(schools)
schools$schools <- NULL
schools <- st_sf(schools)
plot(schools)




### plot  ----------------

# annotate parameters
x = -141.25
color = 'gray40'

temp1 <- ggplot() +
  # terrain
  geom_sf(
    data = dem |> rotate_data(),
    aes(fill = poa_elevation.tif),
    color = NA,
    show.legend = FALSE
  ) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  annotate("text", label = 'Terrain', x = x, y= -8.0, hjust = 0, color = color) +
  labs(caption = "image by @UrbanDemog")

temp2 <- temp1 +
  # pop income
  new_scale_fill() +
  new_scale_color() +
  geom_sf(
    data = landuse |> subset(P001 > 0) |> rotate_data(y_add = 0.1),
    aes(fill = R001),
    color = NA,
    show.legend = FALSE) +
  scale_fill_viridis_c(option = 'E') +
  annotate("text", label = 'Population', x = x, y = -7.9, hjust = 0, color = color) +
  # schools
  geom_sf(data = hex %>% rotate_data(y_add = .2), color='gray50', fill=NA, size=.1) +
  geom_sf(data = schools %>% rotate_data(y_add = .2), color='#0f3c53', size=.1, alpha=.8) +
  annotate("text", label='Schools', x=x, y= -7.8, hjust = 0, color=color) +

  # hospitals
  geom_sf(data = hex %>% rotate_data(y_add = .3), color='gray50', fill=NA, size=.1) +
  geom_sf(data = hospitals %>% rotate_data(y_add = .3), color='#d5303e', size=.1, alpha=.5) +
  annotate("text", label='Hospitals', x=x, y= -7.7, hjust = 0, color=color) +

  # OSM
  geom_sf(data = roads2 %>% rotate_data(y_add = .4), color='#019a98', size=.2) +
  annotate("text", label='Roads', x=x, y= -7.6, hjust = 0, color=color) +

  # public transport
  geom_sf(data = gtfs %>% rotate_data(y_add = .5), color='#0f3c53', size=.2) +
  annotate("text", label='Public transport', x=x, y= -7.5, hjust = 0, color=color) +

  # accessibility
  new_scale_fill() +
  new_scale_color() +
  geom_sf(data = subset(landuse, P001>0) %>% rotate_data(y_add = .6), aes(fill=CMATT30), color=NA, show.legend = FALSE) +
  scale_fill_viridis_c(direction = 1, option = 'viridis' ) +
  theme(legend.position = "none") +
  annotate("text", label='Accessibility', x=x, y= -7.4, hjust = 0, color=color) +
  theme_void() +
  scale_x_continuous(limits = c(-141.65, -141.1))


# save plot
ggsave(plot = temp2, filename = 'map_layers.png',
       dpi=200, width = 15, height = 16, units='cm')
