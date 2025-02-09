

plot_hexes_map <- function(res) {
  eur_hex <- bbox_nm() |>
    hexes_for_bbox_at_res(resolution = res)

  eur_hex_union <- bbox_nm() |>
    # hex union at res 2
    hexes_for_bbox_at_res(resolution = 2) |>
    sf::st_union() |>
    st_exterior_ring() |>
    st_as_sf() |>
    mutate(resolution = 2)

  eur_centreoid <- eur_hex |>
    pull(h3_address) |>
    unlist() |>
    cell_to_point(simple = FALSE)

  gisco_RG <- giscoR::gisco_get_countries(
    resolution = "20",
    epsg = "4326",
    spatialtype = "RG",
    region = c("Europe", "Africa", "Asia")) |>
    mutate(res = "20M")

  gisco <- gisco_RG

  EUR_res20 <- gisco |>
    st_intersection(eur_hex_union) |>
    select(res)

  g <- ggplot() +
    geom_sf(data = EUR_res20, fill = 'lightgrey', linewidth = 0.4) +
    geom_sf(data = eur_hex, fill = NA, colour = 'red', linewidth = 0.3) +
    geom_sf(data = eur_centreoid, fill = NA, colour = 'blue', size = 0.05) +
    theme_minimal() +
    coord_sf(crs = "ESRI:102013", datum = NA)
  g

}







#' H3 hexagons covering a bounding box.
#'
#' @param bbox bounding box to be covered by H3 hexagons
#' @param resolution H3 hexagons' resolution
#'
#' @returns simple features of H3 hexagons polygons and relevant addresses
#'
hexes_for_bbox_at_res <- function(bbox, resolution) {
  bbox |>
    sf::st_bbox(crs = 4326) |>
    sf::st_as_sfc(crs = 4326) |>
    # increase points to the bbox polygon
    smoothr::densify(n = 300L) |>
    sf::st_as_sf(resolution = resolution) |>
    sf::st_transform(crs = 4326) |>
    h3jsr::polygon_to_cells(res = res, simple = FALSE) |>
    dplyr::pull(h3_addresses) |>
    unlist() |>
    h3jsr::cell_to_polygon(simple = FALSE)
}


#' Return the bounding box of the H3 hexagons covering a user bounding box.
#'
#' @param bbox the user bounding box
#' @param resolution the resolution of the H3 hexagons covering `bbox`
#'
#' @returns the bounding box containing the H3 hexagons covering `bbox`
#'
bbox_of_hexes_for_bbox_at_res <- function(bbox, resolution) {
  hexes_for_bbox_at_res(bbox, resolution) |>
    st_union() |>
    st_exterior_ring() |>
    st_as_sf() |>
    mutate(resolution = res)
}


#' Approximate bounding box for NM area
#'
#' @returns bounding box for NM area
bbox_nm <- function() {
  # bbox Europe/EUROCONTROL area
  # -25.488281,26.638253,45.407181,71.864780
  c(
    xmin = -25.488281,
    ymin = 26.638253,
    xmax = 45.407181,
    ymax = 71.864780)
}
