#' H3 hexagons covering a bounding box.
#'
#' @param bbox bounding box to be covered by H3 hexagons
#' @param resolution H3 hexagons' resolution
#'
#' @returns simple features of H3 hexagons polygons and relevant addresses
#'
bbox_cells_at_res <- function(bbox, resolution) {
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
cells_boundary_at_res <- function(bbox, resolution) {
  bbox_cells_at_res(bbox, resolution) |>
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
