bbox_cells_at_res <- function(bbox, resolution) {
  bbox |>
    sf::st_bbox(crs = 4326) |>
    sf::st_as_sfc(crs = 4326) |>
    # add points to the bbox polygon
    smoothr::densify(n = 300L) |>
    sf::st_as_sf(resolution = resolution) |>
    sf::st_transform(crs = 4326) |>
    h3jsr::polygon_to_cells(res = res, simple = FALSE) |>
    pull(h3_addresses) |>
    unlist() |>
    h3jsr::cell_to_polygon(simple = FALSE)
}

cells_boundary_at_res <- function(bbox, resolution) {
  bbox_cells_at_res(bbox, resolution) |>
    st_union() |>
    st_exterior_ring() |>
    st_as_sf() |>
    mutate(resolution = res)
}

bbox_nm <- function() {
  # bbox Europe/EUROCONTROL area
  # -25.488281,26.638253,45.407181,71.864780
  c(
    xmin = -25.488281,
    ymin = 26.638253,
    xmax = 45.407181,
    ymax = 71.864780)
}
