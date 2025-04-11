#' convert from orbital elements to Cartesian coordinates
#'
#' @param nu the true anomaly, i.e. the angular displacement measured from
#'           periapsis to the position vector along the direction of motion
#' @param a  the semi-major axis of the orbit
#' @param e  the eccentricity of the orbit
#' @param i the inclination of the orbit
#' @param Omega the Right ascension of the ascending node
#' @param w the argument of perigee
#' @param M the mass of the first body (more massive one), i.e. Earth
#' @param m the mass of the orbiting body (less massive), i.e. satellite.
#'          In the case of m << M, m can be passed as 0
#'
#' @returns a vector with I, J, K, Vi, Vj, Vk of the values of position and
#'          velocity in a geocentric-equatorial reference system
#'
#' @examples
#' \dontrun{
#' rE  <- 6378e3 # Earth equatorial radius
#' alt <- 550e3 # altitude of satellite, i.e. 500 km for LEO
#' ecc <- 0 # circular orbit, i.e. when uncontrolled reentry of debris
#' m   <- 0 # mass of debris/satellite << mass Earth, so it can be 0
#' o   <- 0
#' w   <- 0
#' i <- 10
#' kepler_to_cartesian(
#'   nu = 0.5,
#'   a = rE + alt,
#'   e = ecc,
#'   i = i, Omega = o,
#'   w = w,
#'   m = m)
#' }
kepler_to_cartesian <- function(
  nu,
  a,
  e,
  i,
  Omega,
  w,
  M = 5.9722e24, # mass of the major body, i.e. Earth
  m = 0 # mass of the other body, i.e. satellite or debris
) {
  G <- 6.67430e-11 # gravitational constant
  mu <- G * (M + m) #standard gravitational parameter

  p <- a * (1 - e^2) # semi-parameter (semi-latus rectum)
  r_0 = p / (1 + e * cos(nu))

  #--------------- Coordinates in the perifocal reference system Oxyz -----------------
  # position vector coordinates
  x <- r_0 * cos(nu)
  y <- r_0 * sin(nu)
  # velocity vector coordinates
  Vx_ <- -sqrt(mu / p) * sin(nu)
  Vy_ <- sqrt(mu / p) * (e + cos(nu))

  #-------------- the geocentric-equatorial reference system OIJK ---------------------
  # position vector components I, J, and K
  # fmt: skip
  {
    cosO <- cos(Omega)
    sinO <- sin(Omega)
    cosw <- cos(w)
    sinw <- sin(w)
    cosi <- cos(i)
    sini <- sin(i)
    I = (cosO * cosw - sinO * sinw * cosi) * x + (-cosO * sinw - sinO * cosw * cosi) * y
    J = (sinO * cosw + cosO * sinw * cosi) * x + (-sinO * sinw + cosO * cosw * cosi) * y
    K = (sinw * sini) * x                                    + (cosw * sini) * y
    # velocity vector components I', J', and K'
    Vi = (cosO * cosw - sinO * sinw * cosi) * Vx_ + (-cosO * sinw - sinO * cosw * cosi) * Vy_
    Vj = (sinO * cosw + cosO * sinw * cosi) * Vx_ + (-sinO * sinw + cosO * cosw * cosi) * Vy_
    Vk = (sinw * sini) * Vx_                                    + (cosw * sini) * Vy_
  }
  return
  c(i = I, j = J, k = K, vi = Vi, vj = Vj, vk = Vk)
}


#' latitude weight for a space object circularly orbiting at `inclination`
#'
#' @param delta_lat delta altitude [decimal degrees]
#' @param altitude  altitude of the orbiting space object [m]
#' @param inclination_deg inclination [decimal degrees]
#' @param n number of samples of the orbit
#'
#' @returns a tibble of latitudes and values for the density
#'
#' @examples
#' \dontrun{
#' rE  <- 6378e3 # Earth equatorial radius
#' alt <- 550e3 # altitude of satellite, i.e. 500 km for LEO
#' latitude_weights(0.5, rE + alt, 50)
#' }
latitude_weights <- function(delta_lat, altitude, inclination_deg, n = 10001L) {
  twopi <- 2 * pi
  n <- n + if_else((n %% 2) == 0, 1, 0)
  inclination_rad <- inclination_deg * twopi / 360.0
  nus <- seq(0, twopi, length.out = n) |>
    head(-1) |>
    as_tibble_col(column_name = "nu") |>
    mutate(inclination = inclination_rad)
  mSat <- 0.0
  ecc <- 0.0 # circular orbits
  Omega <- 0.0 # irrelevant
  w <- 0.0

  lats <- 90 - seq(delta_lat / 2, 180 - delta_lat / 2, by = delta_lat)
  nlat <- length(lats)
  vals <- vector("numeric", nlat)

  if (inclination_deg == 0) {
    vals[1 + nlat / 2] = 1
    res <- tibble::tibble(lat = lats, val = vals)
  } else {
    to_tibble <- function(x, colnames) {
      x |>
        matrix(ncol = length(colnames), dimnames = list(NULL, colnames)) |>
        as_tibble()
    }

    fff <- purrr::partial(
      kepler_to_cartesian,
      a = altitude,
      e = 0,
      Omega = Omega,
      w = w,
      m = mSat
    )

    lll <- nus |>
      mutate(
        cartesian = fff(
          nu = nu,
          i = inclination
        ) |>
          to_tibble(c("i", "j", "k", "vi", "vj", "vk"))
      ) |>
      unnest(cartesian) |>
      mutate(
        radius = sqrt(i * i + j * j + k * k),
        colatitude = acos(k / radius),
        lat = (0.5 * pi - colatitude) * 360 / twopi,
        min = pmin(trunc((90 - lat) / delta_lat), nlat - 1),
        latitude_index = 1 + min,
        lat = lats[latitude_index],
        NULL
      ) |>
      count(lat, name = "val")
    res <- lats |>
      as_tibble_col(column_name = "lat") |>
      left_join(lll, by = "lat") |>
      mutate(val = if_else(is.na(val), 0, val), val = val / n) |>
      arrange(lat)
  }
  return(res)
}
