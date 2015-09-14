get_line <- function(coordinate, n) {
  if (n == 1) {
    c(coordinate)
  } else if (n %% 2 == 0) {
    c(coordinate - (n/2):1 * 5/60, coordinate + 1:(n/2) * 5/60)
  } else {
    c(coordinate - ((n - 1)/2):1 * 5/60, coordinate, coordinate + 1:((n - 1)/2) * 5/60)
  }
}

get_grid <- function(latitude, longitude, x, y) {
# this function will provide centroid lat/lng for x*y grids
  list("latitudes" = get_line(latitude, x),
       "longitudes" = get_line(longitude, y))
}

