grid_weather <- function(latitude, longitude, x, y, start_date, token, options) {

  check_input(token, start_date)

  grid <- get_grid(latitude, longitude, x, y)

  strquery <- outer(1:x, 1:y, Vectorize(function(x, y) {
    paste(c("latitude", "longitude", "startDate"),
          c(grid$latitudes[x], grid$longitudes[y], start_date),
          sep = "=", collapse = "&")
  }))

  registerDoParallel(detectCores())

  foreach(i = 1:dim(strquery)[1], .combine = 'rbind')  %:%
  foreach(j = 1:dim(strquery)[2], .combine = 'rbind', .export = "get_weather")  %dopar% {
    get_weather(token, strquery[i, j])
  }

}



