weather_observed <- function(latitude, longitude, start_date, token, options) {

  check_input(token, start_date)

  strquery <- paste(c("latitude", "longitude", "startDate"), c(latitude, longitude, start_date), sep = "=", collapse = "&")

  get_weather(token, strquery)

}

