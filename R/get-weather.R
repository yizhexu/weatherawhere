weather_observed <- function(latitude, longitude, start_date, options) {

  url_data <- "https://api.awhere.com/v1/weather"

  # parameter validation
  if ( Token %in% ls() == FALSE | expire_time < Sys.time()) {

    stop("All regular API requests require a token. Tokens expire after an hour. Use get_token() to receive one")

  } else if ( is.number(latitude) == FALSE | is.number(latitude) == FALSE ) {

    stop("Latitude and longitude has to be decimal-formatted numbers")

  } else if ( lubridate::is.Date(as.Date(start_date, "%Y-%m-%d")) ) {

    stop("Start date has to be ISO-8601 formatted date in format: YYYY-MM-DD")

  }

  # heads up about forecast data
  forecast <- 8 - as.numeric(Sys.Date() - as.Date(start_date))

  if( forecast > 0 ) cat("The result will include", forecast, "days of forecasting data")

  strquery <- paste(c("latitude", "longitude", "startDate"), c(latitude, longitude, start_date), sep = "=", collapse = "&")

  request <- GET(url_data, query = strquery, add_headers(Authorization = token))

  data <- jsonlite::fromJSON(content(request, as = "text"))

  data <- data.frame(latitude = data$latitude,
                     longitude = data$longitude,
                     date = as.Date(data$date),
                     data$dailyAttributes,
                     stringsAsFactors=FALSE)

}



