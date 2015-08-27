weather_observed <- function(latitude, longitude, start_date, options) {

  url_data <- "https://api.awhere.com/v1/weather"

  # parameter validation
  if ( "token" %in% ls() ) {
    stop("All regular API requests require a token. Use get_token() to get one")
  } else if (expire_time < Sys.time())  {
    stop("All API tokens expire after an hour. Use get_token() to get one")
  } else if ( is.numeric(latitude) == FALSE | is.numeric(latitude) == FALSE ) {
    stop("Latitude and longitude has to be decimal-formatted numbers")
  } else if ( !lubridate::is.Date(as.Date(start_date, "%Y-%m-%d")) ) {
    stop("Start date has to be ISO-8601 formatted date in format: YYYY-MM-DD")
  }

  # heads up about forecast data
  forecast <- 8 - as.numeric(Sys.Date() - as.Date(start_date))

  if ( forecast > 0 ) cat("The result will include", forecast, "days of forecasting data\n")

  strquery <- paste(c("latitude", "longitude", "startDate"), c(latitude, longitude, start_date), sep = "=", collapse = "&")

  request <- httr::GET(url_data, query = strquery, httr::add_headers(Authorization = token))

  if ( request$status != 200 ) {
    cat(paste("API call failed. \nStatus", request$status))
  } else {
    cat(paste("API call success! \n"))
    data <- jsonlite::fromJSON(httr::content(request, as = "text"))

    data <- data.frame(latitude = data$latitude,
                       longitude = data$longitude,
                       date = as.Date(data$date),
                       data$dailyAttributes,
                       stringsAsFactors = FALSE)
  }
}

grid_weather <- function(latitude, longitude, x, y, start_date, options) {

  doParallel::registerDoParallel(detectCores())
  grid <- get_grid(latitude, longitude, x, y)

  strquery_normal <- function(x,y) {
    outer(1:x, 1:y, Vectorize(function(x, y) {
      paste(c("latitude", "longitude", "startDate"),
            c(grid$latitudes[x], grid$longitudes[y], start_date),
            sep = "=", collapse = "&")
    }))
  }

  strquery_par <- function(x,y) {
    unlist(foreach(x = 1:x) %:%
             foreach(y = 1:y) %dopar% {
               paste(c("latitude", "longitude", "startDate"),
                     c(grid$latitudes[x], grid$longitudes[y], start_date),
                     sep = "=", collapse = "&")
             })
  }
  library(microbenchmark)
  microbenchmark(strquery_normal(x,y), strquery_par(x,y), times=20L, unit = "relative")

}

rep(grid$latitudes, y)
rep(grid$longitudes, x)
