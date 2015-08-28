check_input <- function(token, start_date) {
  # parameter validation
  if ( !"token" %in% ls() ) {
    stop("All regular API requests require a token. Use get_token() to get one")
  } else if (expire_time < Sys.time())  {
    stop("All API tokens expire after an hour. Use get_token() to get one")
  } else if ( is.numeric(latitude) == FALSE | is.numeric(latitude) == FALSE ) {
    stop("Latitude and longitude has to be decimal-formatted numbers")
  } else if ( !lubridate::is.Date(as.Date(start_date, "%Y-%m-%d")) ) {
    stop("Start date has to be ISO-8601 formatted date in format: YYYY-MM-DD")
  } else {

    # heads up about forecast data
    forecast <- as.numeric(Sys.Date() - as.Date(start_date)) - 1

    if ( forecast > 0 ) cat("The result will include", forecast, "days of forecasting data\n")

  }
}
