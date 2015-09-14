check_date <- function(date) {
  # parameter validation
  if ( !lubridate::is.Date(as.Date(date, "%Y-%m-%d")) ) {
    stop("Date has to be ISO-8601 formatted date in format: YYYY-MM-DD")
  }
}

check_token <- function(token) {
  # parameter validation
  if ( missing(token) | "expire_time" %in% ls()) {
    stop("All regular API requests require a token. Use get_token() to get one")
  } else if (expire_time < Sys.time())  {
    stop("All API tokens expire after an hour. Use get_token() to get one")
  }
}

check_location <- function(coordinate) {
  if ( is.numeric(coordinate) == FALSE | is.numeric(coordinate) == FALSE ) {
    stop("Latitude and longitude has to be decimal-formatted numbers")
  }
}

check_query <- function(strquery) {
  if ( class(strquery) != "query" ) {
    stop("Invalid type of query string")
  }
}
