get_weather <- function(token, strquery) {

  url_data <- "https://api.awhere.com/v1/weather"

  request <- httr::GET(url_data, query = strquery, httr::add_headers(Authorization = token))

  if ( request$status != 200 ) {
    cat(paste("API call failed. \nStatus", request$status))
  } else {
    # cat(paste("API call success! \n"))
    data <- jsonlite::fromJSON(httr::content(request, as = "text"))

    data.frame(latitude = data$latitude,
                       longitude = data$longitude,
                       date = as.Date(data$date),
                       data$dailyAttributes,
                       stringsAsFactors = FALSE)

  }
}

