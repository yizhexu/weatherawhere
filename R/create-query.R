create_query <- function(latitude, longitude, x = NULL, y = NULL, date = NULL, attribute = NULL, gdd_method = NULL) {

  check_location(latitude)
  check_location(longitude)

  if(is.null(x)) x <- 1
  if(is.null(y)) y <- 1

  grid <- get_grid(latitude, longitude, x, y)

  str_loc <- outer(1:x, 1:y, Vectorize(function(x, y) {
    paste(c("latitude", "longitude"),
          c(grid$latitudes[x], grid$longitudes[y]),
          sep = "=", collapse = "&")
  }))

  str_att <- if(!is.null(attribute)) paste0("&", paste("attribute", attribute, sep = "=", collapse = "&"))

  str_date <- if(!is.null(date)) {
    paste0("&", paste(names(date), date, sep = "=", collapse = "&"))
  } else {
    paste0("&", paste("startDate", set_date(Sys.Date()), sep = "=", collapse = "&"))
  }

  str_gdd <- if(!is.null(gdd_method)) paste0("&", paste(names(gdd_method), gdd_method, sep = "=", collapse = "&"))

  query <- paste0(str_loc, str_att, str_date, str_gdd)
  structure(query, class = "query")
}


get_token("yizhexu@awhere.com", "181225tiancai@X")
strquery <- create_query(latitude, longitude, x = 2, y = 4, set_date("2015-05-15", "2015-06-15"), get_attribute("precip"))
strquery <- create_query(latitude, longitude)

get_weather(token, strquery)
