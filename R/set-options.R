set_attribute <- function(type = "all") {

  # list of all available attributes
  attribute <- c('minTemperature','maxTemperature','precip','accPrecip','accPrecipPriorYear','accPrecip3YearAverage','accPrecipLongTermAverage','solar', 'minHumidity','maxHumidity','mornWind','maxWind','gdd','accGdd','accGddPriorYear','accGdd3YearAverage','accGddLongTermAverage','pet','accPet','ppet')

  type <- tolower(type)
  type <- ifelse(nchar(type) > 4, substr(type, 1, 4), type)

  match <- grep(paste0(type, sep = "", collapse = "+"), tolower(attribute))

  if ("all" %in% type) {
    att <- attribute
  } else if (length(match) == 0) {
    stop("No valid attributes found. Type in get_attribute() for all available attributes")
  } else {
    att <- attribute[match]
  }

  query <- paste("attribute", att, sep = "=", collapse = "&")
  structure(query, class = "query")
}

set_attribute(c("acc", "pet"))
set_attribute()

set_date <- function(start_date, end_date, plant_date) {

}

get_options <- function(end_date, plant_date, temperature, gdd) {
  if (!lubridate::is.Date(as.Date(end_date, "%Y-%m-%d")))


}
