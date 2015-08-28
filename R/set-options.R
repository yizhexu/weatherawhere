get_attribute <- function(type = "all") {

  type <- tolower(type)
  type <- ifelse(nchar(type) > 4, substr(type, 1, 4), type)

  attribute <- c('minTemperature','maxTemperature','precip','accPrecip','accPrecipPriorYear','accPrecip3YearAverage','accPrecipLongTermAverage','solar', 'minHumidity','maxHumidity','mornWind','maxWind','gdd','accGdd','accGddPriorYear','accGdd3YearAverage','accGddLongTermAverage','pet','accPet','ppet')

  match <- grep(paste0(type, sep = "", collapse = "+"), tolower(attribute))

  if(type == "all") {
    attribute
  } else if (length(match) == 0) {
    stop("No valid attributes found. Type in get_attribute() for all available attributes")
  } else {
    attribute[match]
  }
}

set_options <- function(x, input) {
  query <- if(x = "attribute") {
    paste("attribute", input, sep = "=", collapse = "&")
  } else {
    paste()
  }
}

structure(parameter, class = "parameter")

parameter <- get_parameter("weather_observed")
parameter$attribute$pet

class(parameter$attribute)
