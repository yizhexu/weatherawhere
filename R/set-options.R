get_parameter <- function( type = c("weather_observed", "weather_forecast") ) {
  parameter <- if (type == "weather_observed") {
    list("attribute" = list( "all" = c('minTemperature','maxTemperature','precip','accPrecip','accPrecipPriorYear','accPrecip3YearAverage','accPrecipLongTermAverage','solar', 'minHumidity','maxHumidity','mornWind','maxWind','gdd','accGdd','accGddPriorYear','accGdd3YearAverage','accGddLongTermAverage','pet','accPet','ppet'),
                             "precipitation" = c('precip','accPrecip','accPrecipPriorYear','accPrecip3YearAverage','accPrecipLongTermAverage'),
                             "precipitation_acc" = c('accPrecip','accPrecipPriorYear','accPrecip3YearAverage','accPrecipLongTermAverage'),
                             "temperature" = c('minTemperature','maxTemperature'),
                             "solar" = c('solar'),
                             "humidity" = c('minHumidity','maxHumidity'),
                             "wind" = c('mornWind','maxWind'),
                             "gdd" = c('gdd','accGdd','accGddPriorYear','accGdd3YearAverage','accGddLongTermAverage'),
                             "gdd_acc" = c('accGdd','accGddPriorYear','accGdd3YearAverage','accGddLongTermAverage'),
                             "pet" = c('pet','accPet','ppet')),
         "parameter" = list("date" = c("endDate", "plantDate"),
                          "gdd" = c("temperatureUnits", "gddMethod", "baseTemp", "maxTempCap", "minTempCap"))
    )
  }

  structure(parameter, class = "parameter")
}

parameter <- get_parameter("weather_observed")
parameter$attribute$pet

class(parameter$attribute)
