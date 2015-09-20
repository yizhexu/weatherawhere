# library for awhere data
library(httr)
library(jsonlite)
library(magrittr)
library(doParallel)
library(foreach)
library(lubridate)
library(devtools)

# library for shiny
library(leaflet)
library(shiny)
library(markdown)
library(sp)

# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/check-input.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/create-query.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/date-year.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/get-grid.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/get-token.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/get-weather.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/send-query.R")
# source("https://raw.githubusercontent.com/yizhexu/weatherawhere/master/R/set-options.R")
#

calculate_index <- function(start_date, end_date, data) {
  start <- as.Date(data[, 4], "%Y-%m-%d") == start_date
  end <- as.Date(data[, 4], "%Y-%m-%d") == end_date
  new_data <- data[end == TRUE, 5:6, drop = FALSE] - data[start == TRUE, 5:6, drop = FALSE]
  new_data[, "dailyAttributes.accPrecip", drop = TRUE] / new_data[, "dailyAttributes.accPet", drop = TRUE]
}

#### query data ####

latitude = 40.7127
longitude = -74.0059

x = 10
y = 15

start_date_baseline <- "2014-05-15"
end_date_baseline <- "2014-09-15"
start_date_target <- "2015-05-15"
end_date_target <- "2015-09-15"

attribute <- c("accPrecip", "accPet")
query_baseline <- create_query(latitude, longitude, x, y, size = 1, date = set_date(start_date_baseline, end_date_baseline), attribute = attribute)
query_target <- create_query(latitude, longitude, x, y, size = 1, date = set_date(start_date_target, end_date_target), attribute = attribute)

get_token("yizhexu@awhere.com", "181225tiancai@X")

baseline <- get_weather(token, query_baseline)
target <- get_weather(token, query_target)
save(baseline, file = "baseline.RData")
save(target, file = "target.RData")

#### load data ####

load("baseline.RData")
load("target.RData")
calculate_index <- function(start_date, end_date, data) {
  start <- as.Date(data[, 4], "%Y-%m-%d") == start_date
  end <- as.Date(data[, 4], "%Y-%m-%d") == end_date
  new_data <- data[end == TRUE, 5:6, drop = FALSE] - data[start == TRUE, 5:6, drop = FALSE]
  new_data[, "dailyAttributes.accPrecip", drop = TRUE] / new_data[, "dailyAttributes.accPet", drop = TRUE]
}

calculate_coor <- function(start_date, data) {
  start <- as.Date(data[, 4], "%Y-%m-%d") == start_date
  data[start == TRUE, 2:3, drop = FALSE]
}

start_date_baseline <- "2014-05-15"
end_date_baseline <- "2014-09-15"
start_date_target <- "2015-05-15"
end_date_target <- "2015-09-15"

baseline_index <- calculate_index(start_date_baseline, end_date_baseline, baseline)
target_index <- calculate_index(start_date_target, end_date_target, target)

coors <- calculate_coor(end_date_baseline, baseline)

# try for multiple
create_poly <- function(latitude, longitude, x, size = 1) {
  lat_m <- latitude[x] - size/2/60
  lat_p <- latitude[x] + size/2/60
  lng_m <- longitude[x] - size/2/60
  lng_p <- longitude[x] + size/2/60

  p <- Polygon(matrix(c(lng_m, lat_m, lng_p, lat_m, lng_p, lat_p, lng_m, lat_p), ncol = 2, byrow = TRUE))
  Polygons(list(p),x)
}

list <- lapply(1:150, function(i) {
  create_poly(coors$latitude, coors$longitude, i)}
)

sps = SpatialPolygons(list)
data = data.frame(index = baseline_index, row.names = row.names(sps))
spdf = SpatialPolygonsDataFrame(sps, data)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = spdf, color = colorQuantile("RdYlBu", baseline_index)(baseline_index), stroke = FALSE)

#### working: rectangle

create_rectangle <- function(coors, x) {
  lat_m <- coors$latitude[x] - 0.5/60
  lat_p <- coors$latitude[x] + 0.5/60
  lng_m <- coors$longitude[x] - 0.5/60
  lng_p <- coors$longitude[x] + 0.5/60
  c(lng_m, lat_p, lng_p, lat_m)
}

coors <- calculate_coor(start_date_baseline, baseline)

list <- do.call(rbind, lapply(1:150, function(i) {
  create_rectangle(coors, i)}
))

leaflet() %>%
  addTiles() %>%
  addRectangles(lng1 = list[, 1], lat1 = list[, 2],
                lng2 = list[, 3], lat2 = list[, 4],
                fillColor = colorQuantile("RdYlBu", baseline_index)(baseline_index),stroke = FALSE)

# heat map method
# Add heat map : fail
source("https://raw.githubusercontent.com/timelyportfolio/leaflet/feature/heatmap/R/plugin-heatmap.R")
source("https://raw.githubusercontent.com/timelyportfolio/leaflet/feature/heatmap/R/plugin-measure.R")
source("https://raw.githubusercontent.com/timelyportfolio/leaflet/feature/heatmap/R/plugin-omnivore.R")
source("https://raw.githubusercontent.com/timelyportfolio/leaflet/feature/heatmap/R/plugin-providers.R")
source("https://raw.githubusercontent.com/timelyportfolio/leaflet/feature/heatmap/R/normalize.R")
source("https://raw.githubusercontent.com/rstudio/leaflet/4ef0023c9fefa00a64e382ccd77d34d1413c47dc/R/leaflet.R")
source("https://raw.githubusercontent.com/rstudio/leaflet/a165125d952681680e6c65d2ce6f1ee42ed985da/R/layers.R")
data <- data.frame( cbind(coors, baseline_index) )

addressPoints <- readLines(
  "http://leaflet.github.io/Leaflet.markercluster/example/realworld.10000.js"
)

addressPoints <- apply(
  jsonlite::fromJSON(
    sprintf("[%s]",
            paste0(
              addressPoints[4:(length(addressPoints)-1)]
              ,collapse=""
            )
    )
  )
  ,MARGIN = 2
  ,as.numeric
)

addressPoints <- data.frame( addressPoints )
colnames( addressPoints ) <- c( "lat", "lng", "value" )


leaflet() %>%
  addTiles() %>%
  addHeatmap(
    data = addressPoints,
    intensity = ~value,
    blur = 50,
    gradient = "Purples")

#### reproduce ####
source("https://gist.githubusercontent.com/yizhexu/a0f5b4806061005c2d93/raw/931e3da166e3bdc464dcb282ec2472705391f9d1/data.R")
