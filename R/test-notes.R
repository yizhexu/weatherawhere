library(httr)
library(jsonlite)
library(magrittr)
library(doParallel)
library(foreach)
library(lubridate)

source("./R/get-token.R")
source("./R/get-grid.R")
source("./R/get-weather.R")
source("./R/grid-weather.R")
source("./R/observed-weather.R")
source("./R/check-input.R")

get_token("yizhexu@awhere.com", "181225tiancai@X")

latitude = 40
longitude = -105
x = 5
y = 5
start_date = "2015-08-27"

check_input(token, start_date)

grid <- get_grid(latitude, longitude, x, y)

strquery <- outer(1:x, 1:y, Vectorize(function(x, y) {
  paste(c("latitude", "longitude", "startDate"),
        c(grid$latitudes[x], grid$longitudes[y], start_date),
        sep = "=", collapse = "&")
}))

#### bench marking multiple api calls ####

# the for loop way
query_forloop <- function(strquery, token) {
  data <- data.frame()
  for (i in 1:dim(strquery)[1]) {
    for (j in 1:dim(strquery)[2]) {
      d <- get_weather(token, strquery[i, j])
      data <- rbind(data, d)
    }
  }
}

# the lapply way
query_lapply <- function(strquery, token){
  Reduce(rbind,lapply(1:dim(strquery)[1], function(i) {
    Reduce(rbind, lapply(1:dim(strquery)[2], function(j) {
      get_weather(token, strquery[i, j])
    }))
  }))
}

# the parallel way
registerDoParallel( detectCores() )
query_par <- function(strquery, token) {
  foreach(i = 1:dim(strquery)[1], .combine = 'rbind')  %:%
    foreach(j = 1:dim(strquery)[2], .combine = 'rbind', .export = "get_weather") %dopar% {
      get_weather(token, strquery[i, j])
    }
}


library(microbenchmark)

mb <- microbenchmark(query_forloop(strquery, token), query_lapply(strquery, token), query_par(strquery, token), times = 20L, unit = "relative")

print(mb)
boxplot(mb)
autoplot(mb)
ggplot2::qplot(y = time, data = mb, color = expr) + scale_y_log10()
