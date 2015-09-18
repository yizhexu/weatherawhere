library(httr)
library(jsonlite)
library(magrittr)
library(doParallel)
library(foreach)
library(lubridate)
library(devtools)

devtools::source_url("https://gist.github.com/yizhexu/1ac57e64f309fd16d2e2/raw/1223e9397a0e7595d0b033d2043fd2b888fc566a/get-token.R")
devtools::source_url("https://gist.github.com/yizhexu/1ac57e64f309fd16d2e2/raw/1223e9397a0e7595d0b033d2043fd2b888fc566a/get-grid.R")
devtools::source_url("https://gist.githubusercontent.com/yizhexu/1ac57e64f309fd16d2e2/raw/bb23f5e9fc5d37afb7f7052421191526f5cc1d5a/get-weather.R")
devtools::source_url("https://gist.github.com/yizhexu/1ac57e64f309fd16d2e2/raw/1223e9397a0e7595d0b033d2043fd2b888fc566a/grid-weather.R")
devtools::source_url("https://gist.github.com/yizhexu/1ac57e64f309fd16d2e2/raw/1223e9397a0e7595d0b033d2043fd2b888fc566a/observed-weather.R")
devtools::source_url("https://gist.github.com/yizhexu/1ac57e64f309fd16d2e2/raw/1223e9397a0e7595d0b033d2043fd2b888fc566a/check-input.R")

sessionInfo()

get_token("yizhexu@awhere.com", "181225tiancai@X")

latitude = 40
longitude = -105
x = 5
y = 5
start_date = "2015-05-15"
end_date = "2015-09-15"


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

mb <- microbenchmark(query_forloop(strquery, token), query_lapply(strquery, token), query_par(strquery, token), times = 100L)

save(mb, file = "mb-ht-off.RData")

mb <- cbind(mb, ht = rep("on"))
mb <- cbind(mb, console = rep(FALSE))
mb_all <- rbind(mb_all, mb)

mb_all$expr <- plyr::revalue(mb_all$expr, c("query_forloop(strquery, token)" = "forloop", "query_lapply(strquery, token)" = "lapply", "query_par(strquery, token)" = "par"))

library(ggplot2)
ggplot(mb_all, aes(x = expr, y = time)) + geom_violin() + facet_grid(~ht+console)

ggplot(mb_all, aes(x = ht, y = time)) + geom_violin() + facet_grid(~expr+console)


#### weather query ####
latitude = 40
longitude = -105

x = 5
y = 5

start_date = "2015-05-15"
end_date = "2015-09-15"

# plant_date <- "2013-05-01"

get_attribute(c("^acc+pet$"))

get_token("yizhexu@awhere.com", "181225tiancai@X")

query <- create_query(latitude, longitude, date = set_date(start_date, plant_date = plant_date), attribute = get_attribute(c("^acc", "gdd$")), gdd_method = set_gdd(gdd_method = "modifiedstandard"))

query <- create_query(latitude, longitude, date = set_date(start_date), attribute = get_attribute(c("^acc", "gdd$")), gdd_method = set_gdd(gdd_method = "modifiedstandard"))

get_weather(token, query)
