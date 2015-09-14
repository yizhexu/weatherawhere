date_range <- function(start_date, end_date) {

  start <- as.Date(start_date, "%Y-%m-%d")
  end <-  as.Date(end_date, "%Y-%m-%d")

  loop <- c( 1: (year(end) - year(start)))

  list(start_days = c(start, start + days(365) * loop + 1),
       end_days = c(start + days(365) * (loop[1:( length(loop))]), end))
}
