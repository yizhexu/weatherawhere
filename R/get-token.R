get_token <- function(consumer_key, consumer_secret) {
  # the function send authenticate, http header and body to awhere
  # input of this function is api_key, and api_secret
  # the function will give api access expiration time and an api token
  url_token <- "https://api.awhere.com/oauth/token"

  body <- list(grant_type = "client_credentials")

  response <- httr::POST(url_token, httr::verbose(),
                         httr::authenticate(consumer_key, consumer_secret, type = "basic"),
                         encode = "form",
                         body = body)

  expire_time <<- Sys.time() + httr::content(response)$expires_in

  cat(ifelse(response$status != 200,
             paste("API call failed. \nStatus", response$status),
             paste("API call success! \nAPI Access token will expire at:", expire_time, Sys.timezone(),"\n")))

  Token <<- paste0("Bearer ", httr::content(response)$access_token)

  Token

}
