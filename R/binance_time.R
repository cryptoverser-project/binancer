#' Retrieve Binance Server Time
#'
#' Test the connectivity to the Binance REST API and obtain the current server time. This function allows you to verify the connection to the API and retrieve the server's time.
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#check-server-time).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#check-server-time).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#check-server-time).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#check-server-time).
#'
#' @return An object of class \code{"\link[=POSIXt-class]{POSIXt}"} representing the server's time for the selected API.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Example: Get the server time for the Spot API.
#' binance_time("spot")
#'
#' # Example: Get the server time for the Futures USD-M API.
#' binance_time("fapi")
#'
#' # Example: Get the server time for the Futures Coin-M API.
#' binance_time("dapi")
#'
#' # Example: Get the server time for the Options API.
#' binance_time("eapi")
#'
#' @export
#'
#' @rdname binance_time
#'
#' @name binance_time

binance_time <- function(api = "spot"){

  response <- NULL
  response <- binance_api(api = api, path = c("time"), query = NULL)

  if (is.null(response)) {
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  
  return(response)
  
}
