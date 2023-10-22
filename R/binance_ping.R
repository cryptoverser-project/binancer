#' Test Connectivity to Binance API
#'
#' Check the connectivity to the Binance API to ensure it is operational. 
#' This function is useful for verifying that the API is accessible and responsive.
#'
#' @param api Character, reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#test-connectivity).
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#test-connectivity).
#'   - "dapi": For [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#test-connectivity).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#test-connectivity).
#'
#' @return Logical, `TRUE` denote a successful connection, while `FALSE` denote a failed connection.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#' 
#' # Test the connection to the Spot API.
#' binance_ping("spot")
#'
#' # Test the connection to the Futures USD-M API.
#' binance_ping("fapi")
#'
#' # Test the connection to the Futures Coin-M API.
#' binance_ping("dapi")
#'
#' # Test the connection to the Options API.
#' binance_ping("eapi")
#'
#' @export
#'
#' @rdname binance_ping
#' @name binance_ping

binance_ping <- function(api = "spot"){
  
  response <- binance_api(api = api, path = "ping", query = NULL)
  
  if (purrr::is_empty(response)) {
    response <- TRUE
  } else {
    response <- FALSE
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  return(response)
}


