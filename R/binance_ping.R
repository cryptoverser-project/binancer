#' Test Connectivity to Binance API
#'
#' Check the connectivity to the Binance API to ensure it is operational. 
#' This function is useful for verifying that the API is accessible and responsive.
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#test-connectivity).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#test-connectivity).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#test-connectivity).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#test-connectivity).
#'
#' @return Logical, with `TRUE` indicating a successful connection and `FALSE` indicating a failed connection.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Example: Test the connection to the Spot API.
#' binance_ping("spot")
#'
#' # Example: Test the connection to the Futures USD-M API.
#' binance_ping("fapi")
#'
#' # Example: Test the connection to the Futures Coin-M API.
#' binance_ping("dapi")
#'
#' # Example: Test the connection to the Options API.
#' binance_ping("eapi")
#'
#' @export
#'
#' @rdname binance_ping
#'
#' @name binance_ping

binance_ping <- function(api = "spot"){
  
  response <- NULL
  response <- binance_api(api = api, path = c("ping"), query = NULL)
  response <- ifelse(purrr::is_empty(response), TRUE, FALSE)
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  return(response)
}


