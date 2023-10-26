#' Test Connectivity to Binance REST API
#'
#' Check the connectivity to the Binance API to ensure it is working. 
#'
#' @param api Character, reference API. Available options are:
#'   - `"spot"`: for [Spot API](https://binance-docs.github.io/apidocs/spot/en/#test-connectivity).
#'   - `"fapi"`: for [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#test-connectivity).
#'   - `"dapi"`: for [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#test-connectivity).
#'   - `"eapi"`: for [Options API](https://binance-docs.github.io/apidocs/voptions/en/#test-connectivity).
#'
#' @return `TRUE` if the connection was successful, otherwise `FALSE` if the connection failed.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#' 
#' @usage 
#' binance_ping(api)
#' 
#' @examples
#' 
#' # Test connection to Spot API
#' binance_ping("spot")
#'
#' # Test connection to Futures USD-m API
#' binance_ping("fapi")
#'
#' # Test connection to Futures Coin-m API
#' binance_ping("dapi")
#'
#' # Test connection to Options API
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


