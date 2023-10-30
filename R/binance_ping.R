#' Ping to Binance REST API
#'
#' Check the connection to the Binance API. 
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#test-connectivity).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#test-connectivity).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#test-connectivity).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#test-connectivity).
#'
#' @return A logical value. It is `TRUE` if the connection was successful, otherwise it is `FALSE`.
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
  
  response <- binance_query(api = api, path = "ping", query = NULL)
  
  if (purrr::is_empty(response)) {
    response <- TRUE
  } else {
    response <- FALSE
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  
  return(response)
}


