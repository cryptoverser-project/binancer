#' Binance Server Time
#'
#' Obtain the current server time. 
#'
#' @param api Character, reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#check-server-time).
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#check-server-time).
#'   - "dapi": For [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#check-server-time).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#check-server-time).
#'
#' @return An object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the server's time for the reference API.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#' 
#' @usage 
#' binance_time(api)
#' 
#' @examples
#'
#' # Get the server time for the Spot API.
#' binance_time("spot")
#'
#' # Get the server time for the Futures USD-M API.
#' binance_time("fapi")
#'
#' # Get the server time for the Futures Coin-M API.
#' binance_time("dapi")
#'
#' # Get the server time for the Options API.
#' binance_time("eapi")
#'
#' @export
#'
#' @rdname binance_time
#' @name binance_time

binance_time <- function(api){
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } 
  
  # GET call
  response <- binance_api(api = api, path = "time", query = NULL)

  if (purrr::is_empty(response)) {
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  
  return(response)
}
