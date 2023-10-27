#' Binance Server Time
#'
#' Get the current server time from Binance API. 
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#check-server-time).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#check-server-time).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#check-server-time).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#check-server-time).
#'
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#'
#' @return A \code{\link[=POSIXt-class]{POSIXt}} object. The server time for the reference API.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#' 
#' @usage 
#' binance_time(api, quiet = FALSE)
#' 
#' @examples
#'
#' # Get the server time
#' binance_time("spot")
#' binance_time("fapi")
#' binance_time("dapi")
#' binance_time("eapi")
#'
#' @export
#'
#' @rdname binance_time
#' @name binance_time

binance_time <- function(api, quiet = FALSE){
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } 
  
  # GET call
  response <- binance_api(api = api, path = "time", query = NULL, quiet = quiet)

  if (purrr::is_empty(response)) {
    response <- ""
  } else {
    response <- as.POSIXct(response$serverTime/1000, origin = "1970-01-01")
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- 1
  attr(response, "endpoint") <- "time"
  
  return(response)
}
