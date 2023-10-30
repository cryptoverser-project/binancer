#' Binance Market Information
#'
#' Obtain market information and available trading pairs.
#'
#' @param pair Character, optional. Trading pair, e.g. `"BTCUSDT"`. If `NULL`, the default, all available pairs will be returned.
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#exchange-information).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#exchange-information).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#exchange-information).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#exchange-information).
#'
#' @param permissions Character, optional. Used only if `api = "spot"`. Types of trading pairs to return. 
#' If `"all"`, the default, all available types of pairs will be returned. Available options are:
#'   - `"all"`: trading pairs in all markets.
#'   - `"spot"`: trading pairs only in spot markets.
#'   - `"margin"`: trading pairs only in margin markets.
#'   - `"leveraged"`: trading pairs only in leveraged markets.
#'   
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#'
#' @return A \code{\link[=data.frame-class]{data.frame}} with trading pairs and others information.
#'
#' @details The IP weight for this API call is 20 if `api = "spot"` while 1 for others `api`, The data source is memory.
#' 
#' @usage 
#' binance_exchange_info(pair = NULL, 
#'                       api, 
#'                       permissions = "all", 
#'                       quiet = FALSE)
#'
#' @examples
#'
#' # Get all pairs in all markets
#' binance_exchange_info(api = "spot", permissions = "all", pair = NULL)
#'
#' # Get all pairs only in spot markets
#' binance_exchange_info(api = "spot", permissions = "spot", pair = NULL)
#'
#' # Get all pairs only in margin markets
#' binance_exchange_info(api = "spot", permissions = "margin", pair = NULL)
#'
#' # Get all pairs only in leveraged market
#' binance_exchange_info(api = "spot", permissions = "leveraged", pair = NULL)
#'
#' # Get information only for BTCUSDT in all markets
#' binance_exchange_info(api = "spot", permissions = "all", pair = "BTCUSDT")
#'
#' # Get information for multiple pairs in all markets
#' binance_exchange_info(api = "spot", 
#'                       permissions = "all", 
#'                       pair = c("BTCUSDT", "BNBUSDT"))
#'
#' # Get information for multiple pairs only in margin and leveraged markets
#' binance_exchange_info(api = "spot", 
#'                       permissions = c("margin", "leveraged"), 
#'                       pair = c("BTCBUSD", "ETHBUSD"))
#'                       
#' # Get all pairs in futures USD-m markets
#' binance_exchange_info(api = "fapi")
#'
#' # Get all pairs in futures COIN-m markets
#' binance_exchange_info(api = "dapi")
#'
#' # Get all pairs in options markets
#' binance_exchange_info(api = "eapi")
#'
#' @export
#'
#' @rdname binance_exchange_info
#' @name binance_exchange_info

binance_exchange_info <- function(api, permissions, pair, quiet = FALSE){
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  query <- list(permissions = NULL)
  # Check "permissions" argument 
  if (missing(permissions) || is.null(permissions)) {
    if (api == "spot") {
      permissions <- "all"
      if (!quiet) {
        msg <- paste0('The `permissions` argument is missing, default is ', '"', permissions, '"')
        cli::cli_alert_warning(msg)
      }
      query$permissions <- NULL
    } 
  } else {
    permissions <- tolower(permissions)
    # Multiple `permissions` are allowed
    if (sum(permissions %in% "all") == 0) {
      permissions <- match.arg(permissions, choices = c("spot", "margin", "leveraged"), several.ok = TRUE)
      if (length(permissions) > 1) {
        permissions <- purrr::map_chr(permissions, ~paste0('"', .x, '"' ))
        permissions <- paste0('[', paste0(permissions, collapse = ","), ']')
      }
      query$permissions <- toupper(permissions)
    }
  }
  
  # GET call
  response <- binance_query(api = api, path = "exchangeInfo", query = query, quiet = quiet)
  
  if(api == "eapi") {
    data <- dplyr::as_tibble(response$optionSymbols)
    data <- dplyr::mutate(data, expiryDate = as.POSIXct(expiryDate/1000, origin = "1970-01-01"))
    data <- dplyr::arrange(data, expiryDate)
  } else {
    data <- dplyr::as_tibble(response$symbols)
  }
  
  # Filter only if a pair is specified (pair is not NULL)
  if(!missing(pair) && !is.null(pair)){
    pair <- toupper(pair)
    if(length(pair) >= 1){
      data <- dplyr::filter(data, symbol %in% pair)
    }
  }
  return(data)
}
