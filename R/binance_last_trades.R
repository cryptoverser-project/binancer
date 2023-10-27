#' Retrieve Recent Trades Data
#'
#' Get the last 1000 trades for a trading pair.
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#recent-trades-list).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#recent-trades-list).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#recent-trades-list).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#recent-trades-list).
#'
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @usage 
#' binance_last_trades(pair, 
#'                     api, 
#'                     quiet = FALSE)
#'
#' @return A \code{\link[=data.frame-class]{data.frame}} with 7 columns:
#'   - `date`: \code{"\link[=POSIXt-class]{POSIXt}"}, trade execution date.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, trading pair.
#'   - `price`: Numeric, trade price.
#'   - `quantity`: Numeric, trade quantity.
#'   - `side`: Character, trade side. Can be `"BUY"` or `"SELL"`.
#'   - `trade_id`: Integer, trade id.
#'
#' @examples
#'
#' # Get last 1000 trades for BTCUSDT
#' binance_last_trades(pair = "BTCUSDT", api = "spot")
#' binance_last_trades(pair = "BTCUSDT", api = "fapi")
#'
#' # Get last 1000 trades for BTCUSD_PERP
#' binance_last_trades(pair = "BTCUSD_PERP", api = "dapi")
#' 
#' # Get last 1000 trades for a put option on BTC
#' binance_last_trades(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @export
#' 
#' @rdname binance_last_trades
#' @name binance_last_trades

binance_last_trades <- function(pair, api, quiet = FALSE){

  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      wrn <- paste0('The pair argument is missing with no default')
      cli::cli_abort(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
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
  
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(binance_api_last_trades, args = list(pair = pair, api = api)))
  # GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_danger(response$error)
  } else {
    return(response$result)
  }
}

# trades implementation for all APIs
binance_api_last_trades <- function(pair, api){
  
  # GET call 
  response <- binance_api(api = api, path = "trades", query = list(symbol = pair, limit = 1000))
  
  if (!purrr::is_empty(response) & api != "eapi") {
    response <- dplyr::tibble(
      date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      market = api,
      pair = pair,
      price = as.numeric(response$price),
      quantity = as.numeric(response$qty),
      side = ifelse(response$isBuyerMaker, "SELL", "BUY"),
      trade_id = response$id)
  } else {
    response <- dplyr::tibble(
      date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      market = api,
      pair = response$symbol,
      price = as.numeric(response$price),
      quantity = as.numeric(response$qty),
      side = response$side, 
      trade_id = response$id)
    response <- dplyr::mutate(response, side = ifelse(side == -1, "SELL", "BUY"))
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- ifelse(api == "spot", 1, 5)
  attr(response, "endpoint") <- "trades"
  
  return(response)
}