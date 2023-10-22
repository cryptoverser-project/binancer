#' Retrieve Recent Trades Data
#'
#' Get the most recent 1000 trades for a specified trading pair.
#'
#' @param pair Character, trading pair, e.g. "BTCUSDT".
#'
#' @param api Character, reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#recent-trades-list).
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#recent-trades-list).
#'   - "dapi": For [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#recent-trades-list).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#recent-trades-list).
#'
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`.
#'
#' @return A tibble with 7 columns:
#'   - `date`: Datetime, trade execution date.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, selected pair.
#'   - `price`: Numeric, trade price.
#'   - `quantity`: Numeric, trade quantity.
#'   - `side`: Character, trade side. Can be "SELL" or "BUY".
#'   - `trade_id`: Integer, trade Id.
#'
#' @examples
#'
#' # Get the last 1000 trades for BTCUSDT in spot market
#' binance_last_trades(pair = "BTCUSDT", api = "spot")
#'
#' # Get the last 1000 trades for BTCUSDT in USD-M market
#' binance_last_trades(pair = "BTCUSDT", api = "fapi")
#'
#' # Get the last 1000 trades for BTCUSDT in COIN-M market
#' binance_last_trades(pair = "BTCUSD_PERP", api = "dapi")
#' 
#' # Get the last 1000 trades for BTCUSDT in COIN-M market
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
      wrn <- paste0('The pair argument is missing with no default.')
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
    cli::cli_abort(response$error)
  } else {
    return(response$result)
  }
}

# Depth implementation for all APIs
binance_api_last_trades <- function(pair, api){
  
  # GET call 
  response <- binance_api(api = api, path = c("trades"), query = list(symbol = pair, limit = 1000))
  
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
  return(response)
}