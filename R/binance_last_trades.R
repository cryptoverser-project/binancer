#' Retrieve Recent Trades Data
#'
#' Obtain the most recent 1000 trades for a specified trading pair. This function allows you to access the latest trades that have occurred in a specific market.
#'
#' @param pair Character, specifying the trading pair of interest, e.g., "BTCUSDT".
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#recent-trades-list).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#recent-trades-list).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#recent-trades-list).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#recent-trades-list).
#'
#' @param quiet Logical, specifying whether to suppress informational messages. Default is FALSE.
#'
#' @return A tibble (data frame) with the following 7 columns: 
#'   - `trade_id`: Column containing the trade ID.
#'   - `date`: Datetime, representing the trade execution date.
#'   - `pair`: Column specifying the selected trading pair.
#'   - `market`: Column indicating the selected API.
#'   - `price`: Column showing the trade price.
#'   - `quantity`: Column representing the trade quantity.
#'   - `side`: Character, indicating whether the trade was a "SELL" or "BUY."
#'
#' @examples
#'
#' # Example: Get the most recent trades for the BTCUSDT trading pair in the Spot market.
#' binance_last_trades(pair = "BTCUSDT", api = "spot")
#'
#' # Example: Get the most recent trades for the BTCUSDT trading pair in the Futures USD-M market.
#' binance_last_trades(pair = "BTCUSDT", api = "fapi")
#'
#' # Example: Get the most recent trades for the BTCUSD_PERP trading pair in the Futures COIN-M market.
#' binance_last_trades(pair = "BTCUSD_PERP", api = "dapi")
#'
#' @export
#'
#' @rdname binance_last_trades
#'
#' @name binance_last_trades
#'

binance_last_trades <- function(pair, api = "spot", quiet = FALSE){

  api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  
  # Check: default argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      warning(wrn)
    }
  } else {
    pair_name <- toupper(pair)
  }
  
  # function name 
  fun_name <- "binance_api_last_trades"
  # safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = list(pair = pair_name, api = api, quiet = quiet)))
  # api GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    warning(response$error)
  } else {
    return(response$result)
  }
}

# api function 
binance_api_last_trades <- function(pair, api = "spot", quiet = FALSE){
  
  api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  
  # Check: "pair" argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      warning(wrn)
    }
  } else {
    pair_name <- toupper(pair)
  }
  
  # api GET call 
  response <- binance_api(api = api, path = c("trades"), query = list(symbol = pair, limit = 1000))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::tibble(
      trade_id = response$id,
      date = as.POSIXct(response$time/1000, origin = "1970-01-01"),
      pair = pair,
      market = api,
      price = as.numeric(response$price),
      quantity = as.numeric(response$qty),
      side = ifelse(response$isBuyerMaker, "SELL", "BUY"))
  } 
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- ifelse(api == "spot", 1, 5)
  return(response)
}


