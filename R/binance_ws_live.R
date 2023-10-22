#' binance_live_depth
#' @name binance_live_depth
#' @rdname binance_live_depth
#' @description live orderbook
#' @param pair Character, the trading pair of interest, e.g., "BTCUSDT".
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#' @param quiet Logical, suppress informational messages if `TRUE`. Default `FALSE`.
#' @export

binance_live_depth <- function(pair = "BTCUSDT", api, quiet = FALSE){
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi"))
  }
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }

  # Initial snapshot of the order book
  response <- binance_depth(api = api, pair = pair, quiet = quiet)

  # Function name and arguments 
  fun_name <- paste0("binance_ws_", api, "_socket")
  args <- list(pair = pair, subscription = "depth", interval = NULL)
  # Create a websocket connection
  ws <- do.call(fun_name, args = args)
  # Update the order book in stream data
  ws$.__enclos_env__$stream$data <- response
  ws$connect()

  structure(
    list(
      Close = ws$close,
      Data = function() ws$.__enclos_env__$stream$data
    )
  )
}

#' binance_live_klines
#' @name binance_live_klines
#' @rdname binance_live_klines
#' @description live candlestick
#' @param pair Character, the trading pair of interest, e.g., "BTCUSDT".
#' @param interval Character, the time interval for Klines data. 
#' Acceptable intervals include "1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", and "1M".
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#' @param data initial data
#' @param quiet Logical, suppress informational messages if `TRUE`. Default `FALSE`.
#' @export

binance_live_klines <- function(pair = "BTCUSDT", interval = "1m", api, data = NULL, quiet = FALSE){

  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi"))
  }
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }

  # Initialize with klines from last 24 hours
  sys_time <- Sys.time() 
  if (is.null(data)) {
    response <- binance_klines(
      api = api, pair = pair, interval = interval, from = sys_time - lubridate::hours(24), to = sys_time, quiet = quiet
      )
  } else {
    # Update till last date
    new_data <- binance_klines(
      api = api, pair = pair, interval = interval, from = max(data$date), to = sys_time, quiet = quiet
      )
    response <- dplyr::bind_rows(new_data, data)
    response <- response[!duplicated(response$date),]
    response <- dplyr::mutate(response, is_closed = TRUE)
  }
  
  # Function name and arguments 
  fun_name <- paste0("binance_ws_", api, "_socket")
  args <- list(pair = pair, subscription = "kline", interval = interval)
  # Create a websocket connection
  ws <- do.call(fun_name, args = args)
  # Update the klines in stream data
  response$is_closed <- TRUE
  ws$.__enclos_env__$stream$data <- response
  ws$connect()

  structure(
    list(
      Close = ws$close,
      Data = function() ws$.__enclos_env__$stream$data
    )
  )
}

#' binance_live_trades
#' @name binance_live_trades
#' @rdname binance_live_trades
#' @description live trades
#' @param pair Character, the trading pair of interest, e.g., "BTCUSDT".
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#' @param quiet Logical, suppress informational messages if `TRUE`. Default `FALSE`.
#' @export

binance_live_trades <- function(pair = "BTCUSDT", api, quiet = FALSE){

  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi"))
  }
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Initial snapshot of trades of last 10 minutes 
  response <- binance_trades(pair = pair, api = api, quiet = quiet)

  # Function name and arguments 
  fun_name <- paste0("binance_ws_", api, "_socket")
  args <- list(pair = pair, subscription = "aggTrade", interval = NULL)
  # Create a websocket connection
  ws <- do.call(fun_name, args = args)
  # Update the trades in stream data
  ws$.__enclos_env__$stream$data <- response
  ws$connect()

  structure(
    list(
      Close = ws$close,
      Data = function() ws$.__enclos_env__$stream$data
    )
  )
}

