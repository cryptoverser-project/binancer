#' Binance Live Depth 
#' 
#' Live depth stream for a trading pair. 
#' 
#' @param pair Character, trading pair, e.g. `"BTCUSDT"`.
#' 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#diff-depth-stream).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#diff-book-depth-streams).
#'   
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @export
#' 
#' @name binance_live_depth
#' @rdname binance_live_depth

binance_live_depth <- function(pair, api, quiet = FALSE){
  
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
  
  # Initial snapshot of the order book
  response <- binance_depth(api = api, pair = pair, quiet = quiet)

  # Function arguments 
  args <- list(pair = pair, api = api, subscription = "depth", interval = NULL)
  # Create a websocket connection
  ws <- do.call(binance_ws_socket, args = args)
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


#' Binance Live Klines 
#' 
#' Live klines stream for a trading pair. 
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-streams).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-streams).
#'   
#' @param interval Character. Time interval for Klines data. 
#' If it is `missing`, the default, will be used as start date `"1m"`. Available intervals are: 
#'   - Secondly: `"1s"`, available only if `api = "spot"`.
#'   - Minutely: `"1m"`, `"3m"`, `"5m"`, `"15m"` and `"30m"`.
#'   - Hourly: `"1h"`, `"2h"`, `"4h"`, `"6h"`, `"8h"` and `"12h"`.
#'   - Daily: `"1d"` and `"3d"`.
#'   - Weekly: `"1w"`.
#'   - Monthly: `"1M"`.
#'   
#' @param data initial data
#' 
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @export
#' 
#' @name binance_live_klines
#' @rdname binance_live_klines

binance_live_klines <- function(pair, api, interval, data = NULL, quiet = FALSE){

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
    api <- match.arg(api, choices = c("spot", "fapi"))
  }
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- "1m"
    if (!quiet) {
      wrn <- paste0('The `interval` argument is missing, default is ', '"', interval, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    av_int <- c("1s", "1m", "3m", "5m", "15m","30m","1h", "2h", 
                "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
    if (api != "spot"){
      av_int <- av_int[-1] 
    }
    interval  <- match.arg(interval, choices = av_int)
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
  
  # Function arguments 
  args <- list(pair = pair, api = api, subscription = "kline", interval = interval)
  # Create a websocket connection
  ws <- do.call(binance_ws_socket, args = args)
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


#' Binance Live Trades 
#' 
#' Live trades stream for a trading pair. 
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#aggregate-trade-streams).
#'   - `"fapi"`: for [futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#aggregate-trade-streams).
#'   
#' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical data. 
#' If it is `missing`, the default, will be used as start date `Sys.time()-lubridate::minutes(10)`.
#' 
#' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical data.
#' If it is `missing`, the default, will be used as end date \code{\link[=Sys.time]{Sys.time()}}.
#' 
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @export
#' 
#' @name binance_live_trades
#' @rdname binance_live_trades

binance_live_trades <- function(pair, api, from, to, quiet = FALSE){

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
    api <- match.arg(api, choices = c("spot", "fapi"))
  }
  
  # Check "from" argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::minutes(10)
    if (!quiet) {
      wrn <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    from <- as.POSIXct(from, origin = "1970-01-01")
  }
  
  # Check "to" argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.time() 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    to <- as.POSIXct(to, origin = "1970-01-01")
  }
  
  # Initial snapshot of trades of last 10 minutes 
  response <- binance_trades(pair = pair, api = api, from = from, to = to, quiet = quiet)

  # Function arguments 
  args <- list(pair = pair, api = api, subscription = "aggTrade", interval = NULL)
  # Create a websocket connection
  ws <- do.call(binance_ws_socket, args = args)
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

