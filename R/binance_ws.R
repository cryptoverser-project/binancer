#' Binance Websocket in R
#' 
#' Create a connection to Binance websocket API in R.
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#kline-candlestick-data).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#kline-candlestick-data).
#'   
#' @param subscription Character
#' @param interval Character, the time interval for Klines data. 
#' Acceptable intervals are "1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", and "1M".
#' @param contract_type Character, only used if `api = "dapi"`. Choose from:
#'   - `"perpetual"`: For perpetual futures.
#'   - `"current_quarter"`: For futures with a maturity in the current quarter.
#'   - `"next_quarter"`: For futures with a maturity in the next quarter.
#' @param update_speed time in milliseconds 
#' @param nobs_max Integer, maximum number of observations to save. 
#' @param quiet Logical, suppress informational messages if TRUE.
#' @export
#' 
#' @name binance_ws_socket
#' @rdname binance_ws_socket

binance_ws_socket <- function(pair = "BTCUSDT", api = "spot", subscription = "kline", interval = "1m", contract_type = NULL, update_speed = 1000, nobs_max = NULL, quiet = FALSE){
  
  # Url parameters depends on api 
  if (api == "spot") {
    args <- list(pair = pair, subscription = subscription, interval = interval, update_speed = update_speed, quiet = quiet)
  } else if (api == "fapi") {
    args <- list(pair = pair, subscription = subscription, interval = interval, contract_type = contract_type, update_speed = update_speed, quiet = quiet)
  } 
  
  # Function name 
  fun_name <- paste0("binance_ws_", api, "_url")
  # Create the subscription url
  websocket_url <- do.call(fun_name, args = args)
  # Initialize a websocket 
  ws <- websocket::WebSocket$new(websocket_url, autoConnect = FALSE)
  # Initialize a list to save the stream data
  ws$.__enclos_env__[["stream"]] <- list()
  # Initialize an empty tibble for stream data
  ws$.__enclos_env__[["stream"]][["data"]] <- dplyr::tibble()
  # Initialize a tibble for other information
  ws$.__enclos_env__[["stream"]][["info"]] <- dplyr::tibble(date_open = NA_character_,
                                                            date_close = NA_character_,
                                                            api = api,
                                                            pair = pair,
                                                            subscription = subscription,
                                                            interval = interval,
                                                            update_speed = update_speed,
                                                            nobs = 0)
  
  # onOpen: when the connection is opened 
  ws$onOpen(function(event) {
    
    # save the time when connection is opened
    ws$.__enclos_env__[["stream"]][["info"]]$date_open <- Sys.time()
    
    # verbose message 
    if (!quiet) {
      msg <- paste0("Stream: ", pair, "@", subscription, " opened!")
      message(msg)
    }
  })
  
  # onMessages: when a message is received  
  ws$onMessage(function(event) {
    
    # retrieve the saved data
    old_data <- ws$.__enclos_env__[["stream"]][["data"]]
    # get the new stream data
    # new_data <- jsonlite::fromJSON(event$data) # warning if event$data is a vector
    if (is.list(event$data)){
      new_data <- jsonlite::fromJSON(as.list(event$data))
    } else {
      new_data <- jsonlite::fromJSON(event$data)
    }
    # assign a class for binance_ws_cleaner function
    attr(new_data, "class") <- c(subscription, class(new_data))
    # structure the output
    new_data <- binance_ws_cleaner(new_data)
    
    if (subscription == "depth") {
      # substitute the old order book with an updated version
      new_data <- binance_ws_orderbook(data_before = old_data, data_after = new_data)
      ws$.__enclos_env__[["stream"]][["data"]] <- new_data
      
    } else if (subscription == "kline") {
      # check if the candle is closed 
      new_data <- binance_ws_kline(data_before = old_data, data_after = new_data)
      ws$.__enclos_env__[["stream"]][["data"]] <- new_data
      
    } else {
      # save the data 
      ws$.__enclos_env__[["stream"]][["data"]] <- dplyr::bind_rows(new_data, old_data)
    }
    
    # limit the number of observations that can be saved 
    if (!is.null(nobs_max)) {
      ws$.__enclos_env__[["stream"]][["data"]] <- utils::head(ws$.__enclos_env__[["stream"]][["data"]], n = nobs_max)
    }
    
    # update the number of observations
    ws$.__enclos_env__[["stream"]][["info"]]$nobs <- nrow(ws$.__enclos_env__$stream$data)
  })
  
  # onClose: when the connection is closed 
  ws$onClose(function(event) {
    # save the time when connection is closed
    ws$.__enclos_env__[["stream"]][["info"]]$date_close <- Sys.time()
    # verbose message 
    if (!quiet) {
      msg <- paste0("ERROR: Stream: ", pair, "@", subscription, " failed to connect: ", event$code, "\n")
      message(msg)
    }
  })
  
  # onError: when an error is received  
  ws$onError(function(event) {
    # verbose message 
    if (!quiet) {
      msg <- paste0("ERROR: Stream: ", pair, "@", subscription, " failed to connect: ", event$code, "\n")
      message(msg)
    }
  })
  
  return(ws)
}

# Create wss URL for spot websocket 
binance_ws_spot_url <- function(pair = "btcusdt", subscription = "kline", interval = "1m", update_speed = 1000, quiet = FALSE){
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "btcusdt"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- tolower(pair)
  }

  # base url 
  base_url <- "wss://stream.binance.com:9443/ws"
  
  # available subscriptions
  subscription <- match.arg(subscription, choices = c("aggTrade", "bookTicker", "depth", "kline", "miniTicker", "ticker", "trade"))
  
  if (subscription == "kline") {
    # Available intervals 
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M")
    interval <- match.arg(interval, choices = av_int)
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription, "_", interval)
  } else if (subscription == "ticker" & !is.null(interval) && interval %in% c("1h", "4h", "1d")) {
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription, "_", interval)
  } else if (subscription == "depth") {
    # Depth stream with different update speed
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "1000"))
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription, "@", update_speed, "ms")
  } else {
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription)
  }
  
  # Websocket url
  websocket_url <- paste0(base_url, "/", ws_subscription)
  
  # verbose message
  if (!quiet) {
    msg <- paste0('Stream: ', ws_subscription)
    cli::cli_alert_success(msg)
  }
  attr(websocket_url, "ws_subscription") <- ws_subscription
  return(websocket_url)
}

# Create wss URL for fapi websocket 
binance_ws_fapi_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", contract_type = NULL, update_speed = 1000, quiet = FALSE){
  
  # Base url 
  base_url <- "wss://fstream.binance.com/ws"
  
  # Check pair argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "btcusdt"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- tolower(pair)
  }
  
  # Check subscription argument 
  if (missing(subscription) || is.null(subscription)) {
    if (!quiet) {
      msg <- paste0('The subscription argument is missing without default.')
      cli::cli_abort(msg)
    }
  } else {
    # Available subscriptions
    av_subscription <- c("aggTrade", "bookTicker", "continuousKline", "depth", "forceOrder", "kline", "markPrice", "miniTicker", "ticker", "trade")
    subscription <- match.arg(subscription, choices = av_subscription)
  }
  
  if (subscription %in% c("kline", "continuousKline")) {
    # Available intervals 
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M")
    interval <- match.arg(interval, choices = av_int)
    # Available contract_type if subscription is continuousKline
    if (subscription == "continuousKline") {
      contract_type <- tolower(contract_type) 
      contract_type <- match.arg(contract_type, choices = c("perpetual", "current_quarter", "next_quarter"))
      contract_type <- paste0("_", contract_type) 
    } else {
      contract_type <- ""
    }
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, contract_type, "@", subscription, "_", interval)
  } else if (subscription == "depth") {
    # Depth stream with different update speed
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "250", "500"))
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription, "@", update_speed, "ms")
  } else {
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription)
  }
  
  # Websocket url
  websocket_url <- paste0(base_url, "/", ws_subscription)
  
  # verbose message
  if (!quiet) {
    msg <- paste0('Stream: ', ws_subscription)
    cli::cli_alert_success(msg)
  }
  attr(websocket_url, "ws_subscription") <- ws_subscription
  return(websocket_url)
}

# Create wss URL for dapi websocket (in progress)
binance_ws_dapi_url <- function(pair = "BTCUSD", subscription = "kline", interval = "1m", contract_type = NULL, update_speed = 1000, quiet = FALSE){
  
  # Base url 
  base_url <- "wss://dstream.binance.com/ws"
  
  # Check pair argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "btcusdt"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- tolower(pair)
  }
  
  # Check subscription argument 
  if (missing(subscription) || is.null(subscription)) {
    if (!quiet) {
      msg <- paste0('The subscription argument is missing without default.')
      cli::cli_abort(msg)
    }
  } else {
    # Available subscriptions
    av_subscription <- c("aggTrade", "bookTicker", "continuousKline", "contractInfo", "depth", "forceOrder", "indexPrice", 
                         "indexPriceKline", "kline", "markPrice", "markPriceKline", "miniTicker", "ticker", "trade")
    subscription <- match.arg(subscription, choices = av_subscription)
  }
  
  if (subscription %in% c("kline", "continuousKline")) {
    # Available intervals 
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M")
    interval <- match.arg(interval, choices = av_int)
    # Available contract_type if subscription is continuousKline
    if (subscription == "continuousKline") {
      contract_type <- tolower(contract_type) 
      contract_type <- match.arg(contract_type, choices = c("perpetual", "current_quarter", "next_quarter"))
      contract_type <- paste0("_", contract_type) 
    } else {
      contract_type <- ""
    }
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, contract_type, "@", subscription, "_", interval)
  } else if (subscription == "depth") {
    # Depth stream with different update speed
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "250", "500"))
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription, "@", update_speed, "ms")
  } else {
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription)
  }
  
  # Websocket url
  websocket_url <- paste0(base_url, "/", ws_subscription)
  
  # verbose message
  if (!quiet) {
    msg <- paste0('Stream: ', ws_subscription)
    cli::cli_alert_success(msg)
  }
  attr(websocket_url, "ws_subscription") <- ws_subscription
  return(websocket_url)
}

# IN PROGRESS
binance_ws_eapi_url <- function(pair = "BTCUSD", subscription = "kline", interval = "1m", expiration_date = NULL, update_speed = 1000, quiet = FALSE){
  
  # Base url 
  base_url <- "wss://nbstream.binance.com/eoptions"
  
  # Check pair argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "btcusdt"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- tolower(pair)
  }
  
  # Check subscription argument 
  if (missing(subscription) || is.null(subscription)) {
    if (!quiet) {
      msg <- paste0('The subscription argument is missing without default.')
      cli::cli_abort(msg)
    }
  } else {
    # Available subscriptions
    av_subscription <- c("index", "depth", "index", "kline", "markPrice", "openInterest", "ticker", "trade")
    subscription <- match.arg(subscription, choices = av_subscription)
  }
  
  if (subscription %in% c("kline", "continuousKline")) {
    # Available intervals 
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M")
    interval <- match.arg(interval, choices = av_int)
    # Available contract_type if subscription is continuousKline
    if (subscription == "continuousKline") {
      contract_type <- tolower(contract_type) 
      contract_type <- match.arg(contract_type, choices = c("perpetual", "current_quarter", "next_quarter"))
      contract_type <- paste0("_", contract_type) 
    } else {
      contract_type <- ""
    }
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, contract_type, "@", subscription, "_", interval)
  } else if (subscription == "depth") {
    # Depth stream with different update speed
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "250", "500"))
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription, "@", update_speed, "ms")
  } else {
    # Stream subscription identifier  
    ws_subscription <- paste0(pair, "@", subscription)
  }
  
  # Websocket url
  websocket_url <- paste0(base_url, "/", ws_subscription)
  
  # verbose message
  if (!quiet) {
    msg <- paste0('Stream: ', ws_subscription)
    cli::cli_alert_success(msg)
  }
  attr(websocket_url, "ws_subscription") <- ws_subscription
  return(websocket_url)
}
