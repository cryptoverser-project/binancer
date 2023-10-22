#' binance_spot_socket
#' @name binance_ws_socket
#' @rdname binance_ws_socket
#' @description api websocket
#' @param pair Character, the trading pair of interest, e.g., "BTCUSDT".
#' @param api Character, specifying the reference API. Available options are `spot`, `fapi`, `dapi`, `eapi`.
#' @param subscription Character
#' @param interval Character, the time interval for Klines data. 
#' Acceptable intervals are "1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", and "1M".
#' @param contract_type Character, only used if `api = "dapi"`. Choose from:
#'   - "perpetual": For perpetual futures.
#'   - "current_quarter": For futures with a maturity in the current quarter.
#'   - "next_quarter": For futures with a maturity in the next quarter.
#' @param update_speed time in milliseconds 
#' @param nobs_max Integer, maximum number of observations to save. 
#' @param quiet Logical, suppress informational messages if TRUE.
#' @export

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
    new_data <- jsonlite::fromJSON(event$data)
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

# Create the request URL for the spot websocket  
binance_ws_spot_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", update_speed = 1000, quiet = FALSE){
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }

  # base url 
  base_url <- "wss://stream.binance.com:9443/ws"
  
  # available subscriptions
  subscription <- match.arg(subscription, choices = c("aggTrade", "bookTicker", "depth", "kline", "miniTicker", "ticker", "trade"))
  
  # pair name
  pair <- tolower(pair)
  # create websocket_url 
  if (subscription == "kline") {
    
    # available intervals 
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M")
    interval <- match.arg(interval, choices = av_int)
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, "@", subscription, "_", interval)
    # verbose message
    msg <- paste0('The stream is klines: ', pair, "@", subscription, "_", interval)
    
  } else if (subscription == "ticker" & !is.null(interval) && interval %in% c("1h", "4h", "1d")) {
    
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, "@", subscription, "_", interval)
    # verbose message
    msg <- paste0('The stream is a ticker with Window Size: ', pair, "@", subscription, "_", interval)
    
  } else if (subscription == "depth") {
    
    # depth stream with a different update speed
    update_speed <- match.arg(as.character(update_speed), choices = c("100", "1000"))
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, "@", subscription, "@", update_speed, "ms")
    # verbose message
    msg <- paste0('The stream is Depth with Update Speed: ', pair, "@", subscription, "@", update_speed, "ms")
    
  } else {
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, "@", subscription)
    # verbose message
    msg <- paste0('The stream is: ', pair, "@", subscription, "@", update_speed, "ms")
  }
  
  if (!quiet) {
    message(msg)
  }
  
  return(websocket_url)
}

# Create the request URL for the fapi websocket 
binance_ws_fapi_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", contract_type = NULL, update_speed = 1000, quiet = FALSE){
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # base url 
  base_url <- "wss://fstream.binance.com/ws"
  # available subscriptions
  subscription <- match.arg(subscription, choices = c("aggTrade", "bookTicker", "continuousKline", "depth", 
                                                      "forceOrder", "kline", "markPrice", "miniTicker", "ticker", "trade"))
  
  # pair name
  pair <- tolower(pair)
  # create websocket_url 
  if (subscription %in% c("kline", "continuousKline")) {
    
    # available intervals 
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M")
    interval <- match.arg(interval, choices = av_int)
    # available contract_type for continuousKline
    if (subscription == "continuousKline") {
      contract_type <- match.arg(tolower(contract_type), choices = c("perpetual", "current_quarter", "next_quarter"))
      contract_type <- paste0("_", contract_type) 
    } else {
      contract_type <- ""
    }
    
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, contract_type, "@", subscription, "_", interval)
    # verbose message
    msg <- paste0('The stream is klines: ', pair, "(", contract_type, ")", "@", subscription, "_", interval)
    
  } else if (subscription == "depth") {
    
    # depth stream with a different update speed
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "250", "500"))
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, "@", subscription, "@", update_speed, "ms")
    # verbose message
    msg <- paste0('The stream created is a Depth with Update Speed: ', pair, "@", subscription, "@", update_speed, "ms")
    
  } else {
    # websocket url
    websocket_url <- paste0(base_url, "/", pair, "@", subscription)
    # verbose message
    msg <- paste0('The stream is: ', pair, "@", subscription, "@")
  }
  
  if (!quiet) {
    message(msg)
  }
  
  return(websocket_url)
}