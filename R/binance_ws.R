#' binance_ws_spot_socket
#' @name binance_ws_spot_socket
#' @rdname binance_ws_spot_socket
#' @description api websocket
#'
#' @export
binance_ws_spot_socket <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", update_speed = 1000, nobs_max = NULL, quiet = FALSE){
  
  pair_name <- toupper(pair)
  
  # Websocket subscription url
  web_socket_url <- binance_ws_spot_url(pair = pair, subscription = subscription, interval = interval, update_speed = update_speed, quiet = quiet)
  
  # Api WebSocket 
  ws <- websocket::WebSocket$new(web_socket_url, autoConnect = FALSE)
  
  # Create a new list to save the Data
  ws$.__enclos_env__[["stream"]] <- list()
  # Slot for saving websocket data
  ws$.__enclos_env__[["stream"]][["data"]] <- dplyr::tibble()
  # Slot for saving websocket informations 
  ws$.__enclos_env__[["stream"]][["info"]] <- dplyr::tibble(
    date_open = NA_character_,
    date_close = NA_character_,
    pair = pair_name,
    subscription = subscription,
    interval = interval,
    update_speed = update_speed,
    nobs = 0)
  
  # Open Connection 
  ws$onOpen(function(event) {
    
    if(!quiet) message("Stream: ", pair_name, "@", subscription, " opened!")
    
    # Update with time when connection the is opened (date_open)
    ws$.__enclos_env__[["stream"]][["info"]]$date_open = Sys.time()
  })
  
  # Messages 
  ws$onMessage(function(event) {
    
    # Get the Old Data
    old_data <- ws$.__enclos_env__[["stream"]][["data"]]
    
    # Get the New Data
    new_data <- jsonlite::fromJSON(event$data)
    
    # Assign the class for "binance_ws_cleaner"
    attr(new_data, "class") <- c(subscription, class(new_data))
    
    # Structure the output as tibble
    new_data <- binance_ws_cleaner(new_data)
    
    # If depth we substitue the old order book with the updated one
    if (subscription == "depth") {
      new_data <- suppressMessages(binance_ws_orderbook(data_before = old_data, data_after = new_data))
      ws$.__enclos_env__[["stream"]][["data"]] <- new_data
    } else if (subscription == "kline") {
      # If kline we check if the candle is closed to avoid repetitions
      new_data <- binance_ws_kline(data_before = old_data, data_after = new_data)
      ws$.__enclos_env__[["stream"]][["data"]] <- new_data
    } else {
      # In all the other cases we save the data (kline, trades and aggTrades)
      ws$.__enclos_env__[["stream"]][["data"]] <- dplyr::bind_rows(new_data, old_data)
    }
    
    if (is.null(nobs_max)) {
      ws$.__enclos_env__[["stream"]][["data"]] <-  head(ws$.__enclos_env__[["stream"]][["data"]], n = nobs_max)
    }
    
    # Update number of observations (nobs)
    ws$.__enclos_env__[["stream"]][["info"]]$nobs <- nrow(ws$.__enclos_env__$stream$data)
  })
  
  # Close the connection 
  ws$onClose(function(event) {
    if(!quiet) message("Stream: ", toupper(pair), "@", subscription, " Closed with code ", event$code, "\n")
    # Update with time when connection the is closed (date_close)
    ws$.__enclos_env__[["stream"]][["info"]]$date_close <- Sys.time()
  })
  
  # Error in connection 
  ws$onError(function(event) {
    if(!quiet) message("ERROR: Stream: ", pair_name, "@", subscription, " failed to connect: ", event$code, "\n")
  })
  
  return(ws)
}

# Create the request URL for the spot Api 
binance_ws_spot_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", update_speed = 1000, quiet = FALSE){
  
  # Websocket Url and base url 
  web_socket_url <- NULL
  base_url <- "wss://stream.binance.com:9443/ws"
  
  # Available Subscriptions
  subscription <- match.arg(subscription, choices = c("aggTrade", "bookTicker", "depth", "kline", "miniTicker", "ticker", "trade"))
  
  # Pair Name
  pair_name <- tolower(pair)
  
  
  if (subscription == "kline") {
    
    interval <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m", "30m", "1h",
                                                "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M"))
    
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "_", interval)
    
    msg <- paste0('The stream opened (klines): ', pair_name, "@", subscription, "_", interval)
    
    if(!quiet) message(msg)
    
    
  } else if (subscription == "ticker" & !is.null(interval) && interval %in% c("1h", "4h", "1d")) {
    
    # Ticker with Window Size
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "_", interval)
    
    msg <- paste0('The stream opened is a ticker with Window Size: ', pair_name, "@", subscription, "_", interval)
    
    if(!quiet) message(msg)
    
  } else if (subscription == "depth") {
    
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "1000"))
    
    # Depth stream with a different update speed
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "@", update_speed, "ms")
    
    msg <- paste0('The stream opened is a Depth with Update Speed: ', pair_name, "@", subscription, "@", update_speed, "ms")
    
    if(!quiet) message(msg)
    
  } else {
    
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription)
    
  }
  
  return(web_socket_url)
  
}

#' binance_ws_fapi_socket
#'@name binance_ws_fapi_socket
#'@rdname binance_ws_fapi_socket
#'@description fapi websocket
#'
#' @export
binance_ws_fapi_socket <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", contract_type = NULL, update_speed = 1000, nobs_max = NULL, quiet = FALSE){
  
  pair_name <- toupper(pair)
  
  web_socket_url <- binance_ws_fapi_url(pair = pair_name, subscription = subscription, interval = interval, contract_type = contract_type, update_speed = update_speed, quiet = quiet)
  
  ws <- websocket::WebSocket$new(web_socket_url, autoConnect = FALSE)
  
  # Create a new environment to save the Data
  ws$.__enclos_env__[["stream"]] <- new.env()
  
  # Slot for saving websocket data
  ws$.__enclos_env__[["stream"]][["data"]] <- dplyr::tibble()
  
  ws_info <- dplyr::tibble(
    date_open = NA_character_,
    date_close = NA_character_,
    api = "fapi",
    pair = pair_name,
    subscription = subscription,
    interval = interval,
    update_speed = update_speed,
    nobs = 0
  )
  
  ws$.__enclos_env__[["stream"]][["info"]] <- ws_info
  ws$.__enclos_env__$stream$info <- ws_info
  
  ws$onOpen(function(event) {
    if(!quiet) message("Stream: ", pair_name, "@", subscription, " opened!")
    # Update with time when connection the is opened (date_open)
    ws$.__enclos_env__[["stream"]][["info"]]$date_open <- Sys.time()
  })
  
  ws$onMessage(function(event) {
    
    # Get the Old Data
    old_data <- ws$.__enclos_env__$stream$data
    # Get the New Data
    new_data <- jsonlite::fromJSON(event$data)
    # Assign the class for "binance_ws_cleaner"
    attr(new_data, "class") <- c(subscription, class(new_data))
    # Clean the output as tibble
    new_data <- binance_ws_cleaner(new_data)
    
    if(subscription == "depth"){
      # If depth we substitue the old order book with the updated one
      new_data <- binance_ws_orderbook(data_before = old_data, data_after = new_data)
      ws$.__enclos_env__[["stream"]][["data"]] <- new_data
    } else if(subscription %in% c("kline", "continuousKline")){
      # If kline we check if the candle is closed to avoid repetitions
      new_data <- binance_ws_kline(data_before = old_data, data_after = new_data)
      # Update the stream dataset (data)
      ws$.__enclos_env__[["stream"]][["data"]] <- new_data
    } else {
      # In all the other cases we save the data (kline, trades and aggTrades)
      ws$.__enclos_env__[["stream"]][["data"]] <- dplyr::bind_rows(new_data, old_data)
    }
    
    if (is.null(nobs_max)) {
      ws$.__enclos_env__[["stream"]][["data"]] <-  head(ws$.__enclos_env__[["stream"]][["data"]], n = nobs_max)
    }
    
    # Update number of observations (nobs)
    ws$.__enclos_env__[["stream"]][["info"]]$nobs <- nrow(ws$.__enclos_env__$stream$data)
  })
  
  # Close the connection 
  ws$onClose(function(event) {
    if(!quiet) message("Stream: ", pair_name, "@", subscription, " Closed with code ", event$code, "\n")
    # Update with time when connection the is closed (date_close)
    ws$.__enclos_env__[["stream"]][["info"]]$date_close = Sys.time()
  })
  
  # Error in connection 
  ws$onError(function(event) {
    if(!quiet) message("ERROR: Stream: ", pair_name, "@", subscription, " failed to connect: ", event$code, "\n")
  })
  
  return(ws)
}

binance_ws_fapi_url <- function(pair = "BTCUSDT", subscription = "kline", interval = "1m", contract_type = NULL, update_speed = 1000, quiet = FALSE){
  
  # Websocket Url
  web_socket_url <- NULL
  base_url <- "wss://fstream.binance.com/ws"
  
  # Available Subscriptions
  subscription <- match.arg(subscription, choices = c("aggTrade", "bookTicker", "continuousKline", "depth", 
                                                      "forceOrder", "kline", "markPrice", "miniTicker", "ticker", "trade"))
  
  # Pair Name
  pair_name <- tolower(pair)
  
  if(subscription %in% c("kline", "continuousKline")){
    interval <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m", "30m", "1h",
                                                "2h", "4h", "6h", "12h", "1d", "3d", "1w", "1M"))
    
    if(subscription == "kline"){
      web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "_", interval)
    } else {
      contract_type <- tolower(contract_type)
      contract_type <- match.arg(contract_type, choices = c("perpetual", "current_quarter", "next_quarter"))
      web_socket_url <- paste0(base_url, "/", pair_name, "_", contract_type, "@", subscription, "_", interval)
    }
    
  } else if(subscription == "depth") {
    update_speed <- as.character(update_speed)
    update_speed <- match.arg(update_speed, choices = c("100", "250", "500"))
    # Depth stream with a different update speed
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription, "@", update_speed, "ms")
    msg <- paste0('The stream created is a Depth with Update Speed: ', pair_name, "@", subscription, "@", update_speed, "ms")
    
    if(!quiet) message(msg)
    
  } else {
    web_socket_url <- paste0(base_url, "/", pair_name, "@", subscription)
  }
  return(web_socket_url)
}
