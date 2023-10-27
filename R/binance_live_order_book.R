#' Binance Live Order Book  
#' 
#' Local order book management for a trading pair. 
#' 
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' 
#' @param api Character. Reference API. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#diff-depth-stream).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#diff-book-depth-streams).
#'   
#' @param update_speed Integer. Update speed in milliseconds. 
#' 
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @usage 
#' binance_live_order_book(pair, 
#'                         api, 
#'                         update_speed = 1000, 
#'                         quiet = FALSE)
#' @export
#' 
#' @name binance_live_order_book
#' @rdname binance_live_order_book

binance_live_order_book <- function(pair, api, update_speed = 1000, quiet = FALSE){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The `pair` argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- tolower(pair)
  }
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The `api` argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi"))
  }
  
  # Url parameters depends on api 
  if (api == "spot") {
    args <- list(pair = pair, subscription = "depth", update_speed = update_speed, quiet = quiet)
  } else if (api == "fapi") {
    args <- list(pair = pair, subscription = "depth", update_speed = update_speed, quiet = quiet)
  } 
  
  # Function name 
  fun_name <- paste0("binance_ws_", api, "_url")
  # Create the subscription url
  websocket_url <- do.call(fun_name, args = args)
  # Stream subscription identifier  
  ws_subscription <- attr(websocket_url, "ws_subscription")

  # Initialize a websocket object 
  ws <- websocket::WebSocket$new(websocket_url, autoConnect = FALSE)
  # Initialize a list for stream data
  ws$.__enclos_env__[["stream"]] <- list()
  # Initialize a slot for order_book data
  ws$.__enclos_env__[["order_book"]] <- list()
  # Initialize a slot for last_update_id 
  ws$.__enclos_env__[["last_update_id"]] <- 0
  
  # onOpen: when the connection is opened 
  ws$onOpen(function(event) {
    
    # Retrieve a snapshot of the order_book
    ws$.__enclos_env__[["order_book"]] <- binance_depth(pair, api = api)
    ws$.__enclos_env__[["last_update_id"]] <- ws$.__enclos_env__[["order_book"]]$last_update_id[1]
    
    # verbose message 
    if (!quiet) {
      msg <- paste0("Stream ", ws_subscription, " opened!")
      cli::cli_alert_success(msg)
    }
  })
  
  # onMessages: when a message is received  
  ws$onMessage(function(event) {
    
    # Extract old stream data 
    stream <- ws$.__enclos_env__[["stream"]]
    # Extract last_update_id
    last_update_id <- ws$.__enclos_env__[["last_update_id"]]
    # Convert new stream data from JSON 
    new_data <- jsonlite::fromJSON(event$data)
    # Clean and store new stream data
    stream[[length(stream) + 1]] <- binance_ws_cleaner.depth(new_data)
    # Index of events received after last_update_id
    idx_stream_first_update <- purrr::map_dbl(stream, ~.x$first_update_id[1])
    idx_last_update <- idx_stream_first_update >= last_update_id + 1 
    # Remove events before last_update_id 
    stream <- stream[idx_last_update]
    # Process in order all the events after last_update_id
    if(!purrr::is_empty(stream)){
      for(i in 1:length(stream)){
        data_before <- ws$.__enclos_env__[["order_book"]]
        data_after <- stream[[i]]
        ws$.__enclos_env__[["order_book"]] <- binance_ws_orderbook(data_before = data_before, data_after = data_after)
        last_update_id <- ws$.__enclos_env__[["order_book"]]$last_update_id[1]
      }
    }
    # Save last_update_id
    ws$.__enclos_env__[["last_update_id"]] <- last_update_id
    # Index of events received after last_update_id
    idx_stream_last_update <- purrr::map_dbl(stream, ~.x$last_update_id[1])
    idx_last_update <- idx_stream_last_update >= last_update_id + 1
    # Remove events before last_update_id 
    ws$.__enclos_env__[["stream"]] <- stream[idx_last_update]
  })
  
  # onClose: when the connection is closed 
  ws$onClose(function(event) {
    msg <- jsonlite::toJSON(list(method = "UNSUBSCRIBE", params = ws_subscription, id = 221))
    ws$send(msg)
    # verbose message 
    if (!quiet) {
      msg <- paste0("Stream ", ws_subscription, " closed. (last_update_id: ", ws$.__enclos_env__[["last_update_id"]], ")")
      cli::cli_alert_success(msg)
    }
  })
  
  # onError: when an error is received  
  ws$onError(function(event) {
    # verbose message 
    if (!quiet) {
      msg <- paste0("Stream error: ", ws_subscription)
      cli::cli_alert_warning(msg)
    }
  })
  
  ws$connect()
  
  structure(
    list(
      Close = ws$close,
      Data = function() ws$.__enclos_env__$order_book,
      Stream = function() ws$.__enclos_env__$stream,
      last_update_id = function() ws$.__enclos_env__$last_update_id
    )
  )
}