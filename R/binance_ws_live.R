#' binance_live_depth
#' @name binance_live_depth
#' @rdname binance_live_depth
#' @description live orderbook
#' @param pair character
#' @export

binance_live_depth <- function(pair = "BTCUSDT"){

  response <- NULL

  pair_name <- toupper(pair)

  # Websocket connection for Depth Update
  ws <- binance_ws_spot_socket(pair = pair_name, subscription = "depth", interval = NULL)

  # Snapshot of the Order Book
  response <- binance_depth(api = "spot", pair = pair_name)

  # Nest to replicate the stucture in "binance_api_soocket"

  # Add the order book in the Websocket Data
  ws$.__enclos_env__$stream$data  <-  response

  ws$connect()

  Data <- function(){
    ws$.__enclos_env__$stream$data
  }

  structure(
    list(
      Close = ws$close,
      Data = Data
    )
  )
}

#' binance_live_klines
#'@name binance_live_klines
#'@rdname binance_live_klines
#'@description live candlestick
#'
#' @export

binance_live_klines <- function(pair = "BTCUSDT", interval = "1m", data = NULL){

  pair_name <- toupper(pair)
  response <- NULL

  # Initialize with some data
  if(is.null(data)){

    from <- Sys.time()-lubridate::days(2)
    to <- Sys.time()
    response <- binance_klines(api = "spot", pair = pair_name, from = from, to = to, interval = interval)

  } else {

    response <- data
  }


  # Websocket connection for Depth Update
  ws <- binance_ws_spot_socket(pair = pair_name, subscription = "kline", interval = interval)

  # Update till last Data
  new_data <- binance_klines(api = "spot", pair = pair_name, from = max(response$date), to = Sys.time(), interval = interval )

  # Replicate the stucture in "binance_api_socket"
  response <- dplyr::bind_rows(new_data, response)
  response <- response[!duplicated(response$date),]
  response <- dplyr::mutate(response, is_closed = TRUE)

  # Add the order book in the Websocket Data
  ws$.__enclos_env__$stream$data  <- response

  ws$connect()

  Data <- function(){
    ws$.__enclos_env__$stream$data
  }

  structure(
    list(
      Close = ws$close,
      Data = Data
    )
  )
}

#' binance_live_trades
#'@name binance_live_trades
#'@rdname binance_live_trades
#'@description live trades
#'
#' @export

binance_live_trades <- function(pair = "BTCUSDT"){

  response <- NULL

  pair_name <- toupper(pair)

  # Websocket connection for Trades update
  ws <- binance_ws_spot_socket(pair = pair_name, subscription = "aggTrade", interval = NULL)

  # Snapshot of trades of last 10 minutes 
  response <- binance_trades(pair = pair_name)

  # Add trades in the Websocket Data
  ws$.__enclos_env__$stream$data  <- response

  ws$connect()

  Data <- function(){
    ws$.__enclos_env__$stream$data
  }

  structure(
    list(
      Close = ws$close,
      Data = Data
    )
  )
}

#' binance_live_order_book
#'@name binance_live_order_book
#'@rdname binance_live_order_book
#'@description live trades
#'
#' @export

binance_live_order_book <- function(pair){
  
  if(missing(pair) || is.null(pair)){
    warning('Pair argument is missing default: "BTCUSDT"')
    pair <- "BTCUSDT"
  } else {
    pair <- toupper(pair)
  }
  
  # live stream for depth data 
  ws_depth <- binance_live_depth(pair)
  # live stream for trades data 
  ws_trade <- binance_live_trades(pair)
  
  Data <- function(from = 20000, to = 30000, levels = 20){
    
    ob <- OrderBook(ws_depth$Data(), from, to, levels = levels)
    tb <- TradeBook(ws_trade$Data(), from, to, levels = levels)
    
    ob$buy <- tb$buy
    ob$sell <- tb$sell
    ob$net <- tb$net
    
    return(ob)
  }
  
  Close <- function(){
    ws_depth$Close()
    ws_trade$Close()
  }
  
  structure(
    list(
      Data = Data,
      Close = Close
    )
  )
}
