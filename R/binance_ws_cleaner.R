# Clean and structure different binance websocket responses
binance_ws_cleaner <- function(data){
  UseMethod("binance_ws_cleaner")
}

# Cleaner for websocket "aggTrade" endpoint
binance_ws_cleaner.aggTrade <- function(data){

  if (purrr::is_empty(data)) {
    return(dplyr::tibble())
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output,
                          date = "E", 
                          agg_id = "a", 
                          first_id = "f", 
                          last_id = "l",
                          pair = "s", 
                          price = "p", 
                          quantity = "q", 
                          side = "m")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          price = as.numeric(price),
                          quantity = as.numeric(quantity),
                          side = ifelse(side, "SELL", "BUY"))
  return(output)
}

# Cleaner for websocket "trade" endpoint
binance_ws_cleaner.trade <- function(data){

  if (purrr::is_empty(data)) {
    return(dplyr::tibble())
  }
  
  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, 
                          date = "T", 
                          id = "t", 
                          buy_id = "b", 
                          sell_id = "a", 
                          pair = "s", 
                          price = "p", 
                          quantity = "q", 
                          side = "m")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          price = as.numeric(price),
                          quantity = as.numeric(quantity),
                          side = ifelse(side, "SELL", "BUY"))
  return(output)
}


# Cleaner for websocket "miniticker" endpoint
binance_ws_cleaner.miniTicker <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, 
                          date = "E", 
                          pair = "s", 
                          close = "c", 
                          open = "o", 
                          high = "h", 
                          low = "l", 
                          volume = "v", 
                          quantity = "q")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          volume = as.double(volume),
                          quantity = as.double(quantity))

  return(output)
}

# Cleaner for websocket "ticker" endpoint
binance_ws_cleaner.ticker <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output,
                          date = "E", 
                          pair = "s", 
                          price_change = "p", 
                          price_change_perc = "P", 
                          weighted_price = "w",
                          last_quantity = "Q", 
                          bid = "b", 
                          bid_quantity = "B",
                          ask = "a", 
                          ask_quantity = "A",
                          open = "o", 
                          close = "c", 
                          high = "h", 
                          low = "l", 
                          volume = "v", 
                          quantity = "q", 
                          trades = "n")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          price_change = as.double(price_change),
                          price_change_perc = as.double(price_change_perc),
                          weighted_price = as.double(weighted_price),
                          last_quantity = as.double(last_quantity),
                          bid = as.double(bid),
                          bid_quantity = as.double(bid_quantity),
                          ask = as.double(ask),
                          ask_quantity = as.double(ask_quantity),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          volume = as.double(volume),
                          quantity = as.double(quantity),
                          trades = as.integer(trades))
  return(output)
}

# Cleaner for websocket "bookTicker" endpoint
binance_ws_cleaner.bookTicker <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }
  
  output <- dplyr::bind_rows(data)
  output <- dplyr::select(output, 
                          date = "u", 
                          pair = "s", 
                          bid = "b", 
                          quantity_bid = "B", 
                          ask = "a", 
                          quantity_ask = "A")
  output <- dplyr::mutate(output,
                          date = Sys.time(),
                          bid = as.double(bid),
                          quantity_bid = as.double(quantity_bid),
                          ask = as.double(ask),
                          quantity_ask = as.double(quantity_ask))
  
  return(output)
}

# Cleaner for websocket "kline" endpoint
binance_ws_cleaner.kline <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data$k)
  output <- dplyr::select(output,
                          date = "t", 
                          date_close = "T",
                          pair = "s",
                          open = "o", 
                          close = "c", 
                          high = "h", 
                          low = "l", 
                          trades = "n", 
                          is_closed = "x",
                          volume = "v", 
                          volume_quote = "q", 
                          taker_buy = "V", 
                          taker_buy_quote = "Q")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          date_close = as.POSIXct(date_close/1000, origin = "1970-01-01"),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          trades = as.integer(trades),
                          volume = as.double(volume),
                          volume_quote = as.double(volume_quote),
                          taker_buy = as.double(taker_buy),
                          taker_buy_quote = as.double(taker_buy_quote))
  
  return(output)
}

# Cleaner for websocket "binance_ws_cleaner.continuousKline" endpoint
binance_ws_cleaner.continuousKline <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }
  
  output <- dplyr::bind_rows(data$k)
  output <- dplyr::mutate(output, pair = data$ps, contract = data$ct)
  output <- dplyr::select(output,
                          date = "t", 
                          date_close = "T", 
                          pair = "s",
                          open = "o", 
                          close = "c", 
                          high = "h", 
                          low = "l", 
                          trades = "n", 
                          is_closed = "x",
                          volume = "v", 
                          volume_quote = "q", 
                          taker_buy = "V", 
                          taker_buy_quote = "Q")
  output <- dplyr::mutate(output,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          date_close = as.POSIXct(date_close/1000, origin = "1970-01-01"),
                          close = as.double(close),
                          open = as.double(open),
                          high = as.double(high),
                          low = as.double(low),
                          trades = as.integer(trades),
                          volume = as.double(volume),
                          volume_quote = as.double(volume_quote),
                          taker_buy = as.double(taker_buy),
                          taker_buy_quote = as.double(taker_buy_quote))
  
  return(output)
}

# Cleaner for websocket "markPrice" endpoint
binance_ws_cleaner.markPrice <- function(data){

  if (purrr::is_empty(data)) {
    return(data)
  }
  
  output <- dplyr::bind_rows(data)
  output <- dplyr::mutate(output, date = Sys.time())
  output <- dplyr::select(output, 
                          date, 
                          next_funding_date = "T", 
                          pair = "s", 
                          mark_price = "p", 
                          index_price = "i", 
                          settlement = "P", 
                          funding_rate = "r")
  output <- dplyr::mutate(output,
                          next_funding_date = as.POSIXct(next_funding_date/1000, origin = "1970-01-01"),
                          mark_price = as.numeric(mark_price),
                          index_price = as.numeric(index_price),
                          settlement = as.numeric(settlement),
                          funding_rate = as.numeric(funding_rate))
  
  return(output)
}

# Cleaner for websocket "forceOrder" endpoint
binance_ws_cleaner.forceOrder <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }

  output <- dplyr::bind_rows(data$o)
  
  return(output)
}

# Cleaner for websocket "depth" endpoint
binance_ws_cleaner.depth <- function(data){
  
  if (purrr::is_empty(data)) {
    return(data)
  }

  df_out <- dplyr::bind_rows(data[c(2,3,4,5)])
  df_out <- dplyr::select(df_out, date = "E", pair = "s")

  # BID data
  colnames(data$b) <- c("price", "quantity")
  df_BID <- dplyr::as_tibble(data$b)
  df_BID <- dplyr::mutate_all(df_BID, as.numeric)
  df_BID <- dplyr::bind_cols(df_out, df_BID, side = "BID")

  # ASK data
  colnames(data$a) <- c("price", "quantity")
  df_ASK <- dplyr::as_tibble(data$a)
  df_ASK <- dplyr::mutate_all(df_ASK, as.numeric)
  df_ASK <- dplyr::bind_cols(df_out, df_ASK, side = "ASK")

  # Output dataset 
  df_out <- dplyr::bind_rows(df_ASK, df_BID)
  df_out <- dplyr::mutate(df_out,
                          date = as.POSIXct(date/1000, origin = "1970-01-01"),
                          side = factor(side, levels = c("ASK", "BID"), ordered = FALSE))
  
  return(df_out)
}

# Structure kline data to avoid un-closed candles
binance_ws_kline <- function(data_before, data_after){

  if (purrr::is_empty(data_before) || nrow(data_before) == 0) {
    return(data_after)
  }

  if (data_before[1,]$is_closed) {
    merge_data <- dplyr::bind_rows(data_after, data_before)
  } else {
    merge_data <- dplyr::bind_rows(data_after, data_before[-1,])
  }

  return(merge_data)
}

# Structure order book data updating the levels
binance_ws_orderbook <- function(data_before = NULL, data_after){

  if (purrr::is_empty(data_before)) {
    return(data_after)
  }
  
  # data before 
  df_before <- dplyr::group_by(data_before, pair, side, price) 
  df_before <- dplyr::summarise(df_before, quantity = sum(quantity), .groups = "rowwise")
  df_before <- dplyr::ungroup(df_before) 
  # data after 
  df_after <- dplyr::group_by(data_after, pair, side, price) 
  df_after <- dplyr::summarise(df_after, quantity = sum(quantity), .groups = "rowwise")
  df_after <- dplyr::ungroup(df_after) 
  # merge data before with data after 
  df_out <- dplyr::full_join(df_before, df_after, by = c("pair", "side", "price"))
  df_out <- dplyr::mutate(df_out, quantity = ifelse(is.na(quantity.y), quantity.x, quantity.y))
  df_out <- dplyr::mutate(df_out, date = data_after$date[1])
  df_out <- dplyr::select(df_out, date, pair, side, price, quantity)
  # keep only levels with a quantity > 0 
  df_out <- dplyr::filter(df_out, quantity > 0)
  
  return(df_out)
}