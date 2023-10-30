#' Get current general Binance account information, without balances
#' @return data.table
#' @export

binance_account <- function() {
  
  # GET call (signed)
  account <- binance_query(api = 'spot', path = 'account', sign = TRUE)
  
  # Account informations 
  idx_not_list <- !purrr::map_lgl(account, is.list)
  df_account <- dplyr::bind_rows(account[idx_not_list])
  df_account <- binance_formatter(df_account)
  # Account commission rates  
  df_commission <- dplyr::as_tibble(account$commissionRates)
  df_commission <- binance_formatter(df_commission)
 
  # Account balance  
  df_balance <- dplyr::as_tibble(account$balances)
  df_balance <- binance_formatter(df_balance)
  df_balance <- dplyr::filter(df_balance, free > 0 | locked > 0)
  
  structure(
    list(
      info = dplyr::bind_cols(df_account, df_commission),
      balance = df_balance
    )
  )
}

#' Get trades informations 
#' @return data.table
#' @export

binance_account_trades <- function(symbol, from, to, from_id, limit = 1000) {
  
  query <- list(symbol = symbol)
  
  if (!missing(limit)) {
    stopifnot(limit <= 1000L)
    query$limit <- limit
  }
  if (!missing(from_id)) {
    query$fromId <- from_id
  }
  if (!missing(from)) {
    query$startTime <- format(as.numeric(as.POSIXct(from)) * 1e3, scientific = FALSE)
  }
  if (!missing(to)) {
    query$endTime <- format(as.numeric(as.POSIXct(to)) * 1e3, scientific = FALSE)
  }
  
  trades <- binance_query(path = 'myTrades', query = query, sign = TRUE)
  trades <- dplyr::as_tibble(trades)
  trades <- binance_formatter(trades)
  cols_subset <- c("trade_id", "order_id", "date", "pair", "side", "price", "quantity", "commission", "commission_asset", "is_maker")
  trades <- trades[, cols_subset]
  
  return(trades)
}

