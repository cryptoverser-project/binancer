#' Order Book
#'
#' Create and structure an order book from depth data. 
#'
#' @param data A tibble with at least 3 columns (price, quantity and side).
#'
#' @param min_price Numeric, the minimum price for the aggregation of order book.
#'
#' @param max_price Numeric, the maximum price for the aggregation of order book.
#'
#' @param levels The number of levels for the aggregated order book.
#'
#' @param trades A tibble with trades data. 
#'
#' @param as_datatable Logical, if `TRUE` return the order book as a datatable. Default is `FALSE`.
#'
#' @return A tibble with depth data in the specified price range.
#'
#' @export
#' @rdname OrderBook
#' @name OrderBook

OrderBook <- function(data = NULL, min_price = NULL, max_price = NULL, levels = NULL, trades = NULL, as_datatable = FALSE){
  
  # Default min_price
  if (is.null(min_price) & !is.null(data)) {
    min_price <- min(data$price, na.rm = TRUE)
  } else if(is.null(min_price)){
    min_price <- 1
  } else {
    min_price <- as.numeric(min_price)
  }
  
  # Default max_price
  if (is.null(max_price) & !is.null(data)) {
    max_price <- max(data$price, na.rm = TRUE)
  } else if(is.null(max_price)){
    max_price <- 10
  } else {
    max_price <- as.numeric(max_price)
  }
  
  # default number of levels 
  levels <- ifelse(is.null(levels), 20, levels)
  seq_price <- seq(min_price, max_price, (max_price-min_price)/(levels-1))
  
  # initialize order book 
  order_book <- dplyr::tibble(price = seq_price, buy = 0, bid = 0, ask = 0, sell = 0, net = 0)
  
  # IF no data return empty order_book
  if (is.null(data)) {
    order_book <- dplyr::arrange(order_book, dplyr::desc(price))
    return(order_book)
  }
  
  # fill levels
  original_order_book <- data
  for(i in 1:nrow(order_book)){
    if (i == 1) {
      # first level quantity will be the sum of all quantity below first level 
      index <- original_order_book$price <= order_book[i,]$price
    } else if(i == nrow(order_book)) {
      # last level quantity will be the sum of all quantity above last level 
      index <- original_order_book$price > order_book[i,]$price
    } else {
      # a level quantity will be the sum of all quantity in between previous level and actual level  
      index <- original_order_book$price > order_book[i-1,]$price & original_order_book$price <= order_book[i,]$price
    }
    
    index <- which(index)
    if(purrr::is_empty(index)){
      order_book[i,]$ask <- 0 # ASK
      order_book[i,]$bid <- 0 # BID
    } else {
      new_data <- original_order_book[index, ]
      order_book[i,]$ask <- sum(dplyr::filter(new_data, side == "ASK")$quantity, na.rm = TRUE) # ASK
      order_book[i,]$bid <- sum(dplyr::filter(new_data, side == "BID")$quantity, na.rm = TRUE) # BID
    }
  }
  
  if (!is.null(trades)) {
    for(i in 1:nrow(order_book)){
      if (i == 1) {
        # first level quantity will be the sum of all quantity below first level 
        index <- trades$price <= order_book[i,]$price
      } else if(i == nrow(order_book)) {
        # last level quantity will be the sum of all quantity above last level 
        index <- trades$price > order_book[i,]$price
      } else {
        # a level quantity will be the sum of all quantity in between previous level and actual level  
        index <- trades$price > order_book[i-1,]$price & trades$price <= order_book[i,]$price
      }
      new_data <- trades[which(index),]
      order_book[i,]$buy <- sum(dplyr::filter(new_data, side == "BUY")$quantity, na.rm = TRUE)  # BUY trades 
      order_book[i,]$sell <- sum(dplyr::filter(new_data, side == "SELL")$quantity, na.rm = TRUE) # SELL trades 
      order_book[i,]$net <- order_book[i,]$buy - order_book[i,]$sell # NET quantity (BUY - SELL)
    }
  }
  
  # arrange orderbook by descending price 
  order_book <- dplyr::arrange(order_book, dplyr::desc(price))
  
  # IF as_datatable = TRUE display a datatable
  if (as_datatable) {
    dt_order_book <- DT::datatable(
      order_book, options = list(
        lengthMenu = list(c(20, 50, -1), c('5', '15', 'All')),
        pageLength = 50,
        searching = FALSE,
        paging = FALSE,
        processing = FALSE
      )
    )
    dt_order_book <- DT::formatRound(dt_order_book, c("bid", "ask", "price"), 2)
    # format for BIDs
    dt_order_book <- DT::formatStyle(dt_order_book, "bid",
                                     background = DT::styleColorBar(range(order_book$bid), 'green'),
                                     backgroundSize = '98% 88%',
                                     backgroundRepeat = 'no-repeat',
                                     backgroundPosition = 'center')
    dt_order_book <- DT::formatStyle(dt_order_book, 'bid', backgroundColor = "#00E679", color = "white")
    # format for ASKs 
    dt_order_book <-DT::formatStyle(dt_order_book, "ask",
                                    background = DT::styleColorBar(range(order_book$ask), '#961E0C'),
                                    backgroundSize = '98% 88%',
                                    backgroundRepeat = 'no-repeat',
                                    backgroundPosition = 'center')
    dt_order_book <- DT::formatStyle(dt_order_book, 'ask', backgroundColor = "#FF5F47", color = "white")
    return(dt_order_book)
  }
  return(order_book)
}

