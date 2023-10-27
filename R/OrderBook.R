#' Order Book
#'
#' Create and structure an order book from depth data. 
#'
#' @param data A \code{\link[=data.frame-class]{data.frame}} with at least 3 columns (`price`, `quantity` and `side`).
#'
#' @param min_price Numeric. Minimum price for the aggregation of order book.
#'
#' @param max_price Numeric. Maximum price for the aggregation of order book.
#'
#' @param levels Integer. Number of levels for the aggregated order book.
#'
#' @param trades A \code{\link[=data.frame-class]{data.frame}} containing trades data. 
#'
#' @param as_datatable Logical, if `TRUE` return the order book as a datatable. Default is `FALSE`.
#' 
#' @usage 
#' OrderBook(data = NULL, 
#'           min_price = NULL, 
#'           max_price = NULL, 
#'           levels = NULL, 
#'           trades = NULL, 
#'           as_datatable = FALSE)
#'
#' @return A \code{\link[=data.frame-class]{data.frame}} or \code{\link[=DT::datatables-class]{datatables}} object. 
#' Depth data aggregated for a specified price range.
#'
#' @examples 
#'  
#' # Generate an order book for BTCUSDT 
#' depth_data <- binance_depth("BTCUSDT", api = "spot")
#' depth <- 0.01
#' best_ask <- min(depth_data[depth_data$side == "ASK",]$price)
#' best_bid <- max(depth_data[depth_data$side == "BID",]$price)
#' OrderBook(data = depth_data, 
#'           min_price = best_bid*(1 - depth), 
#'           max_price = best_ask*(1 + depth), 
#'           levels = 10, 
#'           trades = NULL, 
#'           as_datatable = FALSE)
#'
#' @export
#' 
#' @rdname OrderBook
#' @name OrderBook

OrderBook <- function(data = NULL, min_price = NULL, max_price = NULL, levels = NULL, trades = NULL, as_datatable = FALSE){
  
  # Check "min_price" and "max_price" arguments 
  if (is.null(data)){
    min_price <- ifelse(is.null(min_price), 1, as.numeric(min_price))
    max_price <- ifelse(is.null(max_price), 1000000, as.numeric(max_price))
  } else {
    min_price <- ifelse(is.null(min_price), min(data$price, na.rm = TRUE), as.numeric(min_price))
    max_price <- ifelse(is.null(max_price), max(data$price, na.rm = TRUE), as.numeric(max_price))
  }
  
  # Check "levels" argument 
  levels <- ifelse(is.null(levels), 20, levels)
  # Compute price levels 
  seq_price <- seq(min_price, max_price, (max_price - min_price)/(levels-1))
  
  # initialize order_book object
  order_book <- dplyr::tibble(price = seq_price, 
                              buy = 0, bid = 0, 
                              ask = 0, sell = 0, 
                              net = 0)
  
  # Check if data is NULL return order_book
  if (is.null(data)) {
    order_book <- dplyr::arrange(order_book, dplyr::desc(price))
    return(order_book)
  }
  
  # Aggregate depth data
  for(i in 1:nrow(order_book)){
    if (i == 1) {
      # First quantity will be the sum of all quantity below first_level 
      first_level <- order_book[i,]$price
      index <- data$price <= first_level
    } else if(i == nrow(order_book)) {
      # Last quantity will be the sum of all quantity above last_level 
      last_level <- order_book[i,]$price
      index <- data$price > last_level
    } else {
      # A middle quantity is the sum of all quantities in between prev_level and next_level (current) 
      prev_level <- order_book[i - 1, ]$price
      next_level <- order_book[i, ]$price
      index <- data$price > prev_level & data$price <= next_level
    }
    
    index <- which(index)
    if (purrr::is_empty(index)) {
      # Set levels to 0
      order_book[i,]$ask <- 0 
      order_book[i,]$bid <- 0
    } else {
      depth_data <- data[index, ]
      order_book[i,]$ask <- sum(dplyr::filter(depth_data, side == "ASK")$quantity, na.rm = TRUE) # ASK
      order_book[i,]$bid <- sum(dplyr::filter(depth_data, side == "BID")$quantity, na.rm = TRUE) # BID
    }
  }
  
  # Aggregate trades data
  if (!is.null(trades)) {
    for(i in 1:nrow(order_book)){
      if (i == 1) {
        # First level is the sum of all quantities below first_level 
        first_level <- order_book[i,]$price
        index <- trades$price <= first_level
      } else if(i == nrow(order_book)) {
        # Last level is the sum of all quantities above last_level 
        last_level <- order_book[i,]$price
        index <- trades$price > last_level
      } else {
        # Middle levels is the sum of all quantities in between prev_level and next_level (current) 
        prev_level <- order_book[i - 1, ]$price
        next_level <- order_book[i, ]$price
        index <- trades$price > prev_level & trades$price <= next_level
      }
      
      index <- which(index)
      if (purrr::is_empty(index)) {
        # Set levels to 0
        order_book[i,]$buy <- 0 
        order_book[i,]$sell <- 0
      } else {
        trades_data <- data[index, ]
        # Aggregate BUY trades 
        order_book[i,]$buy <- sum(dplyr::filter(trades_data, side == "BUY")$quantity, na.rm = TRUE)
        # Aggregate SELL trades 
        order_book[i,]$sell <- sum(dplyr::filter(trades_data, side == "SELL")$quantity, na.rm = TRUE)
        # Compute net quantity traded as NET = BUY - SELL  
        order_book[i,]$net <- order_book[i,]$buy - order_book[i,]$sell
      }
    }
  }
  
  # Arrange orderbook by descending price 
  order_book <- dplyr::arrange(order_book, dplyr::desc(price))
  
  # If as_datatable = TRUE return a datatable
  if (as_datatable) {
    dt_order_book <- DT::datatable(
      order_book, 
      options = list(
        lengthMenu = list(c(20, 50, -1), c('5', '15', 'All')),
        pageLength = 50,
        searching = FALSE,
        paging = FALSE,
        processing = FALSE
      )
    )
    # Round numeric columns 
    dt_order_book <- DT::formatRound(dt_order_book, c("bid", "ask", "price", "buy", "sell", "net"), 2)
    # Change background color 
    dt_order_book <- DT::formatStyle(dt_order_book, c(1:nrow(order_book)),target='row', backgroundColor = "#F3F7F9")
    # Format BID side 
    dt_order_book <- DT::formatStyle(dt_order_book, "bid",
                                     background = DT::styleColorBar(range(order_book$bid), 'green'),
                                     backgroundSize = '98% 88%',
                                     backgroundRepeat = 'no-repeat',
                                     backgroundPosition = 'center')
    dt_order_book <- DT::formatStyle(dt_order_book, 'bid', backgroundColor = "#00E679", color = "white")
    # Format ASK side 
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

