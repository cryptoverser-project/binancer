#' Binance Order Book Snapshot
#'
#' Retrieve a snapshot of the order book for a specified trading pair. 
#'
#' @param pair Character, specifying the trading pair of interest, e.g., "BTCBUSD".
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot api](https://binance-docs.github.io/apidocs/spot/en/#order-book).
#'   - "fapi": For [Futures USD-M api](https://binance-docs.github.io/apidocs/futures/en/#order-book).
#'   - "dapi": For [Futures Coin-M api](https://binance-docs.github.io/apidocs/delivery/en/#order-book).
#'   - "eapi": For [Options api](https://binance-docs.github.io/apidocs/voptions/en/#order-book).
#'
#' @param quiet Logical, specifying whether to suppress informational messages. Default is FALSE.
#'
#' @return A tibble (data frame) with 6 columns:
#'   - `date`: Datetime, the time of the order book snapshot.
#'   - `market`: Character, the selected API.
#'   - `pair`: Character, the selected trading pair.
#'   - `side`: Character, the side of the order. It is "ASK" for sell orders and "BID" for buy orders.
#'   - `price`: Numeric, the price levels in the order book.
#'   - `quantity`: Numeric, the quantity correspondent to each price level.
#'
#' @details This function allows you to access the current state of the order book, including both asks (sell orders) and bids (buy orders). 
#'
#' @examples
#'
#' # Get the order book for BTCBUSD in Spot market.
#' binance_depth(pair = "BTCBUSD", api = "spot")
#'
#' # Get the order book for BTCBUSD in Futures USD-M market.
#' binance_depth(pair = "BTCBUSD", api = "fapi")
#'
#' # Get the order book for BTCUSD_PERP in Futures COIN-M market.
#' binance_depth(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Get the order book for a Put option on BTCUSDT 
#' binance_depth(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @export
#'
#' @rdname binance_depth
#'
#' @name binance_depth

binance_depth <- function(pair, api = "spot", quiet = FALSE){
  
  # api function name 
  fun_name <- "binance_api_depth"
  
  # safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(binance_api_depth, args = list(pair = pair, api = api, quiet = quiet)))

  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    warning(response$error)
  } else {
    return(response$result)
  }
}

# api functions -----------------------------------------------------
binance_api_depth <- function(pair, api = "spot", quiet = FALSE){
  
  # General Check: api default argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      warning(wrn)
    }
  } else {
    pair_name <- toupper(pair)
  }
  
  # match the market correspondent to selected api 
  market = dplyr::case_when(
    api == "spot" ~ "spot", 
    api == "fapi" ~ "usd-m", 
    api == "dapi" ~ "coin-m", 
    api == "eapi" ~ "options"
  )
  
  # api query 
  query <- list(symbol = pair_name, limit = ifelse(api == "spot", 5000, 1000))
  # api response
  response <- binance_api(api = api, path = "depth", query = query)
  
  # IF response is NOT empty then cleaning 
  if(!purrr::is_empty(response)){
    
    # assign standard columns names
    colnames(response$bids) <- c("price", "quantity")
    colnames(response$asks) <- c("price", "quantity")
    
    # bid and ask data 
    df_bid <- dplyr::as_tibble(response$bids)
    df_ask <- dplyr::as_tibble(response$asks)
    
    # update datetime
    update_time <- Sys.time()
    
    # bid-ask orders 
    df_bid <- dplyr::mutate(df_bid, side = "BID", price = as.numeric(price), quantity = as.numeric(quantity))
    df_ask <- dplyr::mutate(df_ask, side = "ASK", price = as.numeric(price), quantity = as.numeric(quantity))
    # order-book
    response <- dplyr::bind_rows(df_bid, df_ask)
    
    # add extra information (date, pair and market)
    response <- dplyr::mutate(response, date = update_time, pair = pair_name, market = market)
    # select and reorder variables 
    response <- dplyr::select(response, date, market, pair, side, price, quantity)
    # arrange by price in descending order 
    response <- dplyr::arrange(response, dplyr::desc(price))
  }
  
  # attributes 
  attr(response, "ip_weight") <- 50
  attr(response, "api") <- api
  return(response)
}