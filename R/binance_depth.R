#' Binance Order Book Snapshot
#'
#' Retrieve a snapshot of the order book for a specified trading pair. 
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#order-book).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#order-book).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#order-book).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#order-book).
#'
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @usage 
#' binance_depth(pair, 
#'               api, 
#'               quiet = FALSE)
#'
#' @return A \code{\link[=data.frame-class]{data.frame}} with 7 columns:
#'   - `last_update_id`: Integer, id of the last snapshot.
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, time of the snapshot.
#'   - `market`: Character, reference API.
#'   - `pair`: Character, trading pair.
#'   - `side`: Character, side of the order. Can be `"ASK"` or `"BID"`.
#'   - `price`: Numeric, price levels.
#'   - `quantity`: Numeric, quantity in each price level.
#'   
#'   
#' @examples
#'
#' # Get the order book for BTCUSDT
#' binance_depth(pair = "BTCUSDT", api = "spot")
#' binance_depth(pair = "BTCUSDT", api = "fapi")
#'
#' # Get the order book for BTCUSD_PERP
#' binance_depth(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Get the order book for a put option on BTC 
#' binance_depth(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @export
#' 
#' @rdname binance_depth
#' @name binance_depth

binance_depth <- function(pair, api, quiet = FALSE){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The "pair" argument is missing with no default argument.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      msg <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }

  # Create API query
  query <- list(symbol = pair, limit = ifelse(api == "spot", 5000, 1000))
  # GET call
  response <- binance_query(api = api, path = "depth", method = "GET", query = query, quiet = quiet)
  
  if(!is.null(response$code)){
    return(NULL)
  } else {
    # Save update time
    update_time <- Sys.time()
    # Save last update id 
    last_update_id <- ifelse(api == "eapi", as.numeric(response$u), as.numeric(response$lastUpdateId))
    
    # Standard columns names
    colnames(response$bids) <- c("price", "quantity")
    colnames(response$asks) <- c("price", "quantity")
    # Bid data 
    df_bid <- dplyr::as_tibble(response$bids)
    df_bid$side <- "BID"
    df_bid <- binance_formatter(df_bid)
    # Ask data 
    df_ask <- dplyr::as_tibble(response$asks)
    df_ask$side <- "ASK"
    df_ask <- binance_formatter(df_ask)
    # Depth data 
    response <- dplyr::bind_rows(df_bid, df_ask)
    # Add extra information (date, pair and market)
    response$date <- update_time
    response$last_update_id <- last_update_id
    response$pair <- pair
    response$market <- api
    # Select and reorder variables 
    response <- response[,c("last_update_id", "date", "market", "pair", "side", "price", "quantity")]
    # Arrange by price (descending) 
    response <- response[order(response$price, decreasing = TRUE),]
  }

  attr(response, "ip_weight") <- 50
  attr(response, "api") <- api
  
  return(response)
}
