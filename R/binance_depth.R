#' Binance Order Book Snapshot
#'
#' Retrieve a snapshot of the order book for a specified trading pair. 
#'
#' @param pair Character, trading pair, e.g. `"BTCUSDT"` or `"BTCUSD_PERP"`.
#'
#' @param api Character, reference API. Available options are:
#'   - `"spot"`: for [Spot API](https://binance-docs.github.io/apidocs/spot/en/#order-book).
#'   - `"fapi"`: for [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#order-book).
#'   - `"dapi"`: for [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#order-book).
#'   - `"eapi"`: for [Options API](https://binance-docs.github.io/apidocs/voptions/en/#order-book).
#'
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`.
#' 
#' @usage 
#' binance_depth(pair, 
#'               api, 
#'               quiet = FALSE)
#'
#' @return A tibble with 7 columns:
#'   - `last_update_id`: Integer, id of the last snapshot.
#'   - `date`: \code{"\link[=POSIXt-class]{POSIXt}"}, time of the snapshot.
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
#'
#' # Get the order book for BTCUSDT
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
      wrn <- paste0('The "pair" argument is missing with no default argument.')
      cli::cli_abort(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  
  # safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(binance_api_depth, args = list(pair = pair, api = api, quiet = quiet)))
  # GET call
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_danger(response$error)
  } else {
    return(response$result)
  }
}

# Depth implementation for all APIs
binance_api_depth <- function(pair, api, quiet = FALSE){
  
  # GET call
  query <- list(symbol = pair, limit = ifelse(api == "spot", 5000, 1000))
  response <- binance_api(api = api, path = "depth", query = query)
  #return(response)
  # Save update time
  update_time <- Sys.time()
  # Save last update id 
  last_update_id <- ifelse(api == "eapi", as.numeric(response$u), as.numeric(response$lastUpdateId))
  
  if (!purrr::is_empty(response)) {
    # Standard columns names
    colnames(response$bids) <- c("price", "quantity")
    colnames(response$asks) <- c("price", "quantity")
    # Bid data 
    df_bid <- dplyr::as_tibble(response$bids)
    df_bid <- dplyr::mutate(df_bid, 
                            side = "BID", 
                            price = as.numeric(price), 
                            quantity = as.numeric(quantity))
    # Ask data 
    df_ask <- dplyr::as_tibble(response$asks)
    df_ask <- dplyr::mutate(df_ask, 
                            side = "ASK", 
                            price = as.numeric(price), 
                            quantity = as.numeric(quantity))
    # Order-book
    response <- dplyr::bind_rows(df_bid, df_ask)
    # Add extra information (date, pair and market)
    market = dplyr::case_when(
      api == "spot" ~ "spot", 
      api == "fapi" ~ "usd-m", 
      api == "dapi" ~ "coin-m", 
      api == "eapi" ~ "options")
    response <- dplyr::mutate(response, 
                              date = update_time, 
                              pair = pair, 
                              market = market, 
                              last_update_id = last_update_id)
    # Select and reorder variables 
    response <- dplyr::select(response, last_update_id, date, market, pair, side, price, quantity)
    # Arrange by price in descending order 
    response <- dplyr::arrange(response, dplyr::desc(price))
  }
  
  attr(response, "ip_weight") <- 50
  attr(response, "api") <- api
  return(response)
}
