#' Retrieve Aggregated Historical Trades
#'
#' Get aggregated historical trades data for a specified trading pair from the selected reference API.
#'
#' @param pair Character, trading pair, e.g. "BTCUSDT".
#'
#' @param api Character, reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#recent-trades-list).
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#recent-trades-list).
#'   - "dapi": For [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#recent-trades-list).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#recent-trades-list).
#'
#' @param from Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the start time for historical data. 
#' Default is `NULL` and will be used as start date `Sys.time()-lubridate::minutes(10)`.
#' @param to Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the end time for historical data.
#' Default is `NULL` and will be used as end date `Sys.time()`.
#' 
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`.
#'
#' @return A tibble with 9 columns:
#'   - `date`: Datetime, trade execution date.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, selected pair.
#'   - `price`: Numeric, trade price.
#'   - `quantity`: Numeric, trade quantity.
#'   - `side`: Character, trade side. Can be "SELL" or "BUY".
#'   - `agg_id`: Integer, aggregated trade Id.
#'   - `first_id`: Integer, first trade Id for aggregation.
#'   - `last_id`: Integer, last trade Id for aggregation.
#'
#' @examples
#'
#' # Retrieve the last 10 minutes of trades for BTCUSDT in Spot market
#' binance_trades(pair = "BTCUSDT", api = "spot", from = NULL, to = NULL)
#' 
#' # Get trades for LAZIOUSDT in a date range in Spot market.
#' binance_trades(pair = "LAZIOUSDT", api = "spot", from = "2023-01-01", to = "2023-01-02")
#'
#' # Get the last 10 minutes of trades for BTCUSDT in USD-M market
#' binance_trades(pair = "BTCUSDT", api = "fapi", from = NULL, to = NULL)
#'
#' # Get last 10 minutes of trades for BTCUSD_PERP in Coin-M market.
#' binance_trades(pair = "BTCUSD_PERP", api = "dapi", from = NULL, to = NULL)
#'
#' @export
#'
#' @rdname binance_trades
#' @name binance_trades

binance_trades <- function(pair, api, from, to, quiet = FALSE){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      wrn <- paste0('The pair argument is missing with no default.')
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
    api <- match.arg(api, choices = c("spot", "fapi", "dapi"))
  }
  
  # Check "from" argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::days(1)
    if (!quiet) {
      wrn <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    from <- as.POSIXct(from, origin = "1970-01-01")
  }
  
  # Check "to" argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.time() 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    to <- as.POSIXct(to, origin = "1970-01-01")
  }

  # Function name 
  fun_name <- paste0("binance_", api, "_trades")
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = list(pair = pair, from = from, to = to, quiet = quiet)))
  # GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_warning(response$error)
  } else {
    return(response$result) 
  }
}

# Trades implementation for spot api 
binance_spot_trades <- function(pair, from, to, quiet = FALSE){
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  first_date <- as.integer(from)*1000
  while(condition){
    # GET call 
    api_query <- list(symbol = pair, startTime = start_time, endTime = NULL, limit = 1000)
    new_data <- binance_api(api = "spot", path = c("aggTrades"), query = api_query)
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    colnames(response[[i]]) <- c("agg_id", "price", "quantity", "first_id",
                                 "last_id", "date", "side", "isBestMatch")
    
    # Extract the last date
    last_date <- max(as.numeric(response[[i]]$date))
    # Break if last_date is greater than end_time
    condition <- last_date < end_time & first_date < last_date
    first_date <- last_date # needed avoid infinite loops 
    # use the first_date as new endTime
    start_time <- paste0(trunc(last_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              market = "spot",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              price = as.numeric(price),
                              quantity = as.numeric(quantity),
                              side = ifelse(side, "SELL", "BUY"))
    # Reorder columns
    response <- dplyr::select(response, date, market, pair, price, quantity, side, agg_id, first_id, last_id)
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "spot"
  return(response)
}

# Trades implementation for futures USD-m api 
binance_fapi_trades <- function(pair, from, to, quiet = FALSE){
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  first_date <- as.integer(from)*1000
  while(condition){
    # GET call 
    api_query <- list(symbol = pair, startTime = start_time, endTime = NULL, limit = 1000)
    new_data <- binance_api(api = "fapi", path = "aggTrades", query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    colnames(response[[i]]) <- c("agg_id", "price", "quantity", "first_id", "last_id", "date", "side")
    
    # Extract the last date
    last_date <- max(as.numeric(response[[i]]$date))
    # Break if last_date is greater than end_time
    condition <- last_date < end_time & first_date < last_date
    first_date <- last_date # needed avoid infinite loops 
    # Use the first_date as new endTime
    start_time <- paste0(trunc(last_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              market = "usd-m",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              price = as.numeric(price),
                              quantity = as.numeric(quantity),
                              side = ifelse(side, "SELL", "BUY"))
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::select(response, date, market, pair, price, quantity, side, agg_id, first_id, last_id)
  }
  
  attr(response, "ip_weight") <- i * 20
  attr(response, "api") <- "usd-m"
  return(response)
}

# Trades implementation for futures COIN-m api 
binance_dapi_trades <- function(pair, from, to, quiet = FALSE){
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  first_date <- as.integer(from)*1000
  while(condition){
    # GET call 
    api_query <- list(symbol = pair, startTime = start_time, endTime = NULL, limit = 1000)
    new_data <- binance_api(api = "dapi", path = c("aggTrades"), query = api_query)
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    colnames(response[[i]]) <- c("agg_id", "price", "quantity", "first_id", "last_id", "date", "side")
    
    # Extract the last date
    last_date <- max(as.numeric(response[[i]]$date))
    # Break if last_date is greater than end_time
    condition <- last_date < end_time & first_date < last_date
    first_date <- last_date # needed avoid infinite loops 
    # Use the first_date as new endTime
    start_time <- paste0(trunc(last_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              market = "coin-m",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              price = as.numeric(price),
                              quantity = as.numeric(quantity),
                              side = ifelse(side, "SELL", "BUY"))
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::select(response, date, market, pair, price, quantity, side, agg_id, first_id, last_id)
  }
  
  attr(response, "ip_weight") <- i * 20
  attr(response, "api") <- "coin-m"
  return(response)
}

