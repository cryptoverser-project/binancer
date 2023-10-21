#' Get Klines Data from Binance API
#'
#' Retrieve Klines (candlestick) data for a specific trading pair from the Binance API. This function supports various Binance API types, including spot, futures, and options.
#'
#' @param pair Character, the trading pair of interest, e.g., "BTCUSDT".
#' @param interval Character, the time interval for Klines data. 
#' Acceptable intervals include "1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", and "1M".
#' @param from Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the start time for data retrieval. If left unspecified (NULL), the default is Sys.Date()-1.
#' @param to Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the end time for data retrieval. If left unspecified (NULL), the default is Sys.Date().
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#kline-candlestick-data).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#kline-candlestick-data).
#' @param contract_type Character, only used if `api = "dapi"`. Choose from:
#'   - "perpetual": For perpetual futures.
#'   - "current_quarter": For futures with a maturity in the current quarter.
#'   - "next_quarter": For futures with a maturity in the next quarter.
#' @param uiKlines Logical, indicating whether to return data in UI Klines format.
#' @param quiet Logical, suppress informational messages if TRUE.
#'
#' @return A tibble (data frame) with the following 13 columns:
#'   - `date`: POSIXct, the opening date of the candle.
#'   - `date_close`: POSIXct, the closing date of the candle.
#'   - `market`: Character, the selected API.
#'   - `pair`: Character, the selected trading pair.
#'   - `open`: Numeric, the opening price.
#'   - `high`: Numeric, the highest price.
#'   - `low`: Numeric, the lowest price.
#'   - `close`: Numeric, the closing price.
#'   - `volume`: Numeric, the volume of the asset in asset value.
#'   - `volume_quote`: Numeric, the volume of the asset in quote value.
#'   - `trades`: Numeric, the number of trades.
#'   - `taker_buy`: Numeric, the buy volume of the asset in asset value.
#'   - `taker_buy_quote`: Numeric, the buy volume of the asset in quote value.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#' # Get 1-hour OHLC data for BTCUSDT in the spot market
#' binance_klines(api = "spot", pair = "BTCUSDT", interval = "1h")
#'
#' # Get 30-minute OHLC data for BNBUSDT in the perpetual market
#' binance_klines(api = "fapi", pair = "BNBUSDT", interval = "30m")
#'
#' # Get 15-minute OHLC data for BTCUSD_PERP in the futures market
#' binance_klines(api = "dapi", pair = "BTCUSD_PERP", interval = "15m")
#'
#' # Get 1-hour OHLC data for a put option on BTCUSDT with a strike of 30000 and maturity on 2024-06-28
#' binance_klines(api = "eapi", pair = "BTC-240628-30000-P", interval = "1h")
#'
#' @export
#' @rdname binance_klines
#' @name binance_klines

binance_klines <- function(pair, interval, from, to, api = "spot", contract_type, uiKlines = FALSE, quiet = FALSE){
  
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
  
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- "BTCUSDT"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    pair_name <- toupper(pair)
  }
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- "1d"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    av_int <- c("1s", "1m", "3m", "5m", "15m","30m","1h", "2h", 
                "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
    interval  <- match.arg(interval, choices = av_int)
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
  
  # Check "contract_type" argument
  if (missing(contract_type)) {
    contract_type <- "PERPETUAL"
  } else {
    contract_type <- match.arg(contract_type, choices = c("perpetual", "current_quarter", "next_quarter"))
    contract_type <- toupper(contract_type)
  }
  
  # Query parameters depends on api 
  if (api == "spot") {
    args <- list(pair = pair, interval = interval, from = from , to = to, uiKlines = uiKlines, quiet = quiet)
  } else if (api %in% c("fapi", "dapi")) {
    args <- list(pair = pair, interval = interval, from = from , to = to, contract_type = contract_type, uiKlines = uiKlines, quiet = quiet)
  } else {
    args <- list(pair = pair, interval = interval, from = from , to = to, quiet = quiet)
  }
  # Function name 
  fun_name <- paste0("binance_", api, "_klines")
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  # GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_danger(response$error)
  } else {
    return(response$result)
  }
}

# Klines implementation for spot api 
binance_spot_klines <- function(pair, interval, from , to, uiKlines = FALSE, quiet = FALSE){

  i <- 1
  last_date <- 0
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  while(condition){
    # GET call 
    api_path <- ifelse(uiKlines, "uiKlines", "klines")
    api_query <- list(symbol = pair, startTime = NULL, interval = interval, endTime = end_time, limit = 1000)
    new_data <- binance_api(api = "spot", path = api_path, query = api_query)
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    # Rename columns 
    colnames(response[[i]]) <- c("date", "open", "high", "low", "close", "volume",
                                 "date_close", "volume_quote", "trades", "taker_buy",
                                 "taker_buy_quote", "ignore")
    response[[i]] <- dplyr::as_tibble(response[[i]])
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$date))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- paste0(trunc(first_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              market = "spot",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              date_close = as.POSIXct(as.numeric(date_close)/1000, origin = "1970-01-01"),
                              open = as.numeric(open),
                              high = as.numeric(high),
                              low = as.numeric(low),
                              close = as.numeric(close),
                              volume = as.numeric(volume),
                              volume_quote = as.numeric(volume_quote),
                              trades = as.numeric(trades),
                              taker_buy = as.numeric(taker_buy),
                              taker_buy_quote = as.numeric(taker_buy_quote))
    # Remove ignore column and reorder 
    response <- dplyr::select(response, -ignore)
    response <- dplyr::select(response, date, date_close, market, pair, dplyr::everything())
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "spot"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

# Klines implementation for futures USD-M api 
binance_fapi_klines <- function(pair, interval, from, to, contract_type, uiKlines = FALSE, quiet = FALSE){

  # Control: IF uiKlines = TRUE AND contract_type is NULL -> PERPETUAL
  if(uiKlines & is.null(contract_type)){
    contract_type <- "PERPETUAL"
    if (!quiet) {
      wrn <- paste0('When `uiklines = TRUE` you should set `contract_type = "perpetual"`.')
      cli::cli_alert_warning(wrn)
    }
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as.integer(to)*1000
  start_time <- paste0(trunc(as.integer(from)), "000")
  end_time <- paste0(trunc(as.integer(to)), "000")
  last_date <- 0
  while(condition){
    
    # query depending on uiKlines
    if (uiKlines) {
      api_query <- list(pair = pair, contractType = contract_type, startTime = NULL , interval = interval, endTime = end_time, limit = 1500)
    } else {
      api_query <- list(symbol = pair, startTime = NULL , interval = interval, endTime = end_time, limit = 1500)
    }
  
    # api GET call 
    api_path <- ifelse(uiKlines, "continuousKlines", "klines")
    new_data <- binance_api(api = "fapi", path = api_path, query = api_query)
    
    # Break if new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    } else {
      response[[i]] <- new_data
    }
    # Rename columns 
    colnames(response[[i]]) <- c("date", "open", "high", "low", "close", "volume",
                                 "date_close", "volume_quote", "trades", "taker_buy",
                                 "taker_buy_quote", "ignore")
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$date))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- paste0(trunc(first_date/1000), "000")
    i = i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              market = "coin-m",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              date_close = as.POSIXct(as.numeric(date_close)/1000, origin = "1970-01-01"),
                              open = as.numeric(open),
                              high = as.numeric(high),
                              low = as.numeric(low),
                              close = as.numeric(close),
                              volume = as.numeric(volume),
                              volume_quote = as.numeric(volume_quote),
                              trades = as.numeric(trades),
                              taker_buy = as.numeric(taker_buy),
                              taker_buy_quote = as.numeric(taker_buy_quote))
    
    # Remove ignore column and reorder 
    response <- dplyr::select(response, -ignore)
    response <- dplyr::select(response, date, date_close, market, pair, dplyr::everything())
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
  }
  
  attr(response, "api") <- "fapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

# Klines implementation for futures COIN-M api 
binance_dapi_klines <- function(pair, interval, from, to, contract_type, uiKlines = TRUE, quiet = FALSE){
 
  # General Check: interval
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      warning(wrn)
    }
  } else {
    available_intervals <- c("1m", "3m", "5m", "15m","30m","1h", "2h", 
                             "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
    interval  <- match.arg(interval, choices = available_intervals)
  }
  
  uiKlines <- FALSE # not work with uiKlines = TRUE ?
  # Control: IF uiKlines = TRUE AND contract_type is NULL -> PERPETUAL
  if(uiKlines & is.null(contract_type)){
    contract_type <- "PERPETUAL"
    wrn <- paste0('If "continuous_klines = TRUE" the "contract_type = perpetual"')
    warning(wrn)
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as.integer(to)*1000
  start_time <- paste0(trunc(as.integer(from)), "000")
  end_time <- paste0(trunc(as.integer(to)), "000")
  last_date <- 0
  while(condition){
    
    # query
    if(uiKlines){
      api_path <- c("continuousKlines")
      api_query <- list(pair = pair, contractType = contract_type, interval = interval, 
                        startTime = NULL, endTime = end_time, limit = 1500)
    } else {
      api_path <- c("klines")
      api_query <- list(symbol = pair, interval = interval, startTime = NULL,
                        endTime = end_time, limit = 1500)
    }
    
    # api GET call 
    new_data <- binance_api(api = "dapi", path = api_path, query = api_query)
    
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    
    response[[i]] <- new_data
    # assign column names 
    colnames(response[[i]]) <- c("date", "open", "high", "low", "close", "volume",
                                 "date_close", "volume_quote", "trades", "taker_buy",
                                 "taker_buy_quote", "ignore")
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # extract the minimum date
    first_date <- min(as.numeric(response[[i]]$date))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- paste0(trunc(first_date/1000), "000")
    i = i + 1
  }
  
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              market = "coin-m",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              date_close = as.POSIXct(as.numeric(date_close)/1000, origin = "1970-01-01"),
                              open = as.numeric(open),
                              high = as.numeric(high),
                              low = as.numeric(low),
                              close = as.numeric(close),
                              volume = as.numeric(volume),
                              volume_quote = as.numeric(volume_quote),
                              trades = as.numeric(trades),
                              taker_buy = as.numeric(taker_buy),
                              taker_buy_quote = as.numeric(taker_buy_quote))
    
    # remove "ignore" column and reorder 
    response <- dplyr::select(response, -ignore)
    response <- dplyr::select(response, date, date_close, market, pair, dplyr::everything())
    
    # filter to be exactly in the ["from"-"to"] date range
    response <- dplyr::filter(response, date >= from & date <= to)
    
    # arrange with respect to "date" column
    response <- dplyr::arrange(response, date)
    
  }
  
  # Weigth and Api attributes
  attr(response, "api") <- "dapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  
  return(response)
  
} 

# Klines implementation for options api
binance_eapi_klines <- function(pair, interval, from, to, quiet = FALSE){
  
  # pair default argument 
  pair_name <- toupper(pair)
  
  # General Check: interval
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      warning(wrn)
    }
  } else {
    available_intervals <- c("1m", "3m", "5m", "15m","30m","1h", "2h", 
                             "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
    interval  <- match.arg(interval, choices = available_intervals)
  }

  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as.integer(to)*1000
  start_time <- paste0(trunc(as.integer(from)), "000")
  end_time <- paste0(trunc(as.integer(to)), "000")
  last_date <- 0
  while(condition){

    # query
    api_query <- list(symbol = pair_name, startTime = NULL, interval = interval, endTime = end_time, limit = 1500)
    # api GET call 
    new_data <- binance_api(api = "eapi", path = c("klines"), query = api_query)
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # extract the minimum date
    first_date <- min(as.numeric(response[[i]]$closeTime))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- first_date # avoid infinite loops 
    end_time <- paste0(trunc(first_date/1000), "000")
    i = i + 1
  }
  
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair_name,
                              market = "options",
                              openTime = as.POSIXct(as.numeric(openTime)/1000, origin = "1970-01-01"),
                              closeTime = as.POSIXct(as.numeric(closeTime)/1000, origin = "1970-01-01"),
                              open = as.numeric(open),
                              high = as.numeric(high),
                              low = as.numeric(low),
                              close = as.numeric(close),
                              volume = as.numeric(volume),
                              tradeCount = as.numeric(tradeCount),
                              takerVolume = as.numeric(takerVolume),
                              takerAmount = as.numeric(takerAmount),
                              amount = as.numeric(amount))
    
    # rename and reorder 
    response <- dplyr::select(response, date = "openTime", date_close = "closeTime", 
                              market, pair, open, high,  low,  close, volume, 
                              trades = "tradeCount", taker_volume = "takerVolume", 
                              taker_amount = "takerAmount", amount)
    # filter to be exactly in the ["from"-"to"] date range
    response <- dplyr::filter(response, date >= from & date <= to)
    # arrange with respect to "date" column
    response <- dplyr::arrange(response, date)
  }
  
  attr(response, "api") <- "eapi"
  attr(response, "ip_weight") <- i * 1
  attr(response, "interval") <- interval
  return(response)
}


