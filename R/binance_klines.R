#' Get Klines Data from Binance
#'
#' Get candlestick data for a trading pair with Binance API. 
#'
#' @param pair Character, trading pair, e.g. "BTCUSDT".
#' 
#' @param api Character, reference API. Available options are:
#'   - `"spot"`: for [Spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - `"fapi"`: for [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#'   - `"dapi"`: for [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#kline-candlestick-data).
#'   - `"eapi"`: for [Options API](https://binance-docs.github.io/apidocs/voptions/en/#kline-candlestick-data).
#'   
#' @param interval Character, default is `"1d"`. Time interval for Klines data. Available intervals are: 
#'   - `secondly`: `"1s"`, available only for `api = "spot"`.
#'   - `minutely`: `"1m"`, `"3m"`, `"5m"`, `"15m"` and `"30m"`;
#'   - `hourly`: `"1h"`, `"2h"`, `"4h"`, `"6h"`, `"8h"` and `"12h"`;
#'   - `daily`: `"1d"` and `"3d"`;
#'   - `weekly`: `"1w"`;
#'   - `monthly`: `"1M"`.
#'   
#' @param from Character or \code{"\link[=POSIXt-class]{POSIXt}"} object. Start time for historical data. 
#' Default is `missing` and will be used as start date `Sys.time()-lubridate::days(1)`.
#' 
#' @param to Character or \code{"\link[=POSIXt-class]{POSIXt}"} object. End time for historical data.
#' Default is `missing` and will be used as end date \code{"\link[=Sys.time]{Sys.time()}"}.
#' 
#' @param contract_type Character, used only if `api = "dapi"`. Available contract type are: 
#'   - `"perpetual"`: perpetual futures.
#'   - `"current_quarter"`: futures contracts with maturity in the current quarter.
#'   - `"next_quarter"`: futures contracts with maturity in the next quarter.
#'   
#' @param uiKlines Logical, if `TRUE` return data in UIklines format. Default is `FALSE`.
#' 
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`
#'
#' @return A tibble (data frame) with the following 13 columns:
#'   - `date`: \code{"\link[=POSIXt-class]{POSIXt}"}, the opening date of the candle.
#'   - `date_close`: \code{"\link[=POSIXt-class]{POSIXt}"}, the closing date of the candle.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, selected trading pair.
#'   - `open`: Numeric, open price or price in `date`.
#'   - `high`: Numeric, highest price from `date` up to `date_close`.
#'   - `low`: Numeric, lowest price from `date` up to `date_close`.
#'   - `close`: Numeric, close price or price in `date_close`.
#'   - `volume`: Numeric, volume in asset value.
#'   - `volume_quote`: Numeric, volume in quote value.
#'   - `trades`: Numeric, number of trades from `date` up to `date_close`.
#'   - `taker_buy`: Numeric, buy volume in asset value.
#'   - `taker_buy_quote`: Numeric, buy volume in quote value.
#'
#' @usage 
#' binance_klines(pair,
#'                api = "spot",
#'                interval, 
#'                from, 
#'                to,
#'                contract_type
#'                uiKlines = FALSE
#'                quiet = FALSE)
#'                   
#' @details The IP weight for this API call is 1, and the data source is memory.
#' 
#' @examples
#' 
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

binance_klines <- function(pair, api = "spot", interval, from, to, contract_type, uiKlines = FALSE, quiet = FALSE){
  
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
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
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
    if (api != "spot"){
      av_int <- av_int[-1] 
    }
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
  if (missing(contract_type) || is.null(contract_type)) {
    contract_type <- "PERPETUAL"
    if (!quiet) {
      wrn <- paste0('The "contract_type" argument is missing, default is ', '"', tolower(contract_type), '"')
      cli::cli_alert_warning(wrn)
    }
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

  # Check when uiKlines = TRUE AND contract_type is NULL
  if(uiKlines & (missing(contract_type) || is.null(contract_type))){
    contract_type <- "PERPETUAL"
    if (!quiet) {
      msg <- paste0('When `uiklines` is TRUE `contract_type` must be specified. Default is "perpetual"`.')
      cli::cli_alert_warning(msg)
    }
  }
  
  # Check when uiKlines = FALSE AND contract_type is not NULL
  if(!uiKlines & !(missing(contract_type) || is.null(contract_type))){
    uiKlines <- TRUE
    if (!quiet) {
      msg <- paste0('When `contract_type` is specified `uiklines` must be TRUE.')
      cli::cli_alert_warning(msg)
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
    # query 
    api_query <- list(pair = pair, 
                      contractType = NULL, 
                      startTime = NULL, 
                      interval = interval, 
                      endTime = end_time, 
                      limit = 1500)
    
    # query depending on uiKlines
    if (uiKlines) {
      api_query$contractType <- contractType 
    }
  
    # api GET call 
    api_path <- ifelse(uiKlines, "continuousKlines", "klines")
    new_data <- binance_api(api = "fapi", path = api_path, query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
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
    i <- i + 1
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
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "fapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

# Klines implementation for futures COIN-M api 
binance_dapi_klines <- function(pair, interval, from, to, contract_type, uiKlines = FALSE, quiet = FALSE){
 
  uiKlines <- FALSE # not work with uiKlines = TRUE ?
  # Check when uiKlines = TRUE AND contract_type is NULL
  if(uiKlines & (missing(contract_type) || is.null(contract_type))){
    contract_type <- "PERPETUAL"
    if (!quiet) {
      msg <- paste0('When `uiklines` is TRUE `contract_type` must be specified. Default is "perpetual"`.')
      cli::cli_alert_warning(msg)
    }
  }
  
  # Check when uiKlines = FALSE AND contract_type is not NULL
  if(!uiKlines & !(missing(contract_type) || is.null(contract_type))){
    uiKlines <- TRUE
    if (!quiet) {
      msg <- paste0('When `contract_type` is specified `uiklines` must be TRUE.')
      cli::cli_alert_warning(msg)
    }
  }
  
  # format start and end time
  format_time <- function(x){
    paste0(trunc(as.integer(x)), "000")
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as.integer(to)*1000
  start_time <- format_time(from)
  end_time <- format_time(to)
  last_date <- 0
  while(condition){
    
    if (uiKlines) {
      api_path <- "continuousKlines"
      api_query <- list(pair = pair, 
                        contractType = contract_type, 
                        interval = interval, 
                        startTime = NULL, 
                        endTime = end_time, 
                        limit = 1500)
    } else {
      api_path <- "klines"
      api_query <- list(symbol = pair, 
                        interval = interval, 
                        startTime = NULL,
                        endTime = end_time, 
                        limit = 1500)
    }
    # GET call 
    new_data <- binance_api(api = "dapi", path = api_path, query = api_query)
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
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "dapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  
  return(response)
} 

# Klines implementation for options api
binance_eapi_klines <- function(pair, interval, from, to, quiet = FALSE){
  
  # format start and end time
  format_time <- function(x){
    paste0(trunc(as.integer(x)), "000")
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  last_date <- as.integer(to)*1000
  start_time <- format_time(from)
  end_time <- format_time(to)
  last_date <- 0
  
  while(condition){
    # GET call
    api_query <- list(symbol = pair, startTime = NULL, interval = interval, endTime = end_time, limit = 1500)
    new_data <- binance_api(api = "eapi", path = "klines", query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$closeTime))
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
    # Rename and reorder 
    response <- dplyr::select(response, date = "openTime", date_close = "closeTime", 
                              market, pair, open, high, low, close, volume, 
                              trades = "tradeCount", taker_volume = "takerVolume", 
                              taker_amount = "takerAmount", amount)
    # Filter to be exactly in from-to range
    response <- dplyr::filter(response, date >= from & date <= to)
    # Arrange with respect to date
    response <- dplyr::arrange(response, date)
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "api") <- "eapi"
  attr(response, "ip_weight") <- i*1
  attr(response, "interval") <- interval
  
  return(response)
}


