#' Historical Kline/Candlestick from database 
#' 
#' Historical Kline/Candlestick Data for a pair from the database.
#' 
#' @param pair Character, the trading pair of interest, e.g., "BTCUSDT".
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#'   - "dapi": For [Futures COIN-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#' 
#' @param interval Character, the time interval for Klines data. 
#' Available intervals are: "1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h" and "1d".
#' @param from Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the start time for data retrieval. 
#' If `NULL`, the default is `Sys.Date()-lubridate::days(4)`.
#' @param to Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the end time for data retrieval. 
#' If `NULL`, the default is `Sys.Date()-lubridate::days(2)`.
#' @param quiet Logical, suppress informational messages if `TRUE`. Default `FALSE`.
#'
#' @return Returns a `tibble`.
#'
#' @export
#' @name binance_vision_klines
#' @rdname binance_vision_klines

binance_vision_klines <- function(api = "spot", pair = NULL, interval = "1d", from = NULL, to = NULL, quiet = FALSE) {
  
  api <- match.arg(api, choices = c("spot", "fapi", "dapi"))
  market <- dplyr::case_when(
    api == "spot" ~ "spot",
    api == "fapi" ~ "usd-m",
    api == "dapi" ~ "coin-m"
  )
  # api function name 
  fun_name <- paste0("binance_vision_", ifelse(api == "spot", "spot", "futures"), "_klines")
  
  # safe call to avoid errors 
  if (api == "spot") {
    safe_fun <- purrr::safely(function(date){do.call(fun_name, args = list(pair = pair, day_date = date, interval = interval, quiet = quiet))} )
  } else {
    safe_fun <- purrr::safely(function(date){do.call(fun_name, args = list(pair = pair, day_date = date, market = market, interval = interval, quiet = quiet))})
  }
  
  if(is.null(from) & !is.null(to)){
    to <- as.Date(to)
    from <- to - 1
    seq_date <- c(from, to)
  } else if(!is.null(from) & is.null(to)){
    to <- Sys.Date()-2
    from <- as.Date(from)
    seq_date <- seq.Date(from, to, by = "days")
  } else if(!is.null(from) & !is.null(to)){
    to <- as.Date(to)
    from <- as.Date(from)
    seq_date <- seq.Date(from, to, by = "days")
  } else if(is.null(from) & is.null(to)){
    to <- Sys.Date()-2
    from <- Sys.Date()-4
    seq_date <- seq.Date(from, to, by = "days")
  } 
  
  response <- NULL
  response <- purrr::map_df(seq_date, ~safe_fun(.x)$result)
  
  # interval attribute
  attr(response, "interval") <- interval
  
  return(response)
  
}

# spot 
binance_vision_spot_klines <- function(pair = "BTCUSDT", day_date = Sys.Date()-2, interval = "1m", quiet = FALSE){
  
  # Pair and Interval
  pair_name <- toupper(pair)
  interval <- match.arg(interval, choices = c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d"))
  
  # Check default Date
  max_date <- Sys.Date()-2
  if (is.null(day_date) || day_date > max_date) {
    day_date <- max_date
    wrn <- paste0('The maximum date available is: ', day_date)
    if (!quiet) {
      warning(wrn)
    }
  } else {
    day_date <- as.Date(day_date)
  }
  
  # Url Binance Vision: Spot
  base_url <- "https://data.binance.vision/data/spot/daily/klines"
  
  # Dataset Url
  data_url <- paste0(base_url, "/", pair_name, "/", interval, "/", pair_name, "-", interval, "-", day_date, ".zip")
  
  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-", interval, "-", day_date, ".csv")
  
  # Create a temporary directory
  temp <- base::tempfile()
  
  # Download the file
  safe_download <- purrr::safely(utils::download.file)
  
  dwn <- safe_download(data_url, temp, quiet = quiet)
  
  if(is.null(dwn$result)){
    warning("Some errors with the download of the file")
    return(dplyr::tibble())
  }
  
  # OpenTime X1
  # CloseTime X7
  # Open X2
  # High X3
  # Low X4
  # Close X5
  # Volume X6
  # Quote asset Volume X8
  # Number of Trades X9
  # Taker buy base asset volume X10
  # Taker buy quote asset volume X11
  # Ignore X12
  
  # Unzip and Read the file
  response <- base::unz(temp, file_name)
  response <- readr::read_csv(response,
                              show_col_types = FALSE, # Avoid messages
                              col_names = c("date", "open", "high", "low", "close",
                                            "volume", "date_close", "volume_quote", "trades",
                                            "taker_buy", "taker_buy_quote", "Ignore"))
  
  # Unlink the connection created with temp
  base::unlink(temp)
  
  # Clear the response
  response <- dplyr::mutate(response,
                            date = as.POSIXct(date/1000, origin = "1970-01-01"),
                            date_close = as.POSIXct(date_close/1000, origin = "1970-01-01"),
                            market = "spot",
                            pair = pair_name,
                            open = as.numeric(open),
                            high = as.numeric(high),
                            low = as.numeric(low),
                            close = as.numeric(close),
                            volume = as.numeric(volume),
                            volume_quote = as.numeric(volume_quote),
                            trades = as.numeric(trades),
                            taker_buy = as.numeric(taker_buy),
                            taker_buy_quote = as.numeric(taker_buy_quote)
  )
  
  # Reorder the variables
  response <- dplyr::select(response,
                            date, date_close, market, pair,
                            open, high, low, close, volume,
                            volume_quote, trades, taker_buy, taker_buy_quote)
  
  # Arrange by Date (descending)
  response <- dplyr::arrange(response, date)
  
  # interval attribute
  attr(response, "interval") <- interval
  
  return(response)
  
}

# futures 
binance_vision_futures_klines <- function(pair = "BTCUSD_PERP", day_date = Sys.Date()-2, market = "usd-m", interval = "1m", quiet = FALSE){
  
  # Arguments: Pair Name
  pair_name <- toupper(pair)
  
  # Arguments: Type of Future
  future_type <- match.arg(market, choices = c("usd-m", "coin-m"))
  market <- ifelse(future_type == "usd-m", "um", "cm")

  # Arguments: Method
  # method <- match.arg(method, choices = c("klines", "indexPriceKlines", "markPriceKlines", "premiumIndexKlines"))
   method = "klines"
   
  # Arguments: Interval
  interval <- tolower(interval)
  interval <- match.arg(interval, choices = c("1m", "3m", "5m", "15m", "30m", 
                                              "1h", "2h", "4h", "6h", "8h", "12h", 
                                              "1d", "3d", "1w", "1mo"))
  
  # Check default Date
  if(is.null(day_date) || day_date > (Sys.Date()-2)){
    
    day_date <- Sys.Date()-2
    wrn <- paste0('The maximum date available is: ', day_date)
    if(!quiet) warning(wrn)
    
  } else {
    
    day_date <- as.Date(day_date)
    
  }
  
  # Url Binance Vision: Futures
  base_url <- "https://data.binance.vision/data/futures"
  
  # Dataset Url
  data_url <- paste0(base_url, "/", market, "/", "daily", "/", method, "/", pair_name, "/", interval, "/", pair_name, "-", interval, "-", day_date, ".zip")
  
  # File Name in Temp (as csv file)
  file_name <- paste0(pair_name, "-", interval, "-", day_date, ".csv")
  
  # Create a temporary directory
  temp <- base::tempfile()
  
  # Download the file
  utils::download.file(data_url, temp, quiet = quiet)
  
  # OpenTime X1
  # CloseTime X7
  # Open X2
  # High X3
  # Low X4
  # Close X5
  # Volume X6
  # Quote asset Volume X8
  # Number of Trades X9
  # Taker buy base asset volume X10
  # Taker buy quote asset volume X11
  # Ignore X12
  
  # Unzip and Read the file
  response <- base::unz(temp, file_name)
  
  response <- readr::read_csv(response,
                              skip = 1,
                              show_col_types = FALSE, # Avoid messages
                              col_names = c("date", "open", "high", "low", "close",
                                            "volume", "date_close", "volume_quote", "trades",
                                            "taker_buy", "taker_buy_quote", "Ignore"))
  
  # Unlink the connection created with temp
  base::unlink(temp)
  
  response <- dplyr::mutate(response,
                            date = as.numeric(date),
                            date = as.POSIXct(date/1000, origin = "1970-01-01"),
                            date_close = as.numeric(date_close),
                            date_close = as.POSIXct(date_close/1000, origin = "1970-01-01"),
                            market = future_type,
                            pair = pair_name,
                            open = as.numeric(open),
                            high = as.numeric(high),
                            low = as.numeric(low),
                            close = as.numeric(close),
                            volume = as.numeric(volume),
                            volume_quote = as.numeric(volume_quote),
                            trades = as.numeric(trades),
                            taker_buy = as.numeric(taker_buy),
                            taker_buy_quote = as.numeric(taker_buy_quote))
                            
  # Reorder the variables
  response <- dplyr::select(response,
                            date, date_close, market, pair,
                            open, high, low, close, volume,
                            volume_quote, trades, taker_buy, taker_buy_quote)
  
  # Arrange by Date (from the most recent)
  response <- dplyr::arrange(response, date)
  
  # interval attribute
  attr(response, "interval") <- interval
  attr(response, "api") <-  ifelse(future_type == "usd-m", "fapi", "dapi" )
  return(response)
}