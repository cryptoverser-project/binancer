#' Historical Klines data from Binance Vision 
#' 
#' Historical Kline data for a trading pair from the Binance Vision database.
#' 
#' @param pair Character. Trading pair, e.g., `"BTCUSDT"`.
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-data).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-data).
#' 
#' 
#' @param interval Character. Default is `"1d"`. Time interval for klines data. Available intervals are: 
#'   - Secondly: `"1s"`, available only if `api = "spot"`.
#'   - Minutely: `"1m"`, `"3m"`, `"5m"`, `"15m"` and `"30m"`.
#'   - Hourly: `"1h"`, `"2h"`, `"4h"`, `"6h"`, `"8h"` and `"12h"`.
#'   - Daily: `"1d"`. 
#'   
#' @param from Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the start time for data retrieval. 
#' If `NULL`, the default is `Sys.Date()-lubridate::days(4)`.
#' @param to Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the end time for data retrieval. 
#' If `NULL`, the default is `Sys.Date()-lubridate::days(2)`.
#' @param quiet Logical, suppress informational messages if `TRUE`. Default `FALSE`.
#'
#' @return A \code{\link[=data.frame-class]{data.frame}}.
#' 
#' @examples 
#' 
#' from <- "2023-01-01"
#' to <- "2023-01-01"
#' interval <- "1m"
#' binance_vision_klines("BTCUSDT", api = "spot", 
#'                       interval = interval, from = from, to = from)
#' binance_vision_klines("BTCUSDT", api = "fapi",
#'                       interval = interval, from = from, to = from)
#' binance_vision_klines("BTCUSD_PERP", api = "dapi", 
#'                       interval = interval, from = from, to = from)
#'
#' @export
#' @name binance_vision_klines
#' @rdname binance_vision_klines

binance_vision_klines <- function(pair, api, interval, from, to, quiet = FALSE) {
  
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
  
  # Check "interval" argument 
  if (missing(interval) || is.null(interval)) {
    interval <- "1d"
    if (!quiet) {
      wrn <- paste0('The `interval` argument is missing, default is ', '"', interval, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    av_int <- c("1s", "1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d")
    if (api != "spot"){
      av_int <- av_int[-1] 
    }
    interval <- match.arg(interval, choices = av_int)
  }
  
  # Check "from" argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.Date() - lubridate::days(4)
    if (!quiet) {
      wrn <- paste0('The `from` argument is missing, default is ', '"', from, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    from <- as.Date(from)
  }
  
  # Check "to" argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.Date() - lubridate::days(4)
    if (!quiet) {
      wrn <- paste0('The `to` argument is missing, default is ', '"', to, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    to <- as.Date(to)
  }
  
  i <- 1
  seq_date <- seq.Date(from, to, by = "days")
  responses <- list()
  for(i in 1:length(seq_date)){
    day_date <- seq_date[i]
    if (api == "spot") {
      # Url Binance Vision: Spot
      base_url <- "https://data.binance.vision/data/spot/daily/klines"
      # Dataset Url
      data_url <- paste0(base_url, "/", pair, "/", interval, "/", pair, "-", interval, "-", day_date, ".zip")
    } else {
      future_type <- match.arg(api, choices = c(um = "fapi", cm = "dapi"))
      future_type <- names(future_type)
      # Url Binance Vision: Futures
      base_url <- "https://data.binance.vision/data/futures"
      # Dataset Url
      data_url <- paste0(base_url, "/", future_type, "/daily/klines/", pair, "/", interval, "/", pair, "-", interval, "-", day_date, ".zip")
    }
    
    # File Name in Temp (as csv file)
    file_name <- paste0(pair, "-", interval, "-", day_date, ".csv")
    # Create a temporary directory
    temp <- base::tempfile()
    # Download the file
    safe_download <- purrr::safely(utils::download.file)
    dwn <- safe_download(data_url, temp, quiet = quiet)
    if (is.null(dwn$result)) {
      warning("Some errors with the download of the file")
      return(dplyr::tibble())
    }
    
    # Unzip and read the file
    response <- base::unz(temp, file_name)
    col_names <- c("date", "open", "high", "low", "close", "volume", "date_close", 
                   "volume_quote", "trades", "taker_buy", "taker_buy_quote", "ignore") 
    response <- readr::read_csv(response, show_col_types = FALSE, col_names = col_names)
    # Unlink the connection created with temp
    base::unlink(temp)
    response$market <- api
    response$pair <- pair
    response <- binance_formatter(response)
    
    responses[[i]] <- na.omit(response)
  }
  
  response <- dplyr::bind_rows(responses)
  # Arrange by Date (descending)
  response <- dplyr::arrange(response, date)
  
  # interval attribute
  attr(response, "api") <- api
  attr(response, "interval") <- interval
  
  return(response)
}
