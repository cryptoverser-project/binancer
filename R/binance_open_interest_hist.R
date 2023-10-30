#' Historical Open Interest Data
#'
#' Get the historical open interest data for a trading pair.
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"` or `"BTCUSD"`.
#'
#' @param api Character, reference API. Available options are:
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#open-interest).
#'   - `"dapi"`: for [futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#open-interest).
#'   
#' @param interval Character. Default is `"1h"`. Time interval for open interest data. Available intervals are: 
#'   - Minutely: `"5m"`, `"15m"` and `"30m"`.
#'   - Hourly: `"1h"`, `"2h"`, `"4h"`, `"6h"`, `"8h"` and `"12h"`.
#'   - Daily: `"1d"`.
#' 
#' @param from Character or \code{\link[=POSIXt-class]{POSIXt}} object. Start time for historical data, only last 30 days are available. 
#' If it is `missing`, the default, will be used as start date `Sys.time()-lubridate::days(30)`.
#' 
#' @param to Character or \code{\link[=POSIXt-class]{POSIXt}} object. End time for historical data, only last 30 days are available. 
#' If it is `missing`, the default, will be used as start date `Sys.time()`.
#' 
#' 
#' @param contract_type Character. Used only if `api = "dapi"`. Available contract's types are: 
#'   - `"all"`: for all types of contracts.
#'   - `"perpetual"`: for perpetual futures.
#'   - `"current_quarter"`: for futures with maturity in the current quarter.
#'   - `"next_quarter"`: for futures with maturity in the next quarter.
#'
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @return A \code{\link[=data.frame-class]{data.frame}} with 5 columns:
#'   - `date`: \code{"\link[=POSIXt-class]{POSIXt}"}, observation date.
#'   - `market`: Character, API.
#'   - `pair`: Character, trading pair.
#'   - `open_interest`: Numeric, open interest in base currency.
#'   - `open_interest_usd`: Numeric, open interest in USD.
#'   
#' @details The IP weight for this API call is 1, and the data source is memory. 
#' The historical open interest data are only available for the last 30 days. 
#' 
#' @usage 
#' binance_open_interest_hist(pair,
#'                            api = "fapi", 
#'                            interval = "1d", 
#'                            from = NULL, 
#'                            to = NULL, 
#'                            contract_type = "all", 
#'                            quiet = FALSE)
#'
#' @examples
#' 
#' # Historical open interest for BTCUSDT
#' binance_open_interest_hist(pair = "BTCUSDT", 
#'                            api = "fapi", 
#'                            interval = "1d", 
#'                            from = Sys.time()-lubridate::days(15),
#'                            to = Sys.time()-lubridate::days(5))
#'
#' # Historical open interest for BTCUSD
#' binance_open_interest_hist(pair = "BTCUSD", 
#'                            api = "dapi", 
#'                            interval = "1d", 
#'                            from = Sys.time()-lubridate::days(15),
#'                            to = NULL)
#'
#' @export
#'
#' @rdname binance_open_interest_hist
#' @name binance_open_interest_hist

binance_open_interest_hist <- function(pair, api = "fapi", interval = "1d", from = NULL, to = NULL, contract_type = "all", quiet = FALSE){
  
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
    api <- "fapi"
    if (!quiet) {
      msg <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    api <- match.arg(api, choices = c("fapi", "dapi"))
  }
  
  sys_time <- Sys.time()
  # Check "from" argument
  if (missing(from) || is.null(from)) {
    from <- sys_time - lubridate::days(30)
    if (!quiet) {
      msg <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    from <- as.POSIXct(from, origin = "1970-01-01")
    max_from <- sys_time - lubridate::days(30)
    if (!(from < max_from) & !quiet) {
      msg <- paste0('The "from" argument is greater than the maximum value ', max_from)
      cli::cli_alert_warning(msg)
      from <- max_from 
    } 
  }
  
  # Check "to" argument
  if (missing(to) || is.null(to)) {
    to <- sys_time 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    to <- as.POSIXct(to, origin = "1970-01-01")
    min_to <- sys_time - lubridate::days(30)
    if (!(to < min_to) & !quiet) {
      msg <- paste0('The "to" argument is lower than the minimum value ', min_to)
      cli::cli_alert_warning(msg)
      to <- sys_time 
    } 
  }
  
  # Check "interval" argument
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    av_int <- c("5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d")
    interval  <- match.arg(interval, choices = av_int)
  }
  
  # Check "contract_type" argument 
  if (missing(contract_type) || is.null(contract_type)) {
    contract_type <- "all"
    if (!quiet) {
      wrn <- paste0('The "contract_type" argument is missing, default is ', '"', contract_type, '"')
      cli::cli_alert_warning(wrn)
    }
    contract_type <- toupper(contract_type)
  } else {
    av_type <- c("all", "perpetual", "current_quarter", "next_quarter")
    contract_type <- match.arg(contract_type, choices = av_type)
    contract_type <- toupper(contract_type)
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- as_unix_time(to, as_character = TRUE)
  start_time <- as_unix_time(from, as_character = TRUE)
  last_date <- as_unix_time(to, as_character = FALSE)
  while(condition){
    # Build query parameters depends on api 
    if (api == "fapi") {
      query <- list(symbol = pair, period = interval, startTime = NULL, endTime = end_time, limit = 500)
    } else if (api %in% c("dapi")) {
      query <- list(pair = pair, period = interval, contractType = contract_type, startTime = NULL, endTime = end_time, limit = 500)
    } 
    # GET call 
    new_data <- binance_query(api = api, path = c("futures","data", "openInterestHist"), query = query, use_base_path = FALSE)
    # Break Condition: new_data is empty 
    if(!is.null(new_data$code)){
      break
    } else {
      response[[i]] <- dplyr::as_tibble(new_data)
    }
    
    # Extract the minimum date
    first_date <- min(as.numeric(response[[i]]$timestamp))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- first_date > as.numeric(start_time) & first_date < last_date
    last_date <- end_time # needed avoid infinite loops 
    # ELSE: use the first_date as new endTime
    end_time <- paste0(trunc(first_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$api <- api
    response <- binance_formatter(response)
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  attr(response, "api") <- api
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  
  return(response)
}

