#' Historical Open Interest Data
#'
#' Get the historical open interest data for a specific trading pair.
#'
#' @param pair Character, trading pair, e.g. "BTCUSDT".
#'
#' @param api Character, reference API. Available options are:
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#open-interest).
#'   - "dapi": For [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#open-interest).
#'   
#' @param interval Character, time interval for open interest data. Available intervals are: 
#'   - `minutely`: "5m", "15m" and "30m";
#'   - `hourly`: "1h", "2h", "4h", "6h", "8h" and "12h";
#'   - `daily`: "1d".
#'
#' @param from Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the start time for historical data. 
#' Default is `NULL` and will be used as start date `Sys.time()-lubridate::days(30)`.
#' 
#' @param to Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the end time for historical data.
#' Default is `NULL` and will be used as end date `Sys.time()`.
#' 
#' @param contract_type Character, used only if `api = "dapi"`. Available contract type are: 
#'   - "all": for all types of contracts;
#'   - "perpetual": for perpetual futures;
#'   - "current_quarter": for futures with a maturity in the current quarter;
#'   - "next_quarter": for futures with a maturity in the next quarter.
#'
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`.
#' 
#' @return A tibble with 5 columns:
#'   - `date`: Datetime, observation date.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, selected pair.
#'   - `open_interest`: Numeric, open interest in base currency.
#'   - `open_interest_usd`: Numeric, open interest in usd.
#'   
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Retrieve historical open interest for "BTCUSD" in USD-M market
#' binance_open_interest_hist(pair = "BTCUSDT", api = "fapi", interval = "1d", from = "2023-10-01")
#'
#' # Retrieve historical open interest for "BTCUSD" in Coin-M market
#' binance_open_interest_hist(pair = "BTCUSD", api = "dapi", interval = "1d", from = "2023-01-01")
#'
#' @export
#'
#' @rdname binance_open_interest_hist
#' @name binance_open_interest_hist

binance_open_interest_hist <- function(pair, api = "fapi", interval = "1d", from = NULL, to = NULL, contract_type = "all", quiet = FALSE){
  
  # Check "pair" argument
  if (missing(pair) || is.null(pair)) {
    pair <- ifelse(api == "fapi", "BTCUSDT", "BTCUSD")
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Check "api" argument 
  if (missing(api) || is.null(api)) {
    api <- "fapi"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("fapi", "dapi"))
  }
  
  # Check "from" argument
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::days(30)
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
    contract_type <- "ALL"
  } else {
    av_type <- c("all", "perpetual", "current_quarter", "next_quarter")
    contract_type <- match.arg(contract_type, choices = av_type)
    contract_type <- toupper(contract_type)
  }
  
  # Query parameters depends on api 
  if (api == "fapi") {
    args <- list(pair = pair, interval = interval, from = from, to = to)
  } else if (api %in% c("dapi")) {
    args <- list(pair = pair, interval = interval, from = from, to = to, contract_type = contract_type)
  } 

  # Function name 
  fun_name <- paste0("binance_", api, "_open_interest_hist")
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  # GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    #cli::cli_abort(response$error)
  } else {
    return(response$result)
  }
}

# openInterestHist implementation for futures USD-m api 
binance_fapi_open_interest_hist <- function(pair, interval = "1d", from = NULL, to = NULL){
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  last_date <- as.integer(to)*1000
  while(condition){
    # GET call 
    api_query <- list(symbol = pair, period = interval, startTime = NULL, endTime = end_time, limit = 500)
    new_data <- binance_api(api = "fapi", path = c("futures","data", "openInterestHist"), use_base_path = FALSE, query = api_query)
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # extract the minimum date
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
    response <- dplyr::mutate(response,
                              pair = pair,
                              api = "fapi",
                              date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              open_interest = as.numeric(sumOpenInterest),
                              open_interest_usd = as.numeric(sumOpenInterestValue))
    response <- dplyr::select(response, date, api, pair, open_interest, open_interest_usd)
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  attr(response, "api") <- "fapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

# openInterestHist implementation for futures COIN-m api 
binance_dapi_open_interest_hist <- function(pair, interval = "1d", from = NULL, to = NULL, contract_type = "all"){
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  last_date <- as.integer(to)*1000
  while(condition){
    # GET call 
    api_query <- list(pair = pair, period = interval, contractType = contract_type, startTime = NULL, endTime = end_time, limit = 500)
    new_data <- binance_api(api = "dapi", path = c("futures","data", "openInterestHist"), use_base_path = FALSE, query = api_query)
    
    # Break if new_data is empty 
    if (purrr::is_empty(new_data)) {
      break
    }
    response[[i]] <- new_data
    response[[i]] <- dplyr::as_tibble(response[[i]])
    
    # Extract the first date
    first_date <- min(as.numeric(response[[i]]$timestamp))
    # Break if first_date is greater than start_time
    condition <- first_date > as.numeric(start_time) & first_date != last_date
    last_date <- end_time # needed avoid infinite loops 
    end_time <- paste0(trunc(first_date/1000), "000")
    i <- i + 1
  }
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair,
                              api = "dapi",
                              contract_type = tolower(contract_type),
                              date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              open_interest = as.numeric(sumOpenInterest),
                              open_interest_usd = as.numeric(sumOpenInterestValue))
    response <- dplyr::select(response, date, api, pair, contract_type, open_interest, open_interest_usd)
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  attr(response, "api") <- "dapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

