#' Retrieve Historical Open Interest Data
#'
#' Obtain historical open interest data for a specific trading pair. This function allows you to access the open interest information over time.
#'
#' @param pair Character, specifying the trading pair of interest, e.g., "BTCUSDT".
#'
#' @param api Character, specifying the reference API. Default is "fapi". Available options include:
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#open-interest).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#open-interest).

#' @param interval Character, specifying the time interval for open interest data. Default is "1d". Available intervals include "1d", "3d", "1w", and more.
#'
#' @param from Character or POSIXct, specifying the start time for data retrieval. Default is NULL, which represents the earliest available data. You can provide a character or a POSIXct object, e.g., "2023-01-01" or a specific date and time.
#'
#' @param to Character or POSIXct, specifying the end time for data retrieval. Default is NULL, which represents the most recent available data. You can provide a character or a POSIXct object, e.g., "2023-12-31" or a specific date and time.
#'
#' @return A tibble (data frame) object containing open interest data, including date, market, pair, and open interest values over time.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Retrieve historical open interest for "BTCUSD" in USD-M market
#' binance_open_interest_hist(pair = "BTCUSDT", api = "fapi", interval = "1d", from = "2023-01-01")
#'
#' # Retrieve historical open interest for "BTCUSD" in Coin-M market
#' binance_open_interest_hist(pair = "BTCUSD", api = "dapi", interval = "1d", from = "2023-01-01")
#'
#' @export
#'
#' @rdname binance_open_interest_hist
#'
#' @name binance_open_interest_hist

binance_open_interest_hist <- function(pair, api = "fapi", interval = "1d", from = NULL, to = NULL, contract_type = "all", quiet = FALSE){
  
  # function name 
  fun_name <- paste0("binance_", api, "_open_interest_hist")
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair <- ifelse(api == "fapi", "BTCUSDT", "BTCUSD")
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair, '"')
      warning(wrn)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # General Check: from default argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::days(30)
    if (!quiet) {
      wrn <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      warning(wrn)
    }
  } else {
    # safe function to avoid errors 
    safe_as_datetime <- purrr::safely(as.POSIXct)
    from <- safe_as_datetime(from, origin = "1970-01-01")$result
  }
  
  # General Check: to default argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.time() 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      warning(wrn)
    }
  } else {
    # safe function to avoid errors 
    safe_as_datetime <- purrr::safely(as.POSIXct)
    to <- safe_as_datetime(to, origin = "1970-01-01")$result
  }
  
  # General Check: interval
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      warning(wrn)
    }
  } else {
    available_intervals <- c("5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d")
    interval  <- match.arg(interval, choices = available_intervals)
  }
  
  # function arguments 
  if (api == "fapi") {
    args <- list(pair = pair, interval = interval, from = from, to = to)
  } else if (api %in% c("dapi")) {
    args <- list(pair = pair, interval = interval, from = from, to = to, contract_type = contract_type)
  } 
  # safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  
  response <- NULL
  response <- safe_fun()
  if(!quiet & !is.null(response$error)){
    warning(response$error)
  }
  return(response$result)
}

# api functions -----------------------------------------------------------------------------------------------------
binance_fapi_open_interest_hist <- function(pair, interval = "1d", from = NULL, to = NULL){
  
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
  
  # General Check: from default argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::days(30)
    if (!quiet) {
      wrn <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      warning(wrn)
    }
  } else {
    # safe function to avoid errors 
    safe_as_datetime <- purrr::safely(as.POSIXct)
    from <- safe_as_datetime(from, origin = "1970-01-01")$result
  }
  
  # General Check: to default argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.time() 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      warning(wrn)
    }
  } else {
    # safe function to avoid errors 
    safe_as_datetime <- purrr::safely(as.POSIXct)
    to <- safe_as_datetime(to, origin = "1970-01-01")$result
  }
  
  # General Check: interval
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      warning(wrn)
    }
  } else {
    available_intervals <- c("5m", "15m", "30m", "1h", "2h", "4h", "6h", "12h", "1d")
    interval  <- match.arg(interval, choices = available_intervals)
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  last_date <- as.integer(to)*1000
  while(condition){
    # path
    api_path <- c("futures","data", "openInterestHist")
    # query
    api_query <- list(symbol = pair_name, period = interval, startTime = NULL , endTime = end_time, limit = 500)
    # api GET call 
    new_data <- binance_api(api = "fapi", path = api_path, use_base_path = FALSE, query = api_query)
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
    i = i + 1
  }
  
  # Adjust the Response
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair_name,
                              api = "fapi",
                              date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              open_interest = as.numeric(sumOpenInterest),
                              open_interest_usd = as.numeric(sumOpenInterestValue)
    )
    
    response <- dplyr::select(response, date, api, pair, open_interest, open_interest_usd)
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  # Weigth and Api attributes
  attr(response, "api") <- "fapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

binance_dapi_open_interest_hist <- function(pair, interval = "1d", from = NULL, to = NULL, contract_type = "all"){
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- "BTCUSD"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      warning(wrn)
    }
  } else {
    pair_name <- toupper(pair)
  }
  
  # General Check: from default argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::days(30)
    if (!quiet) {
      wrn <- paste0('The "from" argument is missing, default is ', '"', from, '"')
      warning(wrn)
    }
  } else {
    # safe function to avoid errors 
    safe_as_datetime <- purrr::safely(as.POSIXct)
    from <- safe_as_datetime(from, origin = "1970-01-01")$result
  }
  
  # General Check: to default argument 
  if (missing(to) || is.null(to)) {
    to <- Sys.time() 
    if (!quiet) {
      wrn <- paste0('The "to" argument is missing, default is ', '"', to, '"')
      warning(wrn)
    }
  } else {
    # safe function to avoid errors 
    safe_as_datetime <- purrr::safely(as.POSIXct)
    to <- safe_as_datetime(to, origin = "1970-01-01")$result
  }
  
  # General Check: interval
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      warning(wrn)
    }
  } else {
    available_intervals <- c("5m", "15m","30m","1h", "2h", "4h", "6h", "12h", "1d")
    interval  <- match.arg(interval, choices = available_intervals)
  }
  # General Check: contract_type
  if (missing(contract_type) || is.null(contract_type)) {
    contract_type <- "ALL"
  } else {
    available_types <- c("all", "perpetual", "current_quarter", "next_quarter")
    contract_type <- match.arg(contract_type, choices = available_types)
    contract_type <- toupper(contract_type)
  }
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  last_date <- as.integer(to)*1000
  while(condition){
    
    # path
    api_path <- c("futures","data", "openInterestHist")
    # query
    api_query <- list(pair = pair_name, period = interval, contractType = contract_type, startTime = NULL, endTime = end_time, limit = 500)
    # api GET call 
    new_data <- binance_api(api = "dapi", path = api_path, use_base_path = FALSE, query = api_query)
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
    i = i + 1
  }
  
  # Adjust the Response
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::mutate(response,
                              pair = pair_name,
                              api = "dapi",
                              contract_type = tolower(contract_type),
                              date = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"),
                              open_interest = as.numeric(sumOpenInterest),
                              open_interest_usd = as.numeric(sumOpenInterestValue)
    )
    
    response <- dplyr::select(response, date, api, pair, contract_type, open_interest, open_interest_usd)
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  # Weigth and Api attributes
  attr(response, "api") <- "dapi"
  attr(response, "ip_weight") <- i
  attr(response, "interval") <- interval
  return(response)
}

