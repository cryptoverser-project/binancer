#' Retrieve Aggregated Historical Trades
#'
#' Get aggregated historical trades data for a specified trading pair from the selected reference API.
#'
#' @param pair Character, specifying the trading pair of interest, e.g., "BTCUSDT".
#'
#' @param api Character, indicating the reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#recent-trades-list).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#recent-trades-list).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#recent-trades-list).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#recent-trades-list).
#'
#' @param from Character, specifying the start date for historical trades data. Use NULL to indicate no specific start date.
#'
#' @param to Character, specifying the end date for historical trades data. Use NULL to indicate no specific end date.
#'
#' @param quiet Logical, indicating whether to suppress console messages. Default is FALSE.
#'
#' @return A tibble (data frame) object containing aggregated historical trades data for the selected trading pair within the specified date range.
#'
#' @examples
#'
#' # Example: Retrieve the last 10 minutes of trades for BTCUSDT from the Spot API.
#' binance_trades(pair = "BTCUSDT", api = "spot", from = NULL, to = NULL)
#'
#' # Example: Retrieve the last 10 minutes of trades for BTCUSDT from the Futures USD-M API.
#' binance_trades(pair = "BTCUSDT", api = "fapi", from = NULL, to = NULL)
#'
#' # Example: Retrieve the last 10 minutes of trades for BTCUSD_PERP from the Futures Coin-M API.
#' binance_trades(pair = "BTCUSD_PERP", api = "dapi", from = NULL, to = NULL)
#'
#' # Example: Retrieve trades for LAZIOUSDT within a specific date range from the Spot API.
#' binance_trades(pair = "LAZIOUSDT", api = "spot", from = "2023-01-01", to = "2023-01-02")
#'
#' @export
#'
#' @rdname binance_trades
#'
#' @name binance_trades
#'

binance_trades <- function(pair, api = "spot", from, to, quiet = FALSE){
  
  api <- match.arg(api, choices = c("spot", "fapi", "dapi"))
  
  # Check: arguments 
  if (missing(pair)) {
    pair <- NULL
  }
  if (missing(from)) {
    from <- NULL
  }
  if (missing(to)) {
    to <- NULL
  }

  # function name 
  fun_name <- paste0("binance_", api, "_trades")
  
  # safe call to avoid errors 
  args <- list(pair = pair, from = from , to = to, quiet = quiet)
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  response <- NULL
  response <- safe_fun()
  if(!quiet & !is.null(response$error)){
    warning(response$error)
  }
  return(response$result)
}

# spot api 
binance_spot_trades <- function(pair, from, to, quiet = FALSE){
  
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
    from <- Sys.time() - lubridate::minutes(10)
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
  
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  first_date <- as.integer(from)*1000
  while(condition){
    # query
    api_query <- list(symbol = pair_name, startTime = start_time, endTime = NULL, limit = 1000)
    # api GET call 
    new_data <- binance_api(api = "spot", path = c("aggTrades"), query = api_query)
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    response[[i]] <- new_data
    colnames(response[[i]]) <- c("agg_id", "price", "quantity", "first_id",
                                 "last_id", "date", "side", "isBestMatch")
    
    # extract the maximum date
    last_date <- max(as.numeric(response[[i]]$date))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- last_date < end_time & first_date < last_date
    first_date <- last_date # needed avoid infinite loops 
    # ELSE: use the first_date as new endTime
    start_time <- paste0(trunc(last_date/1000), "000")
    i = i + 1
    
  }
  
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              pair = pair_name,
                              market = "spot",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              price = as.numeric(price),
                              quantity = as.numeric(quantity),
                              side = ifelse(side, "SELL", "BUY")
    )
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::select(response, date, market, pair, price, quantity, side, agg_id, first_id, last_id)
  }
  
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "spot"
  return(response)
}

# futures USD-M api 
binance_fapi_trades <- function(pair, from, to, quiet = FALSE){
  
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
    from <- Sys.time() - lubridate::minutes(10)
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
  
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  first_date <- as.integer(from)*1000
  
  while(condition){
    
    # query
    api_query <- list(symbol = pair_name, startTime = start_time, endTime = NULL, limit = 1000)
    # api GET call 
    new_data <- binance_api(api = "fapi", path = c("aggTrades"), query = api_query)
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    response[[i]] <- new_data
    colnames(response[[i]]) <- c("agg_id", "price", "quantity", "first_id",
                                 "last_id", "date", "side")
    
    # extract the maximum date
    last_date <- max(as.numeric(response[[i]]$date))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- last_date < end_time & first_date < last_date
    first_date <- last_date # needed avoid infinite loops 
    # ELSE: use the first_date as new endTime
    start_time <- paste0(trunc(last_date/1000), "000")
    i = i + 1
    
  }
  
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              pair = pair_name,
                              market = "usd-m",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              price = as.numeric(price),
                              quantity = as.numeric(quantity),
                              side = ifelse(side, "SELL", "BUY")
    )
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::select(response, date, market, pair, price, quantity, side, agg_id, first_id, last_id)
  }
  
  attr(response, "ip_weight") <- i * 20
  attr(response, "api") <- "usd-m"
  return(response)
}

# futures COIN-M api 
binance_dapi_trades <- function(pair, from, to, quiet = FALSE){
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- "BTCUSD_PERP"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      warning(wrn)
    }
  } else {
    pair_name <- toupper(pair)
  }
  
  # General Check: from default argument 
  if (missing(from) || is.null(from)) {
    from <- Sys.time() - lubridate::minutes(10)
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
  
  
  i <- 1
  response  <- list()
  condition <- TRUE
  end_time <- paste0(trunc(as.integer(to)), "000")
  start_time <- paste0(trunc(as.integer(from)), "000")
  first_date <- as.integer(from)*1000
  
  while(condition){
    
    # query
    api_query <- list(symbol = pair_name, startTime = start_time, endTime = NULL, limit = 1000)
    # api GET call 
    new_data <- binance_api(api = "dapi", path = c("aggTrades"), query = api_query)
    # Break Condition: new_data is empty 
    if(purrr::is_empty(new_data)){
      break
    }
    response[[i]] <- new_data
    colnames(response[[i]]) <- c("agg_id", "price", "quantity", "first_id",
                                 "last_id", "date", "side")
    
    # extract the maximum date
    last_date <- max(as.numeric(response[[i]]$date))
    # Break Condition: IF first_date is greater than start_time THEN stop
    condition <- last_date < end_time & first_date < last_date
    first_date <- last_date # needed avoid infinite loops 
    # ELSE: use the first_date as new endTime
    start_time <- paste0(trunc(last_date/1000), "000")
    i = i + 1
    
  }
  
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response,
                              pair = pair_name,
                              market = "coin-m",
                              date = as.POSIXct(as.numeric(date)/1000, origin = "1970-01-01"),
                              price = as.numeric(price),
                              quantity = as.numeric(quantity),
                              side = ifelse(side, "SELL", "BUY")
    )
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::select(response, date, market, pair, price, quantity, side, agg_id, first_id, last_id)
  }
  
  attr(response, "ip_weight") <- i * 20
  attr(response, "api") <- "coin-m"
  return(response)
}

