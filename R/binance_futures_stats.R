
binance_futures_stats <- function(pair = "BTCUSDT", api = "fapi", interval = "1d", indicator = NULL, from = NULL, to = NULL, quiet = FALSE){
 
  api <- match.arg(api, choices = c("fapi", "dapi"))
  
  # General Check: indicator default argument 
  if (missing(indicator) || is.null(indicator)) {
    indicator <- "globalLongShortAccountRatio"
    if (!quiet) {
      wrn <- paste0('The indicator argument is missing, default is ', '"', indicator, '"')
      warning(wrn)
    }
  } else {
    available_indicators <- c("takerlongshortRatio", "globalLongShortAccountRatio", "topLongShortPositionRatio", "topLongShortAccountRatio")
    indicator <- match.arg(indicator, choices = available_indicators)
  }
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    pair_name <- ifelse(api == "fapi", "BTCUSDT", "BTCUSD")
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
    limit_date <- Sys.time() - lubridate::days(30)
    from <- safe_as_datetime(from, origin = "1970-01-01")$result
    if(from < limit_date){
      from <- limit_date 
    }
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
    
    limit_date <- Sys.time()
    if(to > limit_date){
      to <- limit_date 
    }
  }
  
  # General Check: interval
  if (missing(interval) || is.null(interval)) {
    interval <- "1h"
    if (!quiet) {
      wrn <- paste0('The "interval" argument is missing, default is ', '"', interval, '"')
      warning(wrn)
    }
  } else {
    available_intervals <- c("5m", "15m","30m","1h", "2h", "4h", "6h","12h","1d")
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
    api_path <- c("futures","data", indicator)
    # query
    if (api == "fapi"){
      api_query <- list(symbol = pair_name, period = interval, startTime = NULL, endTime = end_time, limit = 500)
    } else {
      api_query <- list(pair = pair_name, period = interval, startTime = NULL, endTime = end_time, limit = 500) 
    }
    # api GET call 
    new_data <- binance_api(api = api, path = api_path, use_base_path = FALSE, query = api_query)
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
                              api = api,
                              indicator = indicator, 
                              timestamp = as.POSIXct(as.numeric(timestamp)/1000, origin = "1970-01-01"))
    
    if(indicator == "takerlongshortRatio"){
      response <- dplyr::mutate(response,
                                buySellRatio = as.numeric(buySellRatio),
                                sellVol = as.numeric(sellVol),
                                buyVol = as.numeric(buyVol))

      response <- dplyr::select(response, date = "timestamp", api, indicator, pair, 
                                buy_volume = "buyVol", sell_volume = "sellVol", buy_sell_ratio = "buySellRatio")
    } else {
      response <- dplyr::mutate(response,
                                longShortRatio = as.numeric(longShortRatio),
                                longAccount = as.numeric(longAccount),
                                shortAccount = as.numeric(shortAccount))
      response <- dplyr::select(response, date = "timestamp", api, indicator, pair, 
                                long_account = "longAccount", short_account = "shortAccount", long_short_account = "longShortRatio")
    }
    
    response <- dplyr::filter(response, date >= from & date <= to)
    response <- dplyr::arrange(response, date)
  }
  
  return(response)
}

# binance_futures_stats(pair = "BTCUSD", api = "dapi", interval = "1d", indicator = "takerlongshortRatio", from = NULL, to = NULL)
#binance_futures_stats(pair = "BTCUSD", api = "dapi", interval = "1d", indicator = "globalLongShortAccountRatio", from = NULL, to = NULL)
# binance_futures_stats(pair = "BTCUSD", api = "dapi", interval = "1d", indicator = "topLongShortPositionRatio", from = NULL, to = NULL)
#binance_futures_stats(pair = "BTCUSD", api = "dapi", interval = "1d", indicator = "topLongShortAccountRatio", from = NULL, to = NULL)
