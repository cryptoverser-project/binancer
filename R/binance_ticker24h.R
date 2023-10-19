#' Retrieve 24-Hour Ticker Statistics
#'
#' Get 24-hour ticker statistics for a specified trading pair from the selected reference API.
#'
#' @param pair Character, specifying the trading pair of interest, e.g., "BTCUSDT".
#'
#' @param api Character, indicating the reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#24hr-ticker-price-change-statistics).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#24hr-ticker-price-change-statistics).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#24hr-ticker-price-change-statistics).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#24hr-ticker-price-change-statistics).
#'
#' @param type Character, specifying the type of ticker data to retrieve. Available options are:
#'   - "mini": Retrieve ticker data without "ask" and "bid" prices and quantities.
#'   - "full": Retrieve complete ticker data with all available columns.
#'
#' @param quiet Logical, indicating whether to suppress console messages. Default is FALSE.
#'
#' @return A tibble (data frame) with 13 columns containing 24-hour ticker statistics, including open, high, low, close prices, volume, and more.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Example: Retrieve full 24-hour ticker statistics for the BTCUSDT pair from the Spot API.
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "full")
#'
#' # Example: Retrieve mini 24-hour ticker statistics for the BTCUSDT pair from the Spot API.
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "mini")
#'
#' # Example: Retrieve full 24-hour ticker statistics for the BTCUSDT pair from the Futures USD-M API.
#' binance_ticker24h(pair = "BTCUSDT", api = "fapi")
#'
#' # Example: Retrieve full 24-hour ticker statistics for the BTCUSD_PERP pair from the Futures Coin-M API.
#' binance_ticker24h(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Example: Retrieve full 24-hour ticker statistics for an options trading pair from the Options API.
#' binance_ticker24h(pair = "BTC-230331-21000-P", api = "eapi")
#'
#' @export
#'
#' @rdname binance_ticker24h
#'
#' @name binance_ticker24h

binance_ticker24h <- function(pair, api = "spot", type = c("full", "mini"), quiet = FALSE){
  
  if (missing(pair)) {
    pair <- NULL
  }
  
  # api function name 
  fun_name <- paste0("binance_", api, "_ticker24h")
  
  # safe call to avoid errors 
  if (api == "spot") {
    safe_fun <- purrr::safely(~do.call(fun_name, args = list(pair, type)))
  } else {
    safe_fun <- purrr::safely(~do.call(fun_name, args = list(pair)))
  }
  
  response <- safe_fun()
  if (!quiet & !is.null(response$error)) {
    warning(response$error)
  }
  return(response$result)
}

binance_spot_ticker24h <- function(pair = "BTCUSDT", type = c("full", "mini")) {
  
  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }
  
  # Control on the Type
  if(is.null(type)){
    wrn <- 'The "type" argument cannot be NULL, by default is "full".'
    if(!quiet) warning(wrn)
    type <- "full"
  } else {
    # Match type: full or mini
    type <- match.arg(tolower(type), choices = c("full", "mini"))
  }
  
  # Multiple Pairs allowed
  if(length(pair_name) > 1){
    mult_api_query <- purrr::map_chr(pair_name, ~paste0('"', .x, '"'))
    mult_api_query <- paste0(mult_api_query, collapse = ",")
    pair_name <- paste0('[', mult_api_query, ']')
  }
  
  # Query change for multiple Symbols
  if(length(pair) > 1){
    api_query <- list(symbols = pair_name, type = toupper(type))
  } else {
    api_query <- list(symbol = pair_name, type = toupper(type))
  }
  
  # Api Response
  response <- NULL
  response <- binance_api(api = "spot", path = c("ticker", "24hr" ), query = api_query)
  
  # Cleaning
  if(!purrr::is_empty(response) & type == "mini"){
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               date_close = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               pair = response$symbol,
                               market = "spot",
                               open = as.numeric(response$openPrice),
                               high = as.numeric(response$highPrice),
                               low = as.numeric(response$lowPrice),
                               close = as.numeric(response$lastPrice),
                               volume = as.numeric(response$volume),
                               volume_quote = as.numeric(response$quoteVolume),
                               first_id = as.character(response$firstId),
                               last_id = as.character(response$lastId),
                               trades = as.integer(response$count))
    
  } else if(!purrr::is_empty(response) & type == "full"){
    
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               date_close = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               pair = response$symbol,
                               weighted_price = as.numeric(response$weightedAvgPrice),
                               L1_close = as.numeric(response$prevClosePrice),
                               open = as.numeric(response$openPrice),
                               high = as.numeric(response$highPrice),
                               low = as.numeric(response$lowPrice),
                               last = as.numeric(response$lastPrice),
                               volume = as.numeric(response$volume),
                               volume_quote = as.numeric(response$quoteVolume),
                               ask = as.numeric(response$askPrice),
                               bid = as.numeric(response$bidPrice),
                               quantity_ask = as.numeric(response$askQty),
                               quantity_bid = as.numeric(response$bidQty),
                               first_id = as.character(response$firstId),
                               last_id = as.character(response$lastId),
                               trades = as.integer(response$count))
  } 
  
  # Attributes
  attr(response, "ip_weight") <- dplyr::case_when(
    length(pair) >= 1 & length(pair) <= 20 ~ 1,
    length(pair) > 20 & length(pair) <= 100 ~ 20,
    length(pair) > 100 ~ 40
  )
  attr(response, "api") <- "spot"
  return(response)
}

binance_fapi_ticker24h <- function(pair = "BTCUSDT") {
  
  response <- list()
  
  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }
  
  i <- 1
  for(i in 1:length(pair_name)){
    
    new_data <- binance_api(api = "fapi", path = c("ticker", "24hr"), query = list(symbol = pair_name[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # Cleaning
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               date_close = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               pair = response$symbol,
                               market = "usd-m",
                               open = as.numeric(response$openPrice),
                               high = as.numeric(response$highPrice),
                               low = as.numeric(response$lowPrice),
                               close = as.numeric(response$lastPrice),
                               volume = as.numeric(response$volume),
                               volume_quote = as.numeric(response$quoteVolume),
                               first_id = as.character(response$firstId),
                               last_id = as.character(response$lastId),
                               trades = as.integer(response$count))
    
  } 
  
  # Attributes
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "fapi"
  return(response)
}

binance_dapi_ticker24h <- function(pair = "BTCUSD_PERP") {
  
  response <- list()
  
  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }
  
  i <- 1
  for(i in 1:length(pair_name)){
    
    new_data <- binance_api(api = "dapi", path = c("ticker", "24hr"), query = list(symbol = pair_name[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # Cleaning
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               date_close = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               pair = response$symbol,
                               market = "coin-m",
                               open = as.numeric(response$openPrice),
                               high = as.numeric(response$highPrice),
                               low = as.numeric(response$lowPrice),
                               close = as.numeric(response$lastPrice),
                               volume = as.numeric(response$volume),
                               first_id = as.character(response$firstId),
                               last_id = as.character(response$lastId),
                               trades = as.integer(response$count))
  } 
  
  # Attributes
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "dapi"
  return(response)
}

binance_eapi_ticker24h <- function(pair = "BTCUSD_PERP") {
  
  response <- list()
  
  # Control on the Pair
  if(is.null(pair)){
    wrn <- 'The "pair" argument cannot be NULL'
    if(!quiet) warning(wrn)
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }
  
  i <- 1
  for(i in 1:length(pair_name)){
    
    new_data <- binance_api(api = "eapi", path = c("ticker"), query = list(symbol = pair_name[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # Cleaning
  if(!purrr::is_empty(response)){
    
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$openTime)/1000, origin = "1970-01-01"),
                               date_close = as.POSIXct(as.numeric(response$closeTime)/1000, origin = "1970-01-01"),
                               pair = response$symbol,
                               market = "options",
                               open = as.numeric(response$open),
                               high = as.numeric(response$high),
                               low = as.numeric(response$low),
                               last = as.numeric(response$lastPrice),
                               last_volume = as.numeric(response$lastQty),
                               ask = as.numeric(response$askPrice),
                               bid = as.numeric(response$bidPrice),
                               volume = as.numeric(response$volume),
                               quote_volume = as.numeric(response$amount),
                               first_id = as.character(response$firstTradeId),
                               trades = as.integer(response$tradeCount),
                               strike = as.numeric(response$strikePrice),
                               excercise_price = as.numeric(response$exercisePrice))
  } 
  
  # Attributes
  attr(response, "ip_weight") <- i * 5
  attr(response, "api") <- "eapi"
  return(response)
}


