#' Retrieve 24-Hour Ticker Statistics
#'
#' Get 24-hour ticker statistics for a specified trading pair from the selected reference API.
#'
#' @param pair Character, trading pair, e.g. "BTCUSDT".
#' 
#' @param api Character, reference API. Available options are:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#24hr-ticker-price-change-statistics).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#24hr-ticker-price-change-statistics).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#24hr-ticker-price-change-statistics).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#24hr-ticker-price-change-statistics).
#'   
#' @param type Character, type of ticker data. Available options are:
#'   - "mini": ticker data without "ask" and "bid" prices and quantities.
#'   - "full": complete ticker data with all available columns.
#'   
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`.
#'
#' @return A tibble (data frame) with 13 columns containing 24-hour ticker statistics. 
#' It includes open, high, low, close prices, volume, and more.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Get full 24-hour ticker statistics for the BTCUSDT pair from the Spot API.
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "full")
#'
#' # Get mini 24-hour ticker statistics for the BTCUSDT pair from the Spot API.
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "mini")
#'
#' # Get full 24-hour ticker statistics for the BTCUSDT pair from the Futures USD-M API.
#' binance_ticker24h(pair = "BTCUSDT", api = "fapi")
#'
#' # Get full 24-hour ticker statistics for the BTCUSD_PERP pair from the Futures Coin-M API.
#' binance_ticker24h(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Get full 24-hour ticker statistics for an options trading pair from the Options API.
#' binance_ticker24h(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @export
#'
#' @rdname binance_ticker24h
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

# Ticker24h implementation for spot api 
binance_spot_ticker24h <- function(pair, type = c("full", "mini"), quiet = FALSE) {
  
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
  
  # General Check: type default argument 
  if (missing(type) || is.null(type)) {
    type <- "full"
    if (!quiet) {
      wrn <- paste0('The type argument is missing, default is ', '"', type, '"')
      warning(wrn)
    }
  } else {
    type <- match.arg(type, choices = c("full", "mini"))
  }
  
  # multiple pairs are allowed
  if (length(pair_name) > 1) {
    mult_api_query <- purrr::map_chr(pair_name, ~paste0('"', .x, '"'))
    mult_api_query <- paste0(mult_api_query, collapse = ",")
    pair_name <- paste0('[', mult_api_query, ']')
  }
  
  # query change with multiple pairs
  api_query <- list(type = toupper(type))
  if (length(pair) > 1) {
    api_query$symbols <- pair_name
  } else {
    api_query$symbol <- pair_name
  }
  
  # api GET call 
  response <- binance_api(api = "spot", path = c("ticker", "24hr" ), query = api_query)
  
  # structure output dataset 
  if (!purrr::is_empty(response) & type == "mini") {
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
    
  } else if(!purrr::is_empty(response) & type == "full") {
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
  

  attr(response, "ip_weight") <- dplyr::case_when(
    length(pair) >= 1 & length(pair) <= 20 ~ 1,
    length(pair) > 20 & length(pair) <= 100 ~ 20,
    length(pair) > 100 ~ 40)
  attr(response, "api") <- "spot"
  return(response)
}

# Ticker24h implementation for futures USD-M api 
binance_fapi_ticker24h <- function(pair, quiet = FALSE) {
  
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

  # multiple pairs are not allowed but implemented 
  response <- list()
  for(i in 1:length(pair_name)){
    # api GET call 
    new_data <- binance_api(api = "fapi", path = c("ticker", "24hr"), query = list(symbol = pair_name[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
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
  
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "fapi"
  return(response)
}

# Ticker24h implementation for futures COIN-M api 
binance_dapi_ticker24h <- function(pair, quiet = FALSE) {
  
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
  
  # multiple pairs are not allowed but implemented 
  response <- list()
  for(i in 1:length(pair_name)){
    # api GET call 
    new_data <- binance_api(api = "dapi", path = c("ticker", "24hr"), query = list(symbol = pair_name[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
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
  
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "dapi"
  return(response)
}

# Ticker24h implementation for options api 
binance_eapi_ticker24h <- function(pair, quiet = FALSE) {
  
  # General Check: pair default argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      wrn <- paste0('The pair argument is missing with no default.')
      warning(wrn)
    }
    return(NULL)
  } else {
    pair_name <- toupper(pair)
  }
  
  # multiple pairs are not allowed but implemented 
  response <- list()
  for(i in 1:length(pair_name)){
    # api GET call 
    new_data <- binance_api(api = "eapi", path = c("ticker"), query = list(symbol = pair_name[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
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
  
  attr(response, "ip_weight") <- i * 5
  attr(response, "api") <- "eapi"
  return(response)
}