#' Retrieve Present Open Interest Data
#'
#' Obtain the current open interest data for a specific trading pair. This function allows you to access the real-time open interest information.
#'
#' @param pair Character, specifying the trading pair of interest, e.g., "BTCUSDT".
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#open-interest).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#open-interest).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#open-interest).
#'
#' @param expiration POSIXct, specifying the expiration date for options contracts. This parameter is used when `api = "eapi"`. 
#' Default is NULL, which represents the most recent data. 
#' You can provide a POSIXct object, e.g., Sys.Date() + 1 or a specific date and time in the future.
#'
#' @return A tibble (data frame) object containing the current open interest data, including date, market, pair, and open interest values.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Example: Retrieve the current open interest data for the "BTCUSDT" trading pair in the Futures USD-M market.
#' binance_open_interest(pair = "BTCUSDT", api = "fapi")
#'
#' # Example: Retrieve the current open interest data for the "BTCUSD_PERP" trading pair in the Futures Coin-M market.
#' binance_open_interest(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Example: Retrieve the current open interest data for options contracts on "BTC" with a specific expiration date.
#' binance_open_interest(pair = "BTC", api = "eapi", expiration = Sys.Date() + 1)
#'
#' @export
#'
#' @rdname binance_open_interest
#'
#' @name binance_open_interest
#'

binance_open_interest <- function(pair, api = "fapi", expiration = Sys.Date(), quiet = FALSE){
  
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
  
  # function name 
  fun_name <- paste0("binance_", api, "_open_interest")
  
  # function arguments 
  if (api == "eapi") {
    args <- list(symbol = pair, expiration = expiration, quiet = quiet)
  } else if (api %in% c("fapi", "dapi")) {
    args <- list(pair = pair, quiet = quiet)
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

# api functions ---------------------------------------------------------------------------
binance_fapi_open_interest <- function(pair = "BTCUSDT", quiet = FALSE) {
  
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
  
  response <- NULL
  response <- binance_api(api = "fapi", path = c("openInterest"), query = list(symbol = pair_name))
  
  # Cleaning
  if(!purrr::is_empty(response)){
    
    response <- dplyr::as_tibble(dplyr::bind_rows(response))
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               market = "usd-m",
                               pair = response$symbol,
                               open_interest = as.numeric(response$openInterest))                            
  } else {
    response <- dplyr::tibble()
  }
  
  # Attributes
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "usd-m"
  return(response)
  
}

binance_dapi_open_interest <- function(pair = "BTCUSD_PERP", quiet = FALSE) {
  
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
  
  response <- NULL
  response <- binance_api(api = "dapi", path = c("openInterest"), query = list(symbol = pair_name))
  
  # Cleaning
  if(!purrr::is_empty(response)){
    
    response <- dplyr::as_tibble(dplyr::bind_rows(response))
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               market = "coin-m",
                               pair = response$symbol,
                               open_interest = as.numeric(response$openInterest))                            
  } else {
    response <- dplyr::tibble()
  }
  
  # Attributes
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "coin-m"
  return(response)
  
}

binance_eapi_open_interest <- function(symbol, expiration = Sys.Date(), quiet = FALSE){
  
  # General Check: pair default argument 
  if (missing(symbol) || is.null(symbol)) {
    pair_name <- "BTC"
    if (!quiet) {
      wrn <- paste0('The pair argument is missing, default is ', '"', pair_name, '"')
      warning(wrn)
    }
  } else {
    pair_name <- toupper(symbol)
  }
  
  # Expiration date
  expiration <- as.Date(expiration)
  
  # Create a format YY-MM-DD
  year_name <- as.character(lubridate::year(expiration))
  year_name <- strsplit(year_name, "")[[1]][3:4]
  year_name <- paste0(year_name, collapse = "")
  # Months 01, 02, ..., 10, 11, 12
  month_name <- as.character(lubridate::month(expiration))
  month_name <- ifelse(stringr::str_length(month_name) == 2, month_name, paste0("0", month_name))
  
  # Days 01, 02, ..., 10, 11, 12
  day_name <- as.character(lubridate::day(expiration))
  day_name <- ifelse(stringr::str_length(day_name) == 2, day_name, paste0("0", day_name))
  
  # New Expiration Date 
  expiration <- paste0(year_name, month_name, day_name)
  
  response <- binance_api("eapi", path = c("openInterest"), query = list(underlyingAsset = pair_name, expiration = expiration))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response, 
                              date = as.POSIXct(as.numeric(response$timestamp)/1000, origin = "1970-01-01"),
                              market = "options",
                              underlyine = pair_name)
    response <- dplyr::select(response, date, underlyine,  pair = "symbol", 
                              open_interest = "sumOpenInterest", open_interest_usd = "sumOpenInterestUsd")
                              
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "eapi"
  return(response)
  
}
