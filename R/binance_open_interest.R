#' Current Open Interest Data
#'
#' Get the current open interest data for a specific trading pair.
#'
#' @param pair Character, trading pair, e.g. "BTCUSDT".
#'
#' @param api Character, reference API. Available options are:
#'   - "fapi": For [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#open-interest).
#'   - "dapi": For [Futures Coin-m API](https://binance-docs.github.io/apidocs/delivery/en/#open-interest).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#open-interest).
#'
#' @param expiration POSIXct, specifying the expiration date for options contracts. This parameter is used when `api = "eapi"`. 
#' Default is NULL, which represents the most recent data. 
#' You can provide a POSIXct object, e.g., Sys.Date() + 1 or a specific date and time in the future.
#' 
#' @param quiet Logical, if `TRUE` suppress informational and warnings. Default is `FALSE`.
#' 
#' @return A tibble with 4 columns:
#'   - `date`: Datetime, observation date.
#'   - `market`: Character, selected API.
#'   - `pair`: Character, selected pair.
#'   - `open_interest`: Numeric, open interest in base currency.
#'  
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Get the open interest data for "BTCUSDT" in USD-M market
#' binance_open_interest(pair = "BTCUSDT", api = "fapi")
#' 
#' # Get the open interest data for "BTCUSD_PERP" in COIN-M market
#' binance_open_interest(pair = "BTCUSD_PERP", api = "dapi")
#' 
#' # Get the open interest data for options on BTC
#' binance_open_interest(pair = "BTC", api = "eapi", expiration = Sys.Date() + 1)
#'
#' @export
#'
#' @rdname binance_open_interest
#' @name binance_open_interest

binance_open_interest <- function(pair, api, expiration = Sys.Date(), quiet = FALSE){
  
  #  Check "pair" argument 
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
    api <- "fapi"
    if (!quiet) {
      wrn <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("fapi", "dapi", "eapi"))
  }
  
  # Query parameters depends on api 
  if (api == "eapi") {
    args <- list(symbol = pair, expiration = expiration, quiet = quiet)
  } else if (api %in% c("fapi", "dapi")) {
    args <- list(pair = pair, quiet = quiet)
  } 
  
  # Function name 
  fun_name <- paste0("binance_", api, "_open_interest")
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  # GET call 
  response <- safe_fun()
  
  if(!quiet & !is.null(response$error)){
    warning(response$error)
  }
  return(response$result)
}

# openInterest implementation for USD-m api 
binance_fapi_open_interest <- function(pair, quiet = FALSE) {
  
  response <- binance_api(api = "fapi", path = "openInterest", query = list(symbol = pair))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(dplyr::bind_rows(response))
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               market = "usd-m",
                               pair = response$symbol,
                               open_interest = as.numeric(response$openInterest))                            
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "usd-m"
  return(response)
}

# openInterest implementation for COIN-m api 
binance_dapi_open_interest <- function(pair, quiet = FALSE) {
  
  response <- binance_api(api = "dapi", path = "openInterest", query = list(symbol = pair))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(dplyr::bind_rows(response))
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               market = "coin-m",
                               pair = response$symbol,
                               open_interest = as.numeric(response$openInterest))                            
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "coin-m"
  return(response)
}

# openInterest implementation for options api 
binance_eapi_open_interest <- function(symbol, expiration = Sys.Date(), quiet = FALSE){
  
  # Expiration date
  expiration <- as.Date(expiration)
  # Create a format for expiration date YY-MM-DD
  year_name <- as.character(lubridate::year(expiration))
  year_name <- strsplit(year_name, "")[[1]][3:4]
  year_name <- paste0(year_name, collapse = "")
  # Months 01, 02, ..., 10, 11, 12
  month_name <- as.character(lubridate::month(expiration))
  month_name <- ifelse(stringr::str_length(month_name) == 2, month_name, paste0("0", month_name))
  # Days 01, 02, ..., 10, 11, 12
  day_name <- as.character(lubridate::day(expiration))
  day_name <- ifelse(stringr::str_length(day_name) == 2, day_name, paste0("0", day_name))
  # New expiration date 
  expiration <- paste0(year_name, month_name, day_name)
  
  response <- binance_api("eapi", path = "openInterest", query = list(underlyingAsset = symbol, expiration = expiration))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(response)
    response <- dplyr::mutate(response, 
                              date = as.POSIXct(as.numeric(response$timestamp)/1000, origin = "1970-01-01"),
                              market = "eapi",
                              underlyine = symbol, 
                              sumOpenInterest = as.numeric(sumOpenInterest),
                              sumOpenInterestUsd = as.numeric(sumOpenInterestUsd))
    response <- dplyr::select(response, date, underlyine,  pair = "symbol", 
                              open_interest = "sumOpenInterest", open_interest_usd = "sumOpenInterestUsd")
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "eapi"
  return(response)
}
