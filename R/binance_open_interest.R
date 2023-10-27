#' Current Open Interest Data
#'
#' Get the current open interest data for a specific trading pair.
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#'
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"fapi"`. Available options are:
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#open-interest).
#'   - `"dapi"`: for [futures Coin-m API](https://binance-docs.github.io/apidocs/delivery/en/#open-interest).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#open-interest).
#'
#' @param expiration Character or \code{\link[=POSIXt-class]{POSIXt}} object. Used only if `api = "eapi"`. 
#' Expiration date for options contracts. If it is `missing`, the default, will be used \code{\link[=Sys.time]{Sys.time()}}.  
#' 
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @return A \code{\link[=data.frame-class]{data.frame}} with 4 columns:
#'   - `date`: \code{\link[=POSIXt-class]{POSIXt}}, observation date.
#'   - `market`: Character, API.
#'   - `pair`: Character, trading pair.
#'   - `open_interest`: Numeric, open interest in base currency.
#'  
#' @details The IP weight for this API call is 1, and the data source is memory.
#'
#' @examples
#'
#' # Get the open interest data for BTCUSDT
#' binance_open_interest(pair = "BTCUSDT", api = "fapi")
#' 
#' # Get the open interest data for BTCUSD_PERP
#' binance_open_interest(pair = "BTCUSD_PERP", api = "dapi")
#' 
#' # Get the open interest data for options on BTC
#' binance_open_interest(pair = "BTC", api = "eapi", expiration = Sys.Date() + 1)
#'
#' @export
#'
#' @rdname binance_open_interest
#' @name binance_open_interest

binance_open_interest <- function(pair, api, expiration, quiet = FALSE){
  
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
      wrn <- paste0('The `api` argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    api <- match.arg(api, choices = c("fapi", "dapi", "eapi"))
  }
  
  # Check "expiration" argument 
  if (missing(expiration) || is.null(expiration)) {
    expiration <- Sys.Date()
    if (!quiet) {
      wrn <- paste0('The `expiration` argument is missing, default is ', '"', expiration, '"')
      cli::cli_alert_warning(wrn)
    }
  } else {
    expiration <- as.Date(expiration)
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
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_danger(response$error)
  } else {
    return(response$result)
  }
}

# openInterest implementation for USD-m api 
binance_fapi_open_interest <- function(pair, quiet = FALSE) {
  
  response <- binance_api(api = "fapi", path = "openInterest", query = list(symbol = pair))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(dplyr::bind_rows(response))
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               market = "fapi",
                               pair = response$symbol,
                               open_interest = as.numeric(response$openInterest))                            
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "fapi"
  attr(response, "endpoint") <- "openInterest"
  
  return(response)
}

# openInterest implementation for COIN-m api 
binance_dapi_open_interest <- function(pair, quiet = FALSE) {
  
  response <- binance_api(api = "dapi", path = "openInterest", query = list(symbol = pair))
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(dplyr::bind_rows(response))
    response <- tibble::tibble(date = as.POSIXct(as.numeric(response$time)/1000, origin = "1970-01-01"),
                               market = "dapi",
                               pair = response$symbol,
                               open_interest = as.numeric(response$openInterest))                            
  } else {
    response <- dplyr::tibble()
  }
  
  attr(response, "ip_weight") <- 1
  attr(response, "api") <- "dapi"
  attr(response, "endpoint") <- "openInterest"
  
  return(response)
}

# openInterest implementation for options api 
binance_eapi_open_interest <- function(symbol, expiration, quiet = FALSE){
  
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
  attr(response, "endpoint") <- "openInterest"
  
  return(response)
}
