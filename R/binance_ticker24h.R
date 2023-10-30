#' Retrieve 24-Hour Ticker Statistics
#'
#' Get 24-hour ticker statistics for a specified trading pair from the selected reference API.
#'
#' @param pair Character. Trading pair, e.g. `"BTCUSDT"`.
#' 
#' @param api Character. Reference API. If it is `missing`, the default, will be used `"spot"`. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#24hr-ticker-price-change-statistics).
#'   - `"fapi"`: for [futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#24hr-ticker-price-change-statistics).
#'   - `"dapi"`: for [futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#24hr-ticker-price-change-statistics).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#24hr-ticker-price-change-statistics).
#'   
#' @param type Character. Type of ticker data. Used only if `api = "spot"`. Default is `"full"`. Available options are:
#'   - `"mini"`: data without ask and bid prices and quantities.
#'   - `"full"`: complete ticker data.
#'   
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#'
#' @return A \code{\link[=data.frame-class]{data.frame}} with 13 columns containing 24-hour ticker statistics, including:
#' open, high, low, close prices, volume, and more.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#' 
#' @usage 
#' binance_ticker24h(pair, 
#'                   api, 
#'                   type, 
#'                   quiet = FALSE)
#'
#' @examples
#'
#' # Get full 24-hour ticker for BTCUSDT
#' binance_ticker24h("BTCUSDT")
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "full")
#'
#' # Get mini 24-hour ticker for BTCUSDT
#' binance_ticker24h(pair = "BTCUSDT", api = "spot", type = "mini")
#'
#' # Get full 24-hour ticker for BTCUSDT 
#' binance_ticker24h(pair = "BTCUSDT", api = "fapi")
#'
#' # Get full 24-hour ticker for BTCUSD_PERP
#' binance_ticker24h(pair = "BTCUSD_PERP", api = "dapi")
#'
#' # Get full 24-hour ticker for or a put option on BTCUSDT.
#' # Strike of 30000 and maturity on 2024-06-28.
#' binance_ticker24h(pair = "BTC-240628-30000-P", api = "eapi")
#'
#' @export
#'
#' @rdname binance_ticker24h
#' @name binance_ticker24h

binance_ticker24h <- function(pair, api, type, quiet = FALSE){
  
  # Check `pair` argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The pair argument is missing with no default.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- toupper(pair)
  }
  
  # Check `api` argument 
  if (missing(api) || is.null(api)) {
    api <- "spot"
    if (!quiet) {
      msg <- paste0('The "api" argument is missing, default is ', '"', api, '"')
      cli::cli_alert_warning(msg)
    }
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  }
  
  # Check `type` argument 
  if (missing(type) || is.null(type)) {
    type <- "full"
    if (api == "spot"){
      if (!quiet) {
        msg <- paste0('The type argument is missing, default is ', '"', type, '"')
        cli::cli_alert_warning(msg)
      }
    }
    type <- toupper(type)
  } else {
    if (api == "spot"){
      type <- match.arg(type, choices = c("full", "mini"))
      type <- toupper(type)
    } else {
      type <- "FULL"
    }
  }
  
  # Function arguments depending on `api` 
  if (api == "spot") {
    args <- list(pair, type)
  } else {
    args <- list(pair)
  }
  
  # Function name 
  fun_name <- paste0("binance_", api, "_ticker24h")
  # Safe function to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  # GET call
  response <- safe_fun()
  # Output
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_danger(response$error)
  } else {
    return(response$result)
  }
}

# Ticker24h implementation for spot api 
binance_spot_ticker24h <- function(pair, type, quiet = FALSE) {
  
  # multiple pairs are allowed
  if (length(pair) > 1) {
    mult_api_query <- purrr::map_chr(pair, ~paste0('"', .x, '"'))
    mult_api_query <- paste0(mult_api_query, collapse = ",")
    pair_name <- paste0('[', mult_api_query, ']')
  } else {
    pair_name <- pair
  }
  
  # query change with multiple pairs
  api_query <- list(type = toupper(type))
  if (length(pair) > 1) {
    api_query$symbols <- pair_name
  } else {
    api_query$symbol <- pair_name
  }

  # GET call 
  response <- binance_query(api = "spot", path = c("ticker", "24hr" ), query = api_query)
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response$pair <- pair
    response$market <- "spot"
    response <- binance_formatter(response)
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
  
  # multiple pairs are not allowed but implemented 
  response <- list()
  for(i in 1:length(pair)){
    # api GET call 
    new_data <- binance_query(api = "fapi", path = c("ticker", "24hr"), query = list(symbol = pair[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response$pair <- pair
    response$market <- "fapi"
    response <- binance_formatter(response)
  } 
  
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "fapi"
  attr(response, "endpoint") <- "ticker24hr"
  
  return(response)
}

# Ticker24h implementation for futures COIN-M api 
binance_dapi_ticker24h <- function(pair, quiet = FALSE) {
  
  # multiple pairs are not allowed but implemented 
  response <- list()
  for(i in 1:length(pair)){
    # api GET call 
    new_data <- binance_query(api = "dapi", path = c("ticker", "24hr"), query = list(symbol = pair[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response$pair <- pair
    response$market <- "dapi"
    response <- binance_formatter(response)
  } 
  
  attr(response, "ip_weight") <- i
  attr(response, "api") <- "dapi"

  return(response)
}

# Ticker24h implementation for options api 
binance_eapi_ticker24h <- function(pair, quiet = FALSE) {
  
  # multiple pairs are not allowed but implemented 
  response <- list()
  for(i in 1:length(pair)){
    # api GET call 
    new_data <- binance_query(api = "eapi", path = "ticker", query = list(symbol = pair[i]))
    if(purrr::is_empty(new_data)){
      next
    }
    response[[i]] <- new_data
  }
  
  # structure output dataset 
  if (!purrr::is_empty(response)) {
    response <- dplyr::bind_rows(response)
    response <- dplyr::as_tibble(response)
    response$pair <- pair
    response$market <- "eapi"
    response <- binance_formatter(response)
  } 
  
  attr(response, "ip_weight") <- i*5
  attr(response, "api") <- "eapi"

  return(response)
}

