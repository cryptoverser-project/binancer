#' Binance Market Information
#'
#' Obtain market information and available trading pairs.
#'
#' @param pair Character, optional trading pair, e.g. `"BTCUSDT"`. 
#' Default is `NULL` and all available pairs will be retrieved.
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - `"spot"`: for [Spot API](https://binance-docs.github.io/apidocs/spot/en/#exchange-information);
#'   - `"fapi"`: for [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#exchange-information);
#'   - `"dapi"`: for [Futures COIN-m API](https://binance-docs.github.io/apidocs/delivery/en/#exchange-information);
#'   - `"eapi"`: for [Options API](https://binance-docs.github.io/apidocs/voptions/en/#exchange-information).
#'
#' @param permissions Character or NULL, specifying the types of trading pairs to retrieve (optional). 
#' Available options include:
#'   - `"all"`: get available trading pairs in all markets;
#'   - `"spot"`: get available trading pairs in spot markets;
#'   - `"margin"`: get available trading pairs in margin markets;
#'   - `"leveraged"`: get available trading pairs in leveraged markets.
#'
#' @return A tibble containing market information, including trading pairs, symbols.
#'
#' @details The IP weight for this API call is 1, and the data source is memory.
#' 
#' @usage 
#' binance_exchange_info(pair = NULL, 
#'                       api = "spot", 
#'                       permissions = "all", 
#'                       quiet = FALSE)
#'
#' @examples
#'
#' # Get all pairs in all markets
#' binance_exchange_info(pair = NULL, api = "spot", permissions = "all")
#'
#' # Get all pairs only in spot markets
#' binance_exchange_info(pair = NULL, api = "spot", permissions = "spot")
#'
#' # Get all pairs only in margin markets
#' binance_exchange_info(pair = NULL, api = "spot", permissions = "margin")
#'
#' # Get all pairs only in leveraged market
#' binance_exchange_info(pair = NULL, api = "spot", permissions = "leveraged")
#'
#' # Get information only for BTCUSDT in all markets
#' binance_exchange_info(pair = "BTCUSDT", api = "spot", permissions = "all")
#'
#' # Get information for multiple pairs in all markets
#' binance_exchange_info(pair = c("BTCUSDT", "BNBUSDT"), 
#'                       api = "spot", 
#'                       permissions = "all")
#'
#' # Get information for multiple pairs only in margin and leveraged markets
#' binance_exchange_info(pair = c("BTCBUSD", "ETHBUSD"), 
#'                       api = "spot", 
#'                       permissions = c("margin", "leveraged"))
#'                       
#' # Get all pairs in futures USD-m markets
#' binance_exchange_info(pair = NULL, api = "fapi")
#'
#' # Get all pairs in futures COIN-m markets
#' binance_exchange_info(pair = NULL, api = "dapi")
#'
#' # Get all pairs in options markets
#' binance_exchange_info(pair = NULL, api = "eapi")
#'
#' @export
#'
#' @rdname binance_exchange_info
#' @name binance_exchange_info

binance_exchange_info <- function(pair = NULL, api = "spot", permissions = "all", quiet = FALSE){
  
  args <- list(pair = pair)
  if (api == "spot") {
    args$permissions <- permissions
  } 
  
  # Function name 
  fun_name <- paste0("binance_", api, "_exchange_info")
  # Safe call to avoid errors 
  safe_fun <- purrr::safely(~do.call(fun_name, args = args))
  # GET call 
  response <- safe_fun()
  
  if (!quiet & !is.null(response$error)) {
    cli::cli_alert_warning(response$error)
  } else {
    return(response$result) 
  }
}

# exchangeInfo implementation for spot API
binance_spot_exchange_info <- function(pair = NULL, permissions = "all"){
  
  # multiple pairs in one query are allowed
  if (length(pair) > 1) {
    mult_pair_name <- toupper(pair)
    # Query for Multiple Pairs
    mult_pair_query <- purrr::map_chr(mult_pair_name, ~paste0('"', .x, '"'))
    mult_pair_query <- paste0(mult_pair_query, collapse = ",")
    pair_name <- paste0('[', mult_pair_query, ']')
  } else {
    mult_pair_name <- NULL
    mult_pair_query <- NULL
  }
  
  # Multiple Permissions are allowed, however if a submission is inserted
  # the result must be returned for all the pairs
  if (!is.null(permissions) & sum(tolower(permissions) %in% "all") == 0) {
    
    pair_name <- NULL
    permissions <- tolower(permissions)
    permissions <- match.arg(permissions, choices = c("spot", "margin", "leveraged"), several.ok = TRUE)
    permissions <- toupper(permissions)
    
    if (length(permissions) > 1) {
      permissions <- purrr::map_chr(permissions, ~paste0('"', .x, '"' ))
      permissions <- paste0('[', paste0(permissions, collapse = ","), ']')
    }
  } else {
    permissions <- NULL
  }
  
  # Query parameter "symbol" change for multiple "symbols"
  if (length(mult_pair_name) > 1 & is.null(permissions)) {
    api_query <- list(symbols = pair_name, permissions = permissions)
  } else {
    api_query <- list(symbol = NULL, permissions = permissions)
  }
  
  # GET call
  response <- binance_api(api = "spot", path = c("exchangeInfo"), query = api_query)
  
  if (!purrr::is_empty(response)) {
    
    response <- dplyr::as_tibble(response$symbols)
    response <- dplyr::bind_cols(market = "spot", response)
    # Filter only if a pair is specified (pair is not NULL)
    if (length(mult_pair_name) > 1) {
      response <- dplyr::filter(response, symbol %in% mult_pair_name)
    }
  } 
  attr(response, "api") <- "spot"
  attr(response, "ip_weight") <- 1
  
  return(response)
}

# exchangeInfo implementation for futures USD-M api
binance_fapi_exchange_info <- function(pair = NULL){
  
  # GET call
  response <- binance_api(api = "fapi", path = "exchangeInfo", query = NULL)
  
  if (!purrr::is_empty(response)) {
    
    response <- dplyr::as_tibble(response$symbols)
    response <- dplyr::bind_cols(market = "usd-m", response)
    # Filter only if a pair is specified (pair is not NULL)
    pair_name <- toupper(pair)
    if(length(pair_name) > 1){
      response <- dplyr::filter(response, symbol %in% pair_name)
    }
  }
  
  attr(response, "api") <- "fapi"
  attr(response, "ip_weight") <- 1
  
  return(response)
}

# exchangeInfo implementation for futures COIN-M api
binance_dapi_exchange_info <- function(pair = NULL){
  
  # GET call
  response <- binance_api(api = "dapi", path = "exchangeInfo", query = NULL)
  
  if (!purrr::is_empty(response)) {
    response <- dplyr::as_tibble(response$symbols)
    response <- dplyr::bind_cols(market = "coin-m", response)
    # Filter only if a pair is specified (pair is not NULL)
    pair_name <- toupper(pair)
    if(length(pair_name) > 1){
      response <- dplyr::filter(response, symbol %in% pair_name)
    }
  }
  
  attr(response, "api") <- "dapi"
  attr(response, "ip_weight") <- 1
  
  return(response)
}

# exchangeInfo implementation for options api
binance_eapi_exchange_info <- function(pair = NULL){
  
  # GET call
  response <- binance_api(api = "eapi", path = "exchangeInfo", query = NULL)
  
  if (!purrr::is_empty(response)) {
 
    response <- dplyr::as_tibble(response$optionSymbols)
    response <- dplyr::bind_cols(market = "options", dplyr::select(response, -filters, -contractId))
    response <- dplyr::mutate(response, expiryDate = as.POSIXct(expiryDate/1000, origin = "1970-01-01"))
    response <- dplyr::arrange(response, expiryDate)
    # Filter only if a pair is specified (pair is not NULL)
    pair_name <- toupper(pair)
    if(length(pair_name) >= 1){
      response <- dplyr::filter(response, underlying %in% pair_name)
    }
  }
  
  attr(response, "api") <- "eapi"
  attr(response, "ip_weight") <- 1
  
  return(response)
}


