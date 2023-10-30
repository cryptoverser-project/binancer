#' Binance REST API
#'
#' Execute an call to the Binance REST API.
#'
#' @param api  Character, reference API. Available options are:
#'   - `"spot"`: for [spot API](https://binance-docs.github.io/apidocs/spot/en/#introduction).
#'   - `"fapi"`: for [futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#introduction).
#'   - `"dapi"`: for [futures Coin-m API](https://binance-docs.github.io/apidocs/delivery/en/#introduction).
#'   - `"eapi"`: for [options API](https://binance-docs.github.io/apidocs/voptions/en/#introduction).
#'
#' @param path Character vector. API path, `NULL` or `NA` elements will be excluded. 
#'
#' @param query Named list. Query parameters for the API call, `NULL` or `NA` elements will be excluded. 
#'
#' @param method \code{httr} method
#'
#' @param use_base_path Logical. When `TRUE`, the default, to `path` argument will be added a `base_bath` based on the selected API. 
#' Available `base_bath` are:
#'   - `"spot"`: base path is `"api/v3"`.
#'   - `"fapi"`: base path is `"fapi/v1"`.
#'   - `"dapi"`: base path is `"dapi/v1"`.
#'   - `"eapi"`: base path is `"eapi/v1"`.
#' @param sign Logical. Default is `FALSE`. `TRUE` if signature is required. 
#' 
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @usage 
#' binance_query(api = NULL, 
#'               path = NULL, 
#'               query = NULL, 
#'               method = 'GET',
#'               sign = FALSE,
#'               use_base_path = TRUE, 
#'               quiet = FALSE)
#'
#' @export
#' 
#' @examples 
#' 
#' # Execute a call to spot API with base path
#' binance_query(api = "spot", 
#'               path = "time", 
#'               query = NULL, 
#'               use_base_path = TRUE)
#'
#' # Execute a call to spot API without base path
#' binance_query(api = "spot", 
#'               path = c("api", "v3", "time"), 
#'               query = NULL,
#'               use_base_path = FALSE)
#'
#' @rdname binance_query
#' @name binance_query

binance_query <- function(api = "spot", path = NULL, query = list(), method = 'GET', sign = FALSE, use_base_path = TRUE, quiet = FALSE) {
  
  # Available api
  api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  
  # Create the base url depending on api   
  if (api == "spot") {
    base_url <- "https://api.binance.com"
    base_path <- c("api", "v3")
  } else if (api == "fapi") {
    base_url <- "https://fapi.binance.com"
    base_path <- c("fapi", "v1")
  } else if (api == "dapi") {
    base_url <- "https://dapi.binance.com"
    base_path <- c("dapi", "v1")
  } else if (api == "eapi") {
    base_url <- "https://eapi.binance.com"
    base_path <- c("eapi", "v1")
  }
  
  # Use base path 
  if (use_base_path) {
    path <- c(base_path, path)
  } 
  
  # Api path
  if (purrr::is_empty(path)){
    path <- NULL
  } else {
    # Remove NA elements 
    path <- path[!is.na(path)]
    path <- ifelse(length(path) > 1, paste0(path, collapse = "/"), path)
  }
  
  # Api query
  if (purrr::is_empty(query)){
    query <- list()
  } else {
    # Remove NULL elements 
    non_null <- !purrr::map_lgl(query, is.null)
    query <- query[non_null]
    # Remove NA elements 
    non_na <- !purrr::map_lgl(query, is.na)
    query <- query[non_na]
  }
  
  if (isTRUE(sign)) {
    query <- binance_sign(query)
    config <- httr::add_headers('X-MBX-APIKEY' = binance_key())
  } else {
    config <- httr::config()
  }
  
  method <- match.arg(method, choices = c('GET', 'POST', 'PUT', 'DELETE'))
  response <- binance_api_query(
    base = base_url,
    path = path,
    method = method,
    query = query,
    config = config)

  # Http status code 
  api_status <- httr::status_code(response)
  # Check if response is empty  
  api_empty <- purrr::is_empty(response)
  if (api_empty) {
    if (!quiet) {
      cli::cli_alert_danger("Request is empty!")
    }
    response <- NULL
  } else {
    response <- httr::content(response, type = "text", encoding = "UTF-8")
    response <- jsonlite::fromJSON(response)
  }
  
  if (api_status != 200) {
    if (!quiet) {
      msg <- paste0("[Error code: ", response$code, "] ", response$msg)
      cli::cli_alert_danger(msg)
    }
  }
  
  return(response)
}


#' Make an API request with retries
#' 
#' @param base URL
#' @param path string
#' @param method HTTP request method
#' @param query URL parameters provided as a list
#' @param body body of the request
#' @param config \code{httr} 
#' @param retry allow retrying the query on failure
#' @param retries internal counter of previous retries
#' 
#' @return raw object returned by \code{httr}
#' 
#' @keywords internal
#' 
#' @rdname binance_api_query
#' @name binance_api_query
#' @noRd

binance_api_query <- function(base, path, method, query = list(), body = NULL, config = config(), retry = method == 'GET', retries = 0) {
  
  method <- match.arg(method, choices = c('GET', 'POST', 'PUT', 'DELETE'))
  METHOD <- utils::getFromNamespace(method, ns = 'httr')

  res <- tryCatch(
    METHOD(base, config = config, path = path, query = query, body = body),
    error = function(e) e)
  
  if (inherits(res, 'error')) {
    if (isTRUE(retry) & retries < 4) {
      mc <- match.call()
      mc$retries <- retries + 1
      msg <- paste0('Query to ', base, "/", path, 'failed. Trials: ', mc$retries)
      cli::cli_alert_danger(msg)
      eval(mc, envir = parent.frame())
    }
  }
  return(res)
}
