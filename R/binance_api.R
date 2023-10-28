#' Binance REST API
#'
#' Execute an GET call to the Binance REST API.
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
#' @param use_base_path Logical. When `TRUE`, the default, to `path` argument will be added a `base_bath` based on the selected API. 
#' Available `base_bath` are:
#'   - `"spot"`: base path is `"api/v3"`.
#'   - `"fapi"`: base path is `"fapi/v1"`.
#'   - `"dapi"`: base path is `"dapi/v1"`.
#'   - `"eapi"`: base path is `"eapi/v1"`.
#'
#' @param quiet Logical. Default is `FALSE`. If `TRUE` suppress messages and warnings. 
#' 
#' @usage 
#' binance_api(api = NULL, 
#'             path = NULL, 
#'             query = NULL, 
#'             use_base_path = TRUE, 
#'             quiet = FALSE)
#'
#' @export
#' 
#' @examples 
#' 
#' # Execute a call to spot API with base path
#' binance_api(api = "spot", 
#'             path = "time", 
#'             query = NULL, 
#'             use_base_path = TRUE)
#'
#' # Execute a call to spot API without base path
#' binance_api(api = "spot", 
#'             path = c("api", "v3", "time"), 
#'             query = NULL,
#'             use_base_path = FALSE)
#'
#' @rdname binance_api
#' @name binance_api

binance_api <- function(api = NULL, path = NULL, query = NULL, use_base_path = TRUE, quiet = FALSE){
  
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
    api_path <- NULL
  } else {
    # Remove NA elements 
    api_path <- path[!is.na(path)]
  }
  
  # Api query
  if (purrr::is_empty(query)){
    api_query <- list()
  } else {
    # Remove NULL elements 
    non_null <- !purrr::map_lgl(query, is.null)
    api_query <- query[non_null]
    # Remove NA elements 
    non_na <- !purrr::map_lgl(api_query, is.na)
    api_query <- api_query[non_na]
  }
  
  # Api url
  api_url <- httr::modify_url(base_url, path = api_path, query = api_query)
  # GET call
  response <- httr::GET(api_url)

  # Check http status code 
  api_status <- httr::status_code(response)
  if (api_status != 200) {
    if (!quiet) {
      cli::cli_alert_danger("GET Request ERROR: status code is not equal to 200.")
    }
  }
 
  # Check if response is empty  
  api_empty <- purrr::is_empty(response)
  if (api_empty) {
    if (!quiet) {
      cli::cli_alert_danger("GET Request Error: response is empty!")
    }
  }
  
  # Output 
  if (api_status == 200 && !api_empty) {
    api_content <- httr::content(response, type = "text", encoding = "UTF-8")
    api_content <- jsonlite::fromJSON(api_content)
  } else {
    api_content <- NULL
  }
  
  return(api_content)
}
