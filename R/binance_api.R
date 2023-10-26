#' Binance REST API
#'
#' Execute an GET call to the Binance REST API.
#'
#' @param api  Character, reference API. Available options are:
#'   - `"spot"`: for [Spot API](https://binance-docs.github.io/apidocs/spot/en/#introduction).
#'   - `"fapi"`: for [Futures USD-m API](https://binance-docs.github.io/apidocs/futures/en/#introduction).
#'   - `"dapi"`: for [Futures Coin-m API](https://binance-docs.github.io/apidocs/delivery/en/#introduction).
#'   - `"eapi"`: for [Options API](https://binance-docs.github.io/apidocs/voptions/en/#introduction).
#'
#' @param path Character vector, API path. `NULL` or `NA` elements will be excluded. 
#'
#' @param query Named list, query parameters for the API call. `NULL` or `NA` elements will be excluded. 
#'
#' @param use_base_path Logical, Default is `TRUE` and to `path` will be added a base bath on the selected API. Base paths are:
#'   - `"spot"`: base path is `"api/v3"`;
#'   - `"fapi"`: base path is `"fapi/v1"`;
#'   - `"dapi"`: base path is `"dapi/v1"`;
#'   - `"eapi"`: base path is `"eapi/v1"`.
#'
#' @param quiet Logical, suppress informational messages if `TRUE`. Default is `FALSE`.
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
#'             path = c("time"), 
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
  
  # arguments with the type of api
  api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
  
  # modify the url depending on the api  
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
  
  if (use_base_path) {
    path <- c(base_path, path)
  } 
  
  # path: remove null/NA elements
  if (purrr::is_empty(path)){
    api_path <- NULL
  } else {
    api_path <- path[!is.na(path)]
  }
  # query: remove null/NA elements
  api_query <- query[!is.null(query) && !is.na(query)]
  # request url
  api_url <- httr::modify_url(base_url, path = api_path, query = api_query)
  # api GET call
  response <- httr::GET(api_url)
  
  # check error: status code and result
  api_status <- httr::status_code(response)
  api_empty  <- purrr::is_empty(response)
  
  # initialize output
  api_content <- NULL
  # check error: status code is not equal to 200
  if (api_status != 200) {
    if (!quiet) {
      cli::cli_alert_danger("GET Request ERROR: status code is not equal to 200.")
    }
    return(api_content)
  }
 
  # check error: the result is empty
  if (api_empty) {
    if (!quiet) {
      cli::cli_alert_danger("GET Request Error: response is empty!")
    }
    return(api_content)
  }
  
  # return api content
  if (api_status == 200 && !api_empty) {
    api_content <- httr::content(response, type = "text", encoding = "UTF-8")
    api_content <- jsonlite::fromJSON(api_content)
  }
  return(api_content)
}
