#' Binance REST API Call Function
#'
#' Execute an API call to the Binance REST API, allowing access to various endpoints across different Binance API types.
#'
#' @param api Character, specifying the reference API. Available options include:
#'   - "spot": For [Spot API](https://binance-docs.github.io/apidocs/spot/en/#introduction).
#'   - "fapi": For [Futures USD-M API](https://binance-docs.github.io/apidocs/futures/en/#introduction).
#'   - "dapi": For [Futures Coin-M API](https://binance-docs.github.io/apidocs/delivery/en/#introduction).
#'   - "eapi": For [Options API](https://binance-docs.github.io/apidocs/voptions/en/#introduction).
#'
#' @param path Character vector, specifying the API path. This vector represents the structure of the API endpoint.
#'
#' @param query Named list, containing query parameters for the API call.
#'
#' @param use_base_path Logical, if `TRUE`, the appropriate base path will be automatically added to the `path` argument based on the selected API type. The base paths are as follows:
#'   - "spot": For the Spot API, the base path is "api/v3".
#'   - "fapi": For the Futures USD-M API, the base path is "fapi/v1".
#'   - "dapi": For the Futures Coin-M API, the base path is "dapi/v1".
#'   - "eapi": For the Options API, the base path is "eapi/v1".
#'
#' @param quiet Logical, suppress informational messages if TRUE.
#'
#' @examples 
#' 
#' # Example: Execute an API call to "api/v3/time" with automatic base path addition (use_base_path = TRUE) for the Spot API.
#' binance_api(api = "spot", path = c("time"), query = NULL)
#'
#' # Example: Execute an API call to "api/v3/time" without automatic base path addition (use_base_path = FALSE) for the Spot API.
#' binance_api(api = "spot", path = c("api", "v3", "time"), use_base_path = FALSE, query = NULL)
#'
#' @export
#'
#' @rdname binance_api
#'
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
  api_path <- path[!is.null(path) && !is.na(path)]
  # query: remove null/NA elements
  api_query <- query[!is.null(query) && !is.na(query)]
  # request url
  req_url <- httr::modify_url(base_url, path = api_path, query = api_query)
  # api GET call
  response <- httr::GET(req_url)
  
  # check error: status code and result
  api_status <- httr::status_code(response)
  api_empty  <- purrr::is_empty(response)
  
  # initialize output
  api_content <- NULL
  # check error: status code is not equal to 200
  if (api_status != 200) {
    if (!quiet) {
      warning("GET Request ERROR: the status code is not equal to 200.")
    }
    return(api_content)
  }
 
  # check error: the result is empty
  if (api_empty) {
    if (!quiet) {
      warning("GET Request Error: the response is empty!")
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
