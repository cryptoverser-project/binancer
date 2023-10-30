credentials <- new.env()

#' Check Binance Api Keys
#' 
#' Check if Binance Api Keys were set previously.
#' 
#' @return No return values, but fails when credentials were not set.
#' 
#' @keywords internal
#' 
#' @noRd 

binance_check_credentials <- function() {
  
  if (is.null(credentials$secret)) {
    msg <- 'Binance API secret not set. Call binance_credentials()'
    cli::cli_abort(msg)
  }
  if (is.null(credentials$key)) {
    msg <- 'Binance API key not set. Call binance_credentials()'
    cli::cli_abort(msg)
  }
}

#' Return Binance API secret stored in the environment
#' 
#' @return Character, Binance API secret. 
#' @keywords internal
#' 
#' @rdname binance_secret
#' @name binance_secret
#' @noRd

binance_secret <- function() {
  binance_check_credentials()
  credentials$secret
}

#' Return Binance API key stored in the environment
#' 
#' @return Character, Binance API key.
#' 
#' @keywords internal
#' @noRd

binance_key <- function() {
  binance_check_credentials()
  credentials$key
}

#' Set Binance API Keys
#' 
#' Sets the API key and secret to interact with the Binance API
#' 
#' @param key Character. Api key. 
#' 
#' @param secret Character. Api secret. 
#' 
#' @return No return values, setting config in the package namespace.
#' 
#' @examples \dontrun{
#' # Add api keys 
#' binance_credentials('foo', 'bar')
#' # Remove api keys 
#' binance_credentials()
#' }
#' 
#' @export
#' 
#' @keywords apikey
#' 
#' @rdname binance_credentials
#' @name binance_credentials

binance_credentials <- function(key, secret) {
  
  check_rm_key <- missing(key) || is.null(key)
  if (check_rm_key){
    # Remove API key is key is missing or NULL
    credentials$key <- NULL
    msg <- 'Binance API key removed.'
    cli::cli_alert_danger(msg)
  } else {
    # Set API key 
    credentials$key <- key
    msg <- 'Binance API key setted. Call binance_key() to view it.'
    cli::cli_alert_success(msg)
  }
  
  check_rm_secret <- missing(secret) || is.null(secret)
  if (check_rm_secret){
    # Remove API secret is secret is missing or NULL
    credentials$secret <- NULL
    msg <- 'Binance API secret removed.'
    cli::cli_alert_danger(msg)
  } else {
    # Set API secret 
    credentials$secret <- secret
    msg <- 'Binance API secret setted. Call binance_secret() to view it.'
    cli::cli_alert_success(msg)
  }
}

#' Sign a message 
#' 
#' Sign the query string for Binance API
#' 
#' @param query Named list
#' 
#' @return string
#' 
#' @keywords internal
#' 
#' @examples \dontrun{
#' signature(list(foo = 'bar', z = 4))
#' }
#' @rdname binance_sign
#' @name binance_sign
#' @noRd

binance_sign <- function(query) {
  query$timestamp <- unix_timestamp()
  query$signature <- digest::hmac(
    algo = 'sha256',
    key = binance_secret(),
    object = paste(mapply(paste, names(query), query, sep = '=', USE.NAMES = FALSE), collapse = '&'))
  return(query)
}