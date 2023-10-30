.onLoad <- function(libname, pkgname){
  #binance_set_environment()
}

#' Return current UNIX timestamp in millisecond
#' 
#' @param as_character Logical. If `TRUE`, the default, the timestamp will be returned as Character. Otherwise as numeric. 
#' 
#' @example
#' 
#' unix_timestamp()
#' 
#' @return Time in milliseconds since Jan 1, 1970
#' 
#' @keywords internal
#' @noRd

unix_timestamp <- function(as_character = TRUE) {
  
  unix_time <- round(as.numeric(Sys.time())*1e3)
  
  if (isTRUE(as_character)) {
    unix_time <- format(unix_time, scientific = FALSE)
  } 
  
  return(unix_time)
}


#' Convert a Posixct date into a UNIX timestamp in millisecond
#' 
#' @param x Posixct
#' 
#' @param as_character Logical. If `TRUE`, the default, the timestamp will be returned as Character. Otherwise as numeric. 
#' 
#' @examples 
#' 
#' # Convert a POSIXct date into a UNIX timestamp 
#' unix_timestamp(as.POSIXct("2023-02-01 00:00:00"))
#' 
#' @return milliseconds since Jan 1, 1970
#' 
#' @keywords internal
#' @rdname as_unix_time
#' @name as_unix_time
#' @noRd

as_unix_time <- function(x, as_character = TRUE) {
  
  assertive::assert_is_posixct(x)
  
  unix_time <- round(as.numeric(x)*1e3)
  
  if (isTRUE(as_character)) {
    unix_time <- format(unix_time, scientific = FALSE)
  } 
  
  return(unix_time)
}

#' Binance filters 
#' 
#' Return binance filters applied to spot trading 
#' 
#' @param pair Character. Trading pair, eg `"BTCUSDT"`.
#' 
#' @example 
#
#' binance_filters("BTCUSDT")
#' 
#' @return List 
#' 
#' @keywords internal
#' 
#' @noRd

binance_filters <- function(pair){
  
  # Check "pair" argument 
  if (missing(pair) || is.null(pair)) {
    if (!quiet) {
      msg <- paste0('The "pair" argument is missing with no default argument.')
      cli::cli_abort(msg)
    }
  } else {
    pair <- toupper(pair)
  }
  
  df_pair <- binance_exchange_info(api = "spot", permissions = "all", pair = pair)
  df_filters <- dplyr::as_tibble(df_pair$filters[[1]])
  
  filters <- list()
  for(i in 1:nrow(df_filters)){
    filters[[i]] <- df_filters[i,2:ncol(df_filters)]
    filters[[i]] <- purrr::map_df(filters[[i]], ~ifelse(is.na(.x), 0, as.numeric(.x)))
  }
  names(filters) <- df_filters$filterType
  return(filters)
}

