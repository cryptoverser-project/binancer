#' @docType package
#' @name binancer
#' @description Wrapper for Binance REST API in R

NULL

binance_set_environment <- function(){
  
  new_env <- new.env(parent = .GlobalEnv)
  
  # Initialize a list for each api
  new_env$spot <- list()
  new_env$fapi <- list()
  new_env$dapi <- list()
  new_env$eapi <- list()
  
  # Initialize ip_weight with weights for binance_exchange_info
  new_env$spot$ip_weight <- 20
  new_env$fapi$ip_weight <- 1
  new_env$dapi$ip_weight <- 1
  new_env$eapi$ip_weight <- 1

  # Initialize info on trading pairs 
  new_env$spot$info <- binance_exchange_info(api = "spot")
  new_env$fapi$info <- binance_exchange_info(api = "fapi")
  new_env$dapi$info <- binance_exchange_info(api = "dapi")
  new_env$eapi$info <- binance_exchange_info(api = "eapi")
  
  assign("binance", value = new_env, envir = .GlobalEnv)
  cli::cli_alert_success('"binance" environment created!')
}

binance_add_ip_weight <- function(weight = 0, api = "spot"){
  
  prev_weight <- binance[[api]][['ip_weight']]
  binance[[api]][['ip_weight']] <- prev_weight + weight
  
}

binance_reset_ip_weight <- function(api){
  
  if (missing(api) || is.null(api)){
    binance[["spot"]][['ip_weight']] <- 0
    binance[["fapi"]][['ip_weight']] <- 0
    binance[["dapi"]][['ip_weight']] <- 0
    binance[["eapi"]][['ip_weight']] <- 0 
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
    binance[[api]][['ip_weight']] <- 0
  }
}

binance_ip_weight <- function(api){
  
  if (missing(api) || is.null(api)){
    spot_weight <- binance[["spot"]][['ip_weight']]
    fapi_weight <- binance[["fapi"]][['ip_weight']]
    dapi_weight <- binance[["dapi"]][['ip_weight']]
    eapi_weight <- binance[["eapi"]][['ip_weight']]
    total_weight <- spot_weight + fapi_weight + dapi_weight + eapi_weight
  } else {
    api <- match.arg(api, choices = c("spot", "fapi", "dapi", "eapi"))
    total_weight <- binance[[api]][['ip_weight']]
  }
  return(total_weight)
}