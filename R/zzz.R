binance_add_weight_ip <- function(weight = 0, api = "spot"){
  
  old_weight <- binance_env[['weight_ip']][[api]]
  binance_env[['weight_ip']][[api]] <- old_weight + weight
  
}

binance_weight_ip <- function(api = NULL){
  
  if (is.null(api)){
    return(binance_env[['weight_ip']][["total"]])
  } else {
    return(binance_env[['weight_ip']][[api]])
  }
}




