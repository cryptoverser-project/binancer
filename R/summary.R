summary.depth <- function(data, depth = 0.02) {
  
  output <- data[1,1:3]
  
  ask_data <- dplyr::filter(data, side == "ASK") # sellers
  bid_data <- dplyr::filter(data, side == "BID") # buyers
  
  output$ask <- min(ask_data$price) # best ask
  output$bid <- max(bid_data$price) # best bid 
  output$spread <- output$ask - output$bid # spread 
  ask_depth <- dplyr::filter(ask_data, price <= output$ask*(1+depth)) # +depth%
  bid_depth <- dplyr::filter(bid_data, price <= output$bid*(1-depth)) # -depth%
  output$ask_depth <- sum(ask_depth$quantity*ask_depth$price) # quantity*price at +depth%
  output$bid_depth <- sum(bid_depth$quantity*bid_depth$price) # quantity*price at -depth%
  output$ask_perc <- output$ask_depth/sum(ask_data$quantity*ask_data$price) 
  output$bid_perc <- output$bid_depth/sum(bid_data$quantity*bid_data$price) 
  return(output)
}

