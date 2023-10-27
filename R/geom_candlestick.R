#' Candlestick plot 
#'
#' Candlestick plot for all time frames within the ggplot2 framework
#' 
#' @inheritParams ggplot2::geom_rect
#' @param method Character, method of computing candles. Can be `"candle"` or `"heikin_ashi"`. Default is `"candle"`
#' @param col_up Character, color of the candle when open price is greater than close price. 
#' @param col_dw Character, color of the candle when open price is lower than close price. 
#' @param bargap Numeric, positive number to regulate the distance between candles. 
#' Increasing the `"bargap"` reduce the distance between candles. Default is `6`. 
#'
#' @usage 
#' geom_candlechart(mapping = NULL, 
#'                  data = NULL, 
#'                  stat = "identity", 
#'                  position = "identity", 
#'                  linejoin = "mitre",..., 
#'                  na.rm = FALSE, 
#'                  show.legend = NA, 
#'                  bargap = 6, 
#'                  method = "candle",
#'                  col_up = "green", 
#'                  col_dw = "red", 
#'                  inherit.aes = TRUE)
#'                  
#'
#' @export
#'
#' @rdname geom_candlechart
#' @name geom_candlechart

geom_candlechart <- function(mapping = NULL, data = NULL, 
                             stat = "identity", position = "identity", linejoin = "mitre",
                             ..., na.rm = FALSE, show.legend = NA, bargap = 6, method = "candle",
                             col_up = "green", col_dw = "red", inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomCandle, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  bargap = bargap, 
                  method = method, 
                  col_up = col_up,
                  col_dw = col_dw,...)
  ) 
  
}



GeomCandle <- ggplot2::ggproto("GeomCandle", 
                               required_aes = c("x", "date_close", "open", "close", "low", "high"),
                      
                              default_aes = ggplot2::aes(
                                colour = "black",
                                col_up = "green",
                                col_dw = "red",
                                linewidth = .5,
                                size = 2,
                                method = "candle",
                                bargap = 6,
                                linetype = 1,
                                shape = 19,
                                fill = NA,
                                alpha = NA,
                                stroke = 1
                              ),
                      
                      # Transform the data before any drawing takes place
                      setup_data = function(data, params) {
                       
                        if(params$method == "candle"){
                          stat_candle(data, params)
                        } else if(params$method == "heikin_ashi"){
                          stat_heikin_ashi(data, params)
                        }
                        
                      },
                      
                      draw_panel = function(data, panel_params, coord, ...) {
                        
                        bar_high <- dplyr::mutate(data,
                                                  x = (xmin + xmax)/2, 
                                                  xend = (xmin + xmax)/2,
                                                  y = ifelse(fill == "green", ymax, ymin), 
                                                  yend = y_high, 
                        )
                        bar_low <- dplyr::mutate(data,
                                                 x = (xmin + xmax)/2, 
                                                 xend = (xmin + xmax)/2,
                                                 y = ifelse(fill == "green", ymin, ymax), 
                                                 yend = y_low, 
                        )
                        # Return all the components
                        grid::gList(
                          ggplot2::GeomRect$draw_panel(data, panel_params, coord, ...),
                          ggplot2::GeomSegment$draw_panel(bar_high, panel_params, coord, ...),
                          ggplot2::GeomSegment$draw_panel(bar_low, panel_params, coord, ...)
                        )
                      }
) 

stat_heikin_ashi <- function(data, params){
  
  data <- 
  dplyr::mutate(data,
                xmin = as.numeric(x), 
                xmax = as.numeric(date_close),
                dx = (xmax-xmin)/params$bargap,
                xmax = xmax - dx,
                ymax = (open + low + close + high)/4, # close price 
                y_low = low, # low price 
                y_high = high, # high price 
                ymin = open, # open price 
                fill = ifelse(ymin < ymax, params$col_up, params$col_dw)
  )
  
  for(i in 2:nrow(data)){
    data$ymin[i] <- (data$ymin[i-1] + data$ymax[i-1])/2
    data$y_low[i] <- min(c(data$ymin[i], data$ymax[i], data$low[i]))
    data$y_high[i] <- max(c(data$ymin[i], data$ymax[i], data$high[i]))
    data$fill[i] <- ifelse(data$ymin[i] < data$ymax[i], params$col_up, params$col_dw)
  }
  return(data)
}

stat_candle <- function(data, params){
  
  dplyr::mutate(data,
                xmin = as.numeric(x), 
                xmax = as.numeric(date_close),
                dx = (xmax-xmin)/params$bargap,
                xmax = xmax - dx,
                ymin = open, # open price
                ymax = close, # close price 
                y_high = high, # high price 
                y_low = low, # low price
                fill = ifelse(open < close, "green", "red")
  )
  
}


