#' Candlestick Plot
#'
#' Create a candlestick chart to visualize price movements over a specified time frame.
#'
#' @param data A data frame containing time-series data with columns: 'date', 'open', 'close', 'low', 'high', 'pair', and 'interval'.
#'
#' @param from Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the start date for the plot. 
#' If specified, only data from this date on wards will be included in the chart.
#'
#' @param to Character or an object of class \code{"\link[=POSIXt-class]{POSIXt}"}, the end date for the plot. 
#' If specified, only data up to this date will be included in the chart.
#'
#' @param title Character, optional title for the chart.
#'
#' @param col_body_up Character, color for candlestick bodies when the close price is higher than the open price. Default is "green".
#'
#' @param col_body_dw Character, color for candlestick bodies when the close price is lower than the open price. Default is "red".
#'
#' @param col_wick Character, color for candlestick wicks. Default is "black".
#'
#' @return A ggplot2 object representing the candlestick chart.
#'
#' @export
#'
#' @rdname candleChart
#' @name candleChart

candleChart <- function(data, from = NULL, to = NULL, title = NULL, col_body_up = "green", col_body_dw = "red", col_wick = "black"){
  
  if (missing(data) || is.null(data)) {
    cli::cli_abort("Data is NULL with no default argument.")
  } else {
    df_plot <- data
  }
  
  # get interval from data's attributes 
  interval <- ifelse(is.null(attributes(data)$interval), "", paste0(" (", attributes(data)$interval, ")"))
  
  # filter date to be greater than "from"
  if (!is.null(from)) {
    df_plot <- dplyr::filter(df_plot, date >= as.POSIXct(from))
  }
  # filter date to be lower than "to"
  if (!is.null(to)) {
    df_plot <- dplyr::filter(df_plot, date <= as.POSIXct(to))
  }
  # detect price movement
  df_plot <- dplyr::mutate(df_plot, movement = ifelse(close >= open, "up", "dw"))
  
  # distance between the candles
  alpha <- 0.8*(df_plot$date[2] - df_plot$date[1])
  
  plot_output <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = df_plot, ggplot2::aes(x = date + alpha/2, xend = date + alpha/2, y = low, yend = high), color = col_wick) +
    ggplot2::geom_rect(data = df_plot, ggplot2::aes(xmin = date, xmax = date + alpha, ymin = open, ymax = close, fill = movement), color = "black")+
    ggplot2::scale_fill_manual(values = c(up = col_body_up, dw = col_body_dw)) +
    ggplot2::scale_color_manual(values = c(up = col_body_up, dw = col_body_dw)) +
    ggplot2::labs(title = title, x = NULL, y = NULL)

  time_frame <- lubridate::as.difftime(max(df_plot$date) - min(df_plot$date))
  time_frame_unit <- attr(time_frame, "units")
  
  min_date <- min(df_plot$date)
  max_date <- max(df_plot$date)
  
  n <- nrow(df_plot) # number of observations to plot 
  x_ticks <- seq.POSIXt(as.POSIXct(min_date), as.POSIXct(max_date), length.out = round(2*log(n)) )
  
  if (time_frame_unit == "days" & time_frame > 7) {
    plot_output <- plot_output +
      ggplot2::scale_x_datetime(breaks = x_ticks, date_labels = "%b %d")
  } else if (time_frame_unit == "days" & time_frame > 1 & time_frame <= 7) {
    x_ticks <- seq.POSIXt(as.POSIXct(min_date), as.POSIXct(max_date), length.out = round(1.7*log(n)) )
    plot_output <- plot_output +
      ggplot2::scale_x_datetime(breaks = x_ticks, date_labels = "%d %b %H")
  } else {
    plot_output <- plot_output +
      ggplot2::scale_x_datetime(breaks = x_ticks, date_labels = "%H:%M")
  }
  
  attr(plot_output, "orig_data") <- data
  attr(plot_output, "plot_data") <- df_plot
  attr(plot_output, "time_frame") <- time_frame
  return(plot_output)
}
