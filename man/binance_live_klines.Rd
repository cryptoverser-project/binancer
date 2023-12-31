% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_ws_live.R
\name{binance_live_klines}
\alias{binance_live_klines}
\title{Binance Live Klines}
\usage{
binance_live_klines(pair, api, interval, data = NULL, quiet = FALSE)
}
\arguments{
\item{pair}{Character. Trading pair, e.g. \code{"BTCUSDT"}.}

\item{api}{Character. Reference API. If it is \code{missing}, the default, will be used \code{"spot"}. Available options are:
\itemize{
\item \code{"spot"}: for \href{https://binance-docs.github.io/apidocs/spot/en/#kline-candlestick-streams}{spot API}.
\item \code{"fapi"}: for \href{https://binance-docs.github.io/apidocs/futures/en/#kline-candlestick-streams}{futures USD-m API}.
}}

\item{interval}{Character. Time interval for Klines data.
If it is \code{missing}, the default, will be used as start date \code{"1m"}. Available intervals are:
\itemize{
\item Secondly: \code{"1s"}, available only if \code{api = "spot"}.
\item Minutely: \code{"1m"}, \code{"3m"}, \code{"5m"}, \code{"15m"} and \code{"30m"}.
\item Hourly: \code{"1h"}, \code{"2h"}, \code{"4h"}, \code{"6h"}, \code{"8h"} and \code{"12h"}.
\item Daily: \code{"1d"} and \code{"3d"}.
\item Weekly: \code{"1w"}.
\item Monthly: \code{"1M"}.
}}

\item{data}{initial data}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\description{
Live klines stream for a trading pair.
}
