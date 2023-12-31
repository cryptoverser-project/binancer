% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_ws_live.R
\name{binance_live_depth}
\alias{binance_live_depth}
\title{Binance Live Depth}
\usage{
binance_live_depth(pair, api, quiet = FALSE)
}
\arguments{
\item{pair}{Character, trading pair, e.g. \code{"BTCUSDT"}.}

\item{api}{Character. Reference API. If it is \code{missing}, the default, will be used \code{"spot"}. Available options are:
\itemize{
\item \code{"spot"}: for \href{https://binance-docs.github.io/apidocs/spot/en/#diff-depth-stream}{spot API}.
\item \code{"fapi"}: for \href{https://binance-docs.github.io/apidocs/futures/en/#diff-book-depth-streams}{futures USD-m API}.
}}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\description{
Live depth stream for a trading pair.
}
