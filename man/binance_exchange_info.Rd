% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_exchange_info.R
\name{binance_exchange_info}
\alias{binance_exchange_info}
\title{Binance Market Information}
\usage{
binance_exchange_info(pair = NULL, 
                      api, 
                      permissions = "all", 
                      quiet = FALSE)
}
\arguments{
\item{api}{Character. Reference API. If it is \code{missing}, the default, will be used \code{"spot"}. Available options are:
\itemize{
\item \code{"spot"}: for \href{https://binance-docs.github.io/apidocs/spot/en/#exchange-information}{spot API}.
\item \code{"fapi"}: for \href{https://binance-docs.github.io/apidocs/futures/en/#exchange-information}{futures USD-m API}.
\item \code{"dapi"}: for \href{https://binance-docs.github.io/apidocs/delivery/en/#exchange-information}{futures COIN-m API}.
\item \code{"eapi"}: for \href{https://binance-docs.github.io/apidocs/voptions/en/#exchange-information}{options API}.
}}

\item{permissions}{Character, optional. Used only if \code{api = "spot"}. Types of trading pairs to return.
If \code{"all"}, the default, all available types of pairs will be returned. Available options are:
\itemize{
\item \code{"all"}: trading pairs in all markets.
\item \code{"spot"}: trading pairs only in spot markets.
\item \code{"margin"}: trading pairs only in margin markets.
\item \code{"leveraged"}: trading pairs only in leveraged markets.
}}

\item{pair}{Character, optional. Trading pair, e.g. \code{"BTCUSDT"}. If \code{NULL}, the default, all available pairs will be returned.}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\value{
A \code{\link[=data.frame-class]{data.frame}} with trading pairs and others information.
}
\description{
Obtain market information and available trading pairs.
}
\details{
The IP weight for this API call is 20 if \code{api = "spot"} while 1 for others \code{api}, The data source is memory.
}
\examples{

# Get all pairs in all markets
binance_exchange_info(api = "spot", permissions = "all", pair = NULL)

# Get all pairs only in spot markets
binance_exchange_info(api = "spot", permissions = "spot", pair = NULL)

# Get all pairs only in margin markets
binance_exchange_info(api = "spot", permissions = "margin", pair = NULL)

# Get all pairs only in leveraged market
binance_exchange_info(api = "spot", permissions = "leveraged", pair = NULL)

# Get information only for BTCUSDT in all markets
binance_exchange_info(api = "spot", permissions = "all", pair = "BTCUSDT")

# Get information for multiple pairs in all markets
binance_exchange_info(api = "spot", 
                      permissions = "all", 
                      pair = c("BTCUSDT", "BNBUSDT"))

# Get information for multiple pairs only in margin and leveraged markets
binance_exchange_info(api = "spot", 
                      permissions = c("margin", "leveraged"), 
                      pair = c("BTCBUSD", "ETHBUSD"))
                      
# Get all pairs in futures USD-m markets
binance_exchange_info(api = "fapi")

# Get all pairs in futures COIN-m markets
binance_exchange_info(api = "dapi")

# Get all pairs in options markets
binance_exchange_info(api = "eapi")

}
