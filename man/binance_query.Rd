% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binance_query.R
\name{binance_query}
\alias{binance_query}
\title{Binance REST API}
\usage{
binance_query(api = NULL, 
              path = NULL, 
              query = NULL, 
              method = 'GET',
              sign = FALSE,
              use_base_path = TRUE, 
              quiet = FALSE)
}
\arguments{
\item{api}{Character, reference API. Available options are:
\itemize{
\item \code{"spot"}: for \href{https://binance-docs.github.io/apidocs/spot/en/#introduction}{spot API}.
\item \code{"fapi"}: for \href{https://binance-docs.github.io/apidocs/futures/en/#introduction}{futures USD-m API}.
\item \code{"dapi"}: for \href{https://binance-docs.github.io/apidocs/delivery/en/#introduction}{futures Coin-m API}.
\item \code{"eapi"}: for \href{https://binance-docs.github.io/apidocs/voptions/en/#introduction}{options API}.
}}

\item{path}{Character vector. API path, \code{NULL} or \code{NA} elements will be excluded.}

\item{query}{Named list. Query parameters for the API call, \code{NULL} or \code{NA} elements will be excluded.}

\item{method}{\code{httr} method}

\item{sign}{Logical. Default is \code{FALSE}. \code{TRUE} if signature is required.}

\item{use_base_path}{Logical. When \code{TRUE}, the default, to \code{path} argument will be added a \code{base_bath} based on the selected API.
Available \code{base_bath} are:
\itemize{
\item \code{"spot"}: base path is \code{"api/v3"}.
\item \code{"fapi"}: base path is \code{"fapi/v1"}.
\item \code{"dapi"}: base path is \code{"dapi/v1"}.
\item \code{"eapi"}: base path is \code{"eapi/v1"}.
}}

\item{quiet}{Logical. Default is \code{FALSE}. If \code{TRUE} suppress messages and warnings.}
}
\description{
Execute an call to the Binance REST API.
}
\examples{

# Execute a call to spot API with base path
binance_query(api = "spot", 
              path = "time", 
              query = NULL, 
              use_base_path = TRUE)

# Execute a call to spot API without base path
binance_query(api = "spot", 
              path = c("api", "v3", "time"), 
              query = NULL,
              use_base_path = FALSE)

}
