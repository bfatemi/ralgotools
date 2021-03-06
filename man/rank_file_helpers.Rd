% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rank_file_helpers.R
\name{rank_file_helpers}
\alias{rank_file_helpers}
\alias{getRankfileCSV}
\alias{rf_industry}
\alias{rf_tickers}
\title{Retrieve Data Resources from RankFile}
\usage{
getRankfileCSV(fname = NULL)

rf_industry()

rf_tickers(refresh = FALSE)
}
\arguments{
\item{fname}{The name of the file to retrieve and parse from rankandfile}

\item{refresh}{A boolean. If TRUE (default is FALSE) then data stored locally will be refreshed.}
}
\value{
Data.table with data resources
}
\description{
Functions that return data resources such as all companies on an exchance, all ETFs, SIC codes, etc.
}
\section{Functions}{
\itemize{
\item \code{getRankfileCSV}: A function that retrieves serialized data from \url{www.rankandfiled.com}

\item \code{rf_industry}: A function to pull SEC listed idenfier information for all companies

\item \code{rf_tickers}: A function to pull SEC listed idenfier information for all companies
}}

\section{Citation}{

The data returned is from \url{http://rankandfiled.com/}
}

