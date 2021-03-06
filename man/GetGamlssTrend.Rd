% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gamlssQuantileTrends.R
\name{GetGamlssTrend}
\alias{GetGamlssTrend}
\title{Computes change using gamlss}
\usage{
GetGamlssTrend(formula, data, family = "BCCG", ...)
}
\arguments{
\item{formula}{Formula object}

\item{data}{Data (so far only data.table is checked)}

\item{family}{String either "BCCG" or "SN2"}

\item{...}{Other arguments passed to gamlss}
}
\value{
data.table with columns for  family, predictors, tau, and value
}
\description{
Fits a gamlss model and returns trend for the 0.05, 0.5, and 0.95 quantile
}

