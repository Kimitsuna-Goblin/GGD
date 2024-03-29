% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.trace.q.R
\name{v2.qt4.cmp}
\alias{v2.qt4.cmp}
\title{[Non-exported] Gets cmp field (4 quantiles, v2)}
\usage{
v2.qt4.cmp(qt, control, retryable)
}
\arguments{
\item{qt}{A data frame; the quantiles to be traced.}

\item{control}{The \code{control} option for \code{\link[nleqslv]{nleqslv}}.}

\item{retryable}{A logical; the flag if retrying is enable.}
}
\value{
A list containing components
         \item{cmp}{
                 The data frame for \code{cmp} field.}
         \item{result}{
                 The output of \code{\link[nleqslv]{nleqslv}}.}
}
\description{
Tracing 4 quantiles with 2 normal distributions, makes a data frame for \code{cmp} field
where \code{mix.type = 3}.
}
