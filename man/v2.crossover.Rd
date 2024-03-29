% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.trace.q.R
\name{v2.crossover}
\alias{v2.crossover}
\title{[Non-exported] Crossover-tracing (3 quantiles, v2)}
\usage{
v2.crossover(qt, control)
}
\arguments{
\item{qt}{A data frame; the quantiles to be traced.}

\item{control}{The \code{control} option for \code{\link[nleqslv]{nleqslv}}.}
}
\value{
A list containing components
         \item{cmp}{
                 The data frame for \code{cmp} field.}
         \item{result}{
                 The output of \code{\link[nleqslv]{nleqslv}}.}
}
\description{
Tracing 3 quantiles by crossover-tracing, makes a data frame for \code{cmp} field
for a mean-differed 2-component vertical gradational distribution.
}
\details{
\subsection{Crossover-tracing}{
     Let cumulative distribution functions of 2 mean-differed normal distributions
     crossover at one of the quantiles (except for the median),
     and seek the mean values and standard deviations as tracing the other 2 quantiles.
 }
}
