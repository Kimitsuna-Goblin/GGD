% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.1.R
\name{p}
\alias{p}
\alias{\S4method{p}{GGD}}
\title{Cumulative distribution function}
\usage{
\S4method{p}{GGD}(x)
}
\arguments{
\item{x}{A vector of x-coordinates.}
}
\value{
A vector of the probabilities of that a value of the random variable is less than
         or equal to given x-coordinates.
}
\description{
Gets the probabilities of that a value of the random variable is less than or equal to
the given x-coordinates.
This method works like \code{\link[stats]{pnorm}} for a normal distribution.
}
\examples{
 a <- GGD$new()
 a$trace.q(
     data.frame( x = c( -0.67, 0, 0.53 ), p = c( 0.25, 0.5, 0.75 ) ),
     this.mix.type = 2 )
 a$p( c( -0.67, 0, 0.53 ) )
 plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" )
}
\seealso{
\code{\link[ggd]{d}}, \code{\link[ggd]{q}}, \code{\link[ggd]{r}}
}
