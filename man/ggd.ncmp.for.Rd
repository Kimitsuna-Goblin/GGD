% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.1.R
\name{ggd.ncmp.for}
\alias{ggd.ncmp.for}
\title{Number of components according to grad}
\usage{
ggd.ncmp.for(
  grad = c("default", "normal", "h", "v", "v2", "v3", "hv"),
  mix.type = 2
)
}
\arguments{
\item{grad}{A character string indicating the method of gradation.

                     \code{"h"} for horizontal, \code{"v"} for vertical,
                     and \code{"hv"} for horizontal-vertical.
                     The number after \code{"v"} is the number of components.
                     Numberless \code{"v"} is an alias for \code{"v2"}.

                     \code{"normal"} is for a normal distribution.
                     \code{"default"} is for depending on \code{mix.type}.}

\item{mix.type}{A numeric value for \code{mix.type} field of \code{GGD} object.
If the length is longer than 1, only the first element is valid.}
}
\value{
Appropriate number of components.
         If \code{grad} is \code{"default"} and \code{mix.type} is invalid,
         \code{NA} will be returned.
}
\description{
Gets the number of components for \code{cmp} field of \code{GGD} object
which is appropriate to given \code{grad} or \code{mix.type} argument.
}
\details{
This function does not have \code{kind} argument.
So if you want to apply to \code{kind},
use \code{\link[ggd]{ggd.mix.type.for}} to get appropriate \code{mix.type} value.
}
\examples{
 ggd.ncmp.for( grad = "normal" )                 ## 1
 ggd.ncmp.for( grad = "h" )                      ## 2
 ggd.ncmp.for( grad = "v3" )                     ## 3
 ggd.ncmp.for( grad = "default", mix.type = 4 )  ## 4
 ggd.ncmp.for( mix.type = NA )                   ## 0
 ggd.ncmp.for( mix.type = -1 )                   ## NA (invalid mix.type)
}
\seealso{
\code{\link[ggd]{ggd.mix.type.for}}
}
