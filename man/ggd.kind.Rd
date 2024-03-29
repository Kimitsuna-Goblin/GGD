% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.1.R
\name{ggd.kind}
\alias{ggd.kind}
\title{String of the kind}
\usage{
ggd.kind(objs)
}
\arguments{
\item{objs}{A vector or a list of elements indicating the kind of distribution model.
                 Each element must be a character string of a regular expression pattern
                 matching to an element of \code{ggd:::kinds} or an index number of
                 \code{ggd:::kinds}, or a \code{\link[ggd]{GGD}} object, or an \code{NA}.

                 The matching method follows that of \code{objs} argument of
                 \code{\link[ggd]{ggd.kind.index}}.}
}
\value{
The vector of character strings which represent the kinds of distributions,
         which are elements of \code{ggd:::kinds}.
         If an element of \code{objs} does not match any element of \code{ggd:::kinds},
         \code{NA_character_} will be returned for it.
         If \code{NULL} is given for \code{objs}, \code{NULL} will be returned.
}
\description{
Gets the strings which represent the kinds of distributions.
}
\examples{
 ggd.kind( ggd:::kinds )
 ggd.kind( 1:16 )
 ggd.kind( 6 )
 ggd.kind( "3-Mean-Differed Sigma-Differed Vertical Gradational Distribution" )

 ggd.kind( "Horizontal" )
 ggd.kind( "Mean-Eq.*Vertical" )
 ggd.kind( "3-Mean-Eq.*Vertical" )

 a <- GGD$new()
 ggd.kind( a )

 ## ggd.nls.freq.all returns a list of GGD objects to $obj.
 df <- data.frame( x = seq( -2, 2, 0.2 ),
                   freq = c(     57,    277,   1002,   3178,   9646,  22109, 42723,
                              80646, 117625, 139181, 162319, 150870, 109947, 78736,
                              46616,  21058,   9211,   3466,    976,    260,    61 ) )
 result <- ggd.nls.freq.all( df, not.use.nls = TRUE, start.level = 2 )
 ggd.kind( result$obj )
}
\seealso{
\code{\link[ggd]{ggd.kind.index}}
}
