% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggd.1.R
\name{is.h}
\alias{is.h}
\alias{\S4method{is.h}{GGD}}
\title{Judge if horizontal}
\usage{
\S4method{is.h}{GGD}(strict = FALSE)
}
\arguments{
\item{strict}{If \code{TRUE}, this function returns \code{TRUE} only if the model
                 is a horizontal gradation with 2 different normal distributions.

                 If \code{FALSE}, this function also returns \code{TRUE}
                 when the model is a normal distribution.}
}
\value{
\code{TRUE} if the object represents a kind of horizontal gradational distribution,
         otherwise \code{FALSE}.
}
\description{
Referring \code{mix.type} and \code{cmp} field, checks if the distribution is essentially
a horizontal gradational distribution.
When \code{mix.type = 4}, if it is essentially a horizontal gradational distribution,
this function returns \code{TRUE}.
Note, this function does not check \code{kind} and \code{kind.index} fields.
}
\examples{
 a <- GGD$new()
 a$is.h()    ## TRUE

 a$set.cmp( data.frame( mean = c( 0, 1 ), sd = c( 1.1, 1.2 ) ), this.mix.type = 2 )
 a$is.h()    ## TRUE

 a$set.cmp( grad = "v2" )
 a$kind      ## 2-Mean-Differed Sigma-Differed Vertical Gradational Distribution
 a$is.h()    ## FALSE

 a$set.cmp( data.frame( mean = c( 0, 0, 1, 1 ),
                        sd = c( 1.1, 1.1, 1.2, 1.2 ) ), this.mix.type = 4 )
 a$is.h()    ## TRUE -- this is a horizontal gradational distribution

 a$set.cmp( data.frame( mean = c( 0, 0 ), sd = c( 1, 1 ) ), this.mix.type = 2 )
 a$is.h( strict = FALSE )    ## TRUE  -- this is a horizontal gradational distribution
 a$is.h( strict = TRUE )     ## FALSE -- this is a normal distribution
}
